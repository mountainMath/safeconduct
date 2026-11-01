# What a server answers a request with.
#
# Two kinds of refusal are recognized, both Cloudflare's, and a browser gets
# past both:
#
#   * A challenge. Every challenge response -- managed, JavaScript or
#     interactive -- carries the header `cf-mitigated: challenge`, Cloudflare's
#     documented signal for it. The page runs a script, sets a `cf_clearance`
#     cookie and reloads.
#   * A block. A firewall rule that refuses clients which do not look like a
#     browser answers with a `403`, no `cf-mitigated` header and Cloudflare's
#     own block page ("Sorry, you have been blocked", error 1020 and kin). No
#     script runs and no cookie is handed out; a real browser is simply not
#     blocked. The same page is served for a rule that blocks an address, and
#     that a browser does not get past either; the browser finds out in a
#     second or two and says so.
#
# An origin's own `403` passes through Cloudflare with `server: cloudflare` on
# it too, so the block page is told by its body, which `probe()` already has:
# a one-byte range is answered with the whole page when the answer is not the
# resource. Other vendors' refusals can be added here as their markers are
# found; the rest of the package does not care which vendor it is.

#' Does a response carry a browser check instead of the resource?
#'
#' @param headers The response headers, as the raw header block (one string,
#'   lines separated by `\r\n`) or as a named list such as
#'   [curl::parse_headers_list()] makes of it.
#' @return `TRUE` for a check a real browser would pass: Cloudflare's, by its
#'   `cf-mitigated: challenge` header.
#' @seealso [is_block()] for the other refusal a browser gets past.
#' @examples
#' is_challenge("HTTP/2 403\r\ncf-mitigated: challenge\r\n\r\n")
#' is_challenge("HTTP/2 200\r\ncontent-type: application/zip\r\n\r\n")
#' @export
is_challenge <- function(headers) {
  h <- sc_parse_headers(headers)
  identical(tolower(h[["cf-mitigated"]] %||% ""), "challenge")
}

#' Is a response a block page that a browser would not be shown?
#'
#' Cloudflare's firewall can refuse a client that does not look like a
#' browser outright, with a `403` and its own block page, while a real
#' browser is served.
#'
#' @inheritParams is_challenge
#' @param status The HTTP status code.
#' @param body The response body, as a string.
#' @return `TRUE` for that page: a `403` from `server: cloudflare` whose body
#'   says "Sorry, you have been blocked".
#' @examples
#' is_block(403, "HTTP/2 403\r\nserver: cloudflare\r\n\r\n",
#'          "<html><title>Attention Required! | Cloudflare</title>...")
#' is_block(403, "HTTP/2 403\r\nserver: nginx\r\n\r\n", "Forbidden")
#' @export
is_block <- function(status, headers, body = NULL) {
  if (!identical(as.integer(status), 403L)) return(FALSE)
  h <- sc_parse_headers(headers)
  if (!identical(tolower(h[["server"]] %||% ""), "cloudflare")) return(FALSE)
  if (is.null(body) || !nzchar(body)) return(FALSE)
  grepl(sc_block_markers, body, perl = TRUE)
}

sc_block_markers <- paste(
  "cf-error-details", "Attention Required! \\| Cloudflare",
  "Sorry, you have been blocked", sep = "|")

# Does what the probe saw call for a browser?
sc_needs_browser <- function(p) isTRUE(p$challenged) || isTRUE(p$blocked)

# Headers as a named list with lower-case names, whatever they came as: the
# raw block from curl, curl's parsed list, or the object a DevTools event
# carries, whose names keep the case the server sent.
sc_parse_headers <- function(headers) {
  if (is.list(headers)) {
    names(headers) <- tolower(names(headers))
    return(headers)
  }
  if (is.raw(headers)) headers <- rawToChar(headers)
  curl::parse_headers_list(headers)
}

#' Ask a server what it serves at a URL
#'
#' Sends a `GET` for the first byte, so that the answer carries the
#' resource's headers without the resource; a `HEAD` is not used because
#' some servers answer it differently.
#'
#' @param url The URL.
#' @param clearance A clearance from [clearance()], to probe as the browser
#'   that earned it; `NULL` probes as a plain client.
#' @return A list: `reachable`, `status` (`NA` when unreachable),
#'   `challenged` ([is_challenge()]), `blocked` ([is_block()]), `type` (the
#'   `content-type`, lower case), `size` (bytes, from `content-range` or
#'   `content-length`, `NA` when unknown) and `headers` (a named list). A
#'   browser is called for when `challenged` or `blocked` is `TRUE`.
#' @examples
#' \dontrun{
#' probe("https://www12.statcan.gc.ca/census-recensement/index-eng.cfm")
#' }
#' @export
probe <- function(url, clearance = NULL) {
  res <- tryCatch(
    curl::curl_fetch_memory(url, clearance_handle(clearance, range = "0-0",
                                        maxfilesize = 1e6)),
    error = function(e) NULL)
  if (is.null(res)) {
    return(list(reachable = FALSE, status = NA_integer_, challenged = FALSE,
                blocked = FALSE, type = NA_character_, size = NA_real_,
                headers = list()))
  }
  sc_describe_response(res$status_code, rawToChar(res$headers), res$content)
}

# `body` is only looked at for a 403 page, so that the single byte of a real
# ranged resource, which may well be a NUL, is never turned into a string.
sc_describe_response <- function(status, headers, body = NULL) {
  h <- sc_parse_headers(headers)
  size <- suppressWarnings(as.numeric(h[["content-length"]] %||% NA))
  range <- h[["content-range"]] %||% ""
  if (grepl("/[0-9]+$", range)) {
    size <- suppressWarnings(as.numeric(sub(".*/", "", range)))
  }
  type <- tolower(h[["content-type"]] %||% NA_character_)
  blocked <- FALSE
  if (identical(as.integer(status), 403L) && !is.null(body)) {
    text <- if (is.raw(body)) sc_raw_text(body) else body
    blocked <- is_block(status, h, text)
  }
  list(reachable = TRUE, status = as.integer(status),
       challenged = is_challenge(h), blocked = blocked,
       type = type, size = size, headers = h)
}

# A raw body as text, with any embedded NULs dropped rather than fatal.
sc_raw_text <- function(x) {
  if (length(x) == 0L) return("")
  rawToChar(x[x != as.raw(0L)])
}

#' A curl handle that carries a clearance
#'
#' The user-agent string and the cookies go on a request together: Cloudflare
#' ties the clearance cookie to the user agent that earned it, and the cookie
#' alone is answered with the check again.
#'
#' @param clearance A clearance from [clearance()], or `NULL` for a plain
#'   handle.
#' @param ... Further options for [curl::new_handle()].
#' @return A curl handle with `followlocation` set, and the clearance's
#'   `useragent` and `cookie` when one was given.
#' @examples
#' h <- clearance_handle(list(useragent = "Mozilla/5.0", cookie = "cf_clearance=x"))
#' @export
clearance_handle <- function(clearance = NULL, ...) {
  h <- curl::new_handle(followlocation = TRUE, ...)
  if (!is.null(clearance)) {
    curl::handle_setopt(h, useragent = clearance$useragent,
                        cookie = clearance$cookie)
  }
  h
}
