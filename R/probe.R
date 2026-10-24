# What a server answers a request with, judged on the headers.
#
# The one check recognized today is Cloudflare's, which marks every challenge
# response -- managed, JavaScript or interactive -- with the header
# `cf-mitigated: challenge`. That is Cloudflare's documented signal for it and
# is the whole test: a `403` without the header is a refusal, not a check, and
# a browser would not get past it either. Other vendors' checks can be added
# here as their markers are found; the rest of the package does not care
# which vendor it is.

#' Does a response carry a browser check instead of the resource?
#'
#' @param headers The response headers, either as the raw header block
#'   (one string, lines separated by `\r\n`) or as the named list
#'   [curl::parse_headers_list()] makes of it.
#' @return `TRUE` when the response is a browser check that a real browser
#'   would pass. Recognized today: Cloudflare's, by its `cf-mitigated:
#'   challenge` header.
#' @examples
#' is_challenge("HTTP/2 403\r\ncf-mitigated: challenge\r\n\r\n")
#' is_challenge("HTTP/2 200\r\ncontent-type: application/zip\r\n\r\n")
#' @export
is_challenge <- function(headers) {
  h <- bf_parse_headers(headers)
  identical(tolower(h[["cf-mitigated"]] %||% ""), "challenge")
}

bf_parse_headers <- function(headers) {
  if (is.list(headers)) return(headers)
  if (is.raw(headers)) headers <- rawToChar(headers)
  curl::parse_headers_list(headers)
}

#' Ask a server what it serves at a URL
#'
#' Sends a ranged `GET` for the first byte, so that the answer carries the
#' resource's real headers without the resource, and reports what came back.
#' A `HEAD` is not used because some servers answer it differently from a
#' `GET`; a one-byte range is answered like the full request.
#'
#' @param url The URL.
#' @param clearance A clearance from [clearance()], to probe as the browser
#'   that earned it; `NULL` probes as a plain client.
#' @return A list: `reachable` (was any response had), `status` (the HTTP
#'   status, `NA` when unreachable), `challenged` (see [is_challenge()]),
#'   `type` (the `content-type`, lower case), `size` (the resource's length
#'   in bytes, taken from `content-range` when the request was answered
#'   with a range, else `content-length`; `NA` when unknown) and `headers`
#'   (the parsed list).
#' @examples
#' \dontrun{
#' url <- paste0("https://www12.statcan.gc.ca/census-recensement/2021/",
#'               "geo/sip-pis/rnf-frr/files-fichiers/lrnf000r21a_e.zip")
#' probe(url)
#' }
#' @export
probe <- function(url, clearance = NULL) {
  res <- tryCatch(
    curl::curl_fetch_memory(url, handle(clearance, range = "0-0",
                                        maxfilesize = 1e6)),
    error = function(e) NULL)
  if (is.null(res)) {
    return(list(reachable = FALSE, status = NA_integer_, challenged = FALSE,
                type = NA_character_, size = NA_real_, headers = list()))
  }
  bf_describe_response(res$status_code, rawToChar(res$headers))
}

bf_describe_response <- function(status, headers) {
  h <- bf_parse_headers(headers)
  size <- suppressWarnings(as.numeric(h[["content-length"]] %||% NA))
  range <- h[["content-range"]] %||% ""
  if (grepl("/[0-9]+$", range)) {
    size <- suppressWarnings(as.numeric(sub(".*/", "", range)))
  }
  list(reachable = TRUE, status = as.integer(status),
       challenged = is_challenge(h),
       type = tolower(h[["content-type"]] %||% NA_character_),
       size = size, headers = h)
}

#' A curl handle that carries a clearance
#'
#' The browser's user-agent string and its cookies have to go on a request
#' together: Cloudflare ties the clearance cookie to the user agent that
#' earned it, and the cookie alone is answered with the check again.
#'
#' @param clearance A clearance from [clearance()], or `NULL` for a plain
#'   handle.
#' @param ... Further options for [curl::new_handle()].
#' @return A curl handle with `followlocation` set, and the clearance's
#'   `useragent` and `cookie` when one was given.
#' @examples
#' h <- handle(list(useragent = "Mozilla/5.0", cookie = "cf_clearance=x"))
#' @export
handle <- function(clearance = NULL, ...) {
  h <- curl::new_handle(followlocation = TRUE, ...)
  if (!is.null(clearance)) {
    curl::handle_setopt(h, useragent = clearance$useragent,
                        cookie = clearance$cookie)
  }
  h
}
