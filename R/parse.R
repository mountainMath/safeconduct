# Parsing what `read_page()` returns.
#
# `read_page()` hands back the response, raw body and all, and leaves the
# parsing to these, in the manner of httr2's `resp_body_*()`: the status and
# headers stay with the caller, and the return type does not depend on an
# argument. Both routes deliver the server's own bytes -- curl as received,
# the browser as base64 of `arrayBuffer()` -- so the charset in `type` can be
# trusted either way.

#' Parse a response from read_page()
#'
#' `page_text()` decodes the body to a UTF-8 string, `page_json()` parses it
#' as JSON with [jsonlite::fromJSON()], and `page_html()` parses it as HTML
#' with [xml2::read_html()].
#'
#' The charset is taken from the response's `content-type`. Without one,
#' `page_text()` and `page_json()` assume UTF-8 (JSON is UTF-8 by
#' definition), while `page_html()` leaves it to the page's own
#' `<meta charset>`, falling back to UTF-8 only when there is none. ISO-8859-1
#' is read as windows-1252, as browsers do. A body that is not valid in its
#' charset is an error, not silently patched.
#'
#' A status check catches an error page that is sent as one. Some servers
#' redirect a missing page to a "not found" page served with status 200
#' instead; for a JSON request, `page_json()` catches that by refusing an HTML
#' body, while for a page, compare `resp$url` with the URL asked for.
#'
#' @param resp A response from [read_page()], or a URL, which is then read
#'   with [read_page()]'s defaults: `page_html(url)` stands in for
#'   `rvest::read_html(url)`. To read through a [browser_session()], call
#'   [read_page()] yourself and pass its response.
#' @param encoding The body's charset, overriding the one in the response's
#'   `content-type`.
#' @param check Whether an HTTP status outside 200--299 is an error.
#' @param ... Passed on to [jsonlite::fromJSON()].
#' @return `page_text()`: a character string, in UTF-8. `page_json()`: what
#'   [jsonlite::fromJSON()] returns. `page_html()`: an `xml_document`, whose
#'   relative links resolve against `resp$url`, the URL after any redirects.
#'
#'   Errors are classed under `safeconduct_error`: `safeconduct_http_error`
#'   for a status outside 200--299, carrying `status` and `url`, and
#'   `safeconduct_parse_error` for a body that cannot be decoded, or HTML
#'   where JSON was asked for.
#' @examples
#' \dontrun{
#' r <- read_page("https://results.vancouver.ca/data/arearesults.json")
#' page_json(r)
#'
#' r <- read_page("https://www12.statcan.gc.ca/census-recensement/index-fra.cfm")
#' xml2::xml_text(xml2::xml_find_first(page_html(r), "//title"))
#'
#' # In one step, with read_page()'s defaults.
#' page_html("https://www12.statcan.gc.ca/census-recensement/index-fra.cfm")
#' }
#' @export
page_text <- function(resp, encoding = NULL, check = TRUE) {
  resp <- bf_as_response(resp)
  if (isTRUE(check)) bf_check_status(resp)
  x <- resp$content
  enc <- encoding %||% bf_charset(resp$type) %||% "UTF-8"
  # A byte-order mark overrides the declared charset, as it does in a browser.
  if (bf_has_bom(x)) {
    x <- x[-(1:3)]
    enc <- "UTF-8"
  }
  bf_decode(x, enc, resp$url)
}

#' @rdname page_text
#' @export
page_json <- function(resp, ..., check = TRUE) {
  resp <- bf_as_response(resp)
  if (isTRUE(check)) bf_check_status(resp)
  txt <- page_text(resp, check = FALSE)
  type <- resp$type %||% NA_character_
  if (!grepl("json", type) && grepl("^\\s*<", txt)) {
    stop(bf_error("safeconduct_parse_error", paste0(
      "'", resp$url, "' answered with ",
      if (is.na(type)) "HTML" else paste0("a page (", type, ")"),
      ", not JSON."), url = resp$url))
  }
  jsonlite::fromJSON(txt, ...)
}

#' @rdname page_text
#' @export
page_html <- function(resp, encoding = NULL, check = TRUE) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("page_html() needs the xml2 package: install.packages(\"xml2\")",
         call. = FALSE)
  }
  resp <- bf_as_response(resp)
  if (isTRUE(check)) bf_check_status(resp)
  # The raw bytes, so that libxml2 can read a `<meta charset>` when the header
  # names none; decoding first with a UTF-8 fallback would garble such a page.
  enc <- encoding %||% bf_charset(resp$type)
  enc <- if (is.null(enc)) "" else bf_charset_name(enc)
  xml2::read_html(resp$content, encoding = enc, base_url = resp$url)
}

# A URL is read; a response is passed through; anything else is refused here,
# rather than failing later on `$` with a message that names neither.
bf_as_response <- function(resp) {
  if (is.character(resp) && length(resp) == 1L && !is.na(resp)) {
    return(read_page(resp))
  }
  if (!is.list(resp) || !is.raw(resp$content)) {
    stop("`resp` must be a response from read_page(), or a URL.", call. = FALSE)
  }
  resp
}

# A status outside 200--299 is an error. A status of 0 is no HTTP status at
# all (a `file://` URL), and passes.
bf_check_status <- function(resp) {
  status <- as.integer(resp$status_code %||% NA_integer_)
  if (is.na(status) || status == 0L || (status >= 200L && status < 300L)) {
    return(invisible(resp))
  }
  stop(bf_error("safeconduct_http_error", paste0(
    "'", resp$url, "' answered with HTTP status ", status, "."),
    status = status, url = resp$url))
}

bf_charset <- function(type) {
  if (is.null(type) || is.na(type)) return(NULL)
  m <- regmatches(type, regexec("charset\\s*=\\s*\"?([^\";[:space:]]+)", type,
                                ignore.case = TRUE))[[1]]
  if (length(m) < 2L) NULL else m[[2]]
}

# The name iconv is given: upper case, with the labels browsers read as
# windows-1252 mapped to it, since pages labelled ISO-8859-1 routinely carry
# its curly quotes and dashes in 0x80--0x9F.
bf_charset_name <- function(encoding) {
  enc <- toupper(encoding)
  if (enc %in% c("ISO-8859-1", "ISO8859-1", "ISO_8859-1", "LATIN1", "L1",
                 "US-ASCII", "ASCII", "CP1252")) {
    enc <- "WINDOWS-1252"
  }
  enc
}

bf_has_bom <- function(x) {
  length(x) >= 3L && identical(x[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
}

bf_decode <- function(x, encoding, url) {
  enc <- bf_charset_name(encoding)
  fail <- function(why) {
    stop(bf_error("safeconduct_parse_error", paste0(
      "Could not decode the body of '", url, "' as ", encoding, ": ", why),
      url = url))
  }
  if (!length(x)) return(enc2utf8(""))
  if (any(x == as.raw(0L))) fail("it contains NUL bytes.")
  if (enc %in% c("UTF-8", "UTF8")) {
    out <- rawToChar(x)
    if (!validUTF8(out)) fail("it is not valid UTF-8.")
    Encoding(out) <- "UTF-8"
    return(out)
  }
  out <- tryCatch(iconv(list(x), from = enc, to = "UTF-8"),
                  error = function(e) fail("an unknown charset."))
  if (is.na(out)) fail(paste0("it is not valid ", encoding, "."))
  Encoding(out) <- "UTF-8"
  out
}
