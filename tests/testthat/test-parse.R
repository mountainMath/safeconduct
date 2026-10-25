# Nothing here reaches the network: responses are made-up lists in the shape
# `read_page()` returns, and one comes from curl on a `file://` URL.

a_response <- function(content, type = NA_character_, status = 200L,
                       url = "https://example.org/dir/page.html") {
  if (is.character(content)) content <- charToRaw(enc2utf8(content))
  list(url = url, status_code = status, type = type, headers = list(),
       content = content)
}

test_that("page_text() decodes UTF-8 and marks it so", {
  txt <- page_text(a_response("Recensement de la population à Québec",
                              "text/html; charset=utf-8"))
  expect_identical(txt, "Recensement de la population à Québec")
  expect_identical(Encoding(txt), "UTF-8")
})

test_that("page_text() decodes ISO-8859-1 as windows-1252", {
  # "Qu\xe9bec \x96 r\xe9sultats", with an en dash at 0x96.
  raw <- as.raw(c(0x51, 0x75, 0xe9, 0x62, 0x65, 0x63, 0x20, 0x96, 0x20,
                  0x72, 0xe9, 0x73))
  txt <- page_text(a_response(raw, "text/html; charset=ISO-8859-1"))
  expect_identical(txt, "Québec – rés")
})

test_that("page_text() falls back to UTF-8, strips a BOM, and takes an override", {
  expect_identical(page_text(a_response("café")), "café")
  bom <- c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8("café")))
  expect_identical(page_text(a_response(bom, "text/plain; charset=iso-8859-1")),
                   "café")
  expect_identical(page_text(a_response(as.raw(c(0x63, 0xe9)), "text/plain"),
                             encoding = "latin1"), "cé")
  expect_identical(page_text(a_response(raw())), "")
})

test_that("page_text() refuses a body that is not valid in its charset", {
  bad <- a_response(as.raw(c(0x63, 0xe9)), "text/plain; charset=utf-8")
  expect_error(page_text(bad), class = "safeconduct_parse_error")
  expect_error(page_text(a_response("x", "text/plain; charset=no-such-charset")),
               class = "safeconduct_parse_error")
})

test_that("an HTTP error status is raised, unless asked not to", {
  r <- a_response("<h1>Not found</h1>", "text/html", status = 404L)
  err <- expect_error(page_text(r), class = "safeconduct_http_error")
  expect_s3_class(err, "safeconduct_error")
  expect_identical(err$status, 404L)
  expect_identical(err$url, r$url)
  expect_error(page_json(r), class = "safeconduct_http_error")
  expect_identical(page_text(r, check = FALSE), "<h1>Not found</h1>")
})

test_that("page_json() parses JSON and refuses HTML", {
  expect_identical(page_json(a_response('{"a":1,"b":"é"}', "application/json")),
                   list(a = 1L, b = "é"))
  expect_identical(page_json(a_response("[1,2]"), simplifyVector = FALSE),
                   list(1L, 2L))
  # A "not found" page served with status 200.
  soft404 <- a_response("<!DOCTYPE html><title>Not found</title>", "text/html")
  expect_error(page_json(soft404), "not JSON", class = "safeconduct_parse_error")
})

test_that("a response from curl on a file:// URL parses", {
  src <- withr::local_tempfile(fileext = ".json")
  writeBin(charToRaw('{"a":1}'), src)
  r <- sc_curl_fetch(paste0("file://", normalizePath(src)))
  expect_identical(page_json(r), list(a = 1L))
})

test_that("page_html() parses HTML and resolves links against the final URL", {
  skip_if_not_installed("xml2")
  doc <- page_html(a_response(
    '<html><body><a href="data.csv">Données</a></body></html>',
    "text/html; charset=utf-8"))
  a <- xml2::xml_find_first(doc, "//a")
  expect_identical(xml2::xml_text(a), "Données")
  expect_identical(xml2::url_absolute(xml2::xml_attr(a, "href"), xml2::xml_url(doc)),
                   "https://example.org/dir/data.csv")
  expect_error(page_html(a_response("<p>x</p>", "text/html", status = 500L)),
               class = "safeconduct_http_error")
})

test_that("page_html() reads the charset from <meta> when the header has none", {
  skip_if_not_installed("xml2")
  html <- c(charToRaw('<html><head><meta charset="iso-8859-1"></head><body><p>Qu'),
            as.raw(0xe9), charToRaw("bec</p></body></html>"))
  doc <- page_html(a_response(html, "text/html"))
  expect_identical(xml2::xml_text(xml2::xml_find_first(doc, "//p")), "Québec")
})

test_that("a URL is read with read_page(), and anything else is refused", {
  local_mocked_bindings(read_page = function(url, ...)
    a_response('{"a":1}', "application/json", url = url))
  expect_identical(page_json("https://example.org/a.json"), list(a = 1L))
  expect_identical(page_text("https://example.org/a.json"), '{"a":1}')
  expect_error(page_text(c("a", "b")), "response from read_page")
  expect_error(page_text(list(url = "x")), "response from read_page")
})
