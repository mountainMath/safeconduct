# Nothing here reaches the network: probes and the browser are mocked, and the
# curl path is exercised on `file://` URLs.

unchallenged <- function(url, clearance = NULL)
  list(reachable = TRUE, status = 206L, challenged = FALSE,
       type = "application/zip", size = 10, headers = list())
challenged <- function(url, clearance = NULL)
  list(reachable = TRUE, status = 403L, challenged = TRUE,
       type = "text/html", size = NA_real_, headers = list())
# Challenged for a plain client, served to one carrying a clearance.
challenged_plain <- function(url, clearance = NULL)
  if (is.null(clearance)) challenged(url) else unchallenged(url)

local_source <- function(env = parent.frame()) {
  src <- withr::local_tempfile(lines = "payload", .local_envir = env)
  paste0("file://", normalizePath(src))
}

test_that("curl downloads a file, and fails cleanly", {
  dest <- withr::local_tempfile()
  expect_identical(bf_curl_download(local_source(), dest, quiet = TRUE), dest)
  expect_identical(readLines(dest), "payload")

  dest2 <- withr::local_tempfile()
  expect_error(bf_curl_download("file:///no/such/file.zip", dest2, quiet = TRUE),
               class = "safeconduct_download_error")
  expect_false(file.exists(dest2))
})

test_that("an unchallenged URL is fetched without a browser", {
  local_mocked_bindings(bf_probe = unchallenged,
                        bf_open_and_clear = function(...) stop("no browser wanted"))
  dest <- withr::local_tempfile()
  expect_identical(fetch(local_source(), dest, quiet = TRUE), dest)
  expect_identical(readLines(dest), "payload")
})

test_that("without a browser, a challenged URL is a classed error", {
  reset_cache()
  withr::local_options(safeconduct.browser = FALSE)
  local_mocked_bindings(bf_probe = challenged)
  err <- expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(),
                            quiet = TRUE),
                      class = "safeconduct_unavailable")
  expect_s3_class(err, "safeconduct_error")
  expect_match(conditionMessage(err), "safeconduct.browser")
})

test_that("a clearance that carries over is used by curl and kept", {
  reset_cache()
  chrome <- fake_chrome()
  local_mocked_bindings(
    bf_probe = challenged_plain,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_clearance = function(session, url, timeout) a_clearance(url),
    bf_browser_download = function(...) stop("should not download in browser"))
  dest <- withr::local_tempfile()
  downloaded <- NULL
  local_mocked_bindings(bf_curl_download = function(url, destfile, quiet, clearance = NULL) {
    downloaded <<- list(url = url, clearance = clearance)
    writeLines("x", destfile)
    invisible(destfile)
  })
  msgs <- capture_messages(fetch("https://example.org/x.zip", dest))
  expect_match(paste(msgs, collapse = ""), "browser check")
  expect_match(paste(msgs, collapse = ""), "Passed; downloading")
  expect_s3_class(downloaded$clearance, "safeconduct_clearance")
  expect_true(chrome$is_closed())
  expect_s3_class(bf_clearance_cache[["example.org"]], "safeconduct_clearance")

  # The next file from the host reuses the clearance without a browser.
  local_mocked_bindings(bf_launch_chrome = function(headed) stop("no second window"))
  fetch("https://example.org/y.zip", dest, quiet = TRUE)
  expect_identical(downloaded$url, "https://example.org/y.zip")

  # A lapsed clearance is replaced.
  reopened <- FALSE
  local_mocked_bindings(
    bf_probe = challenged,
    bf_launch_chrome = function(headed) { reopened <<- TRUE; fake_chrome() },
    bf_browser_download = function(session, url, destfile, quiet, timeout) {
      writeLines("x", destfile); invisible(destfile)
    })
  fetch("https://example.org/z.zip", dest, quiet = TRUE)
  expect_true(reopened)
  expect_null(bf_clearance_cache[["example.org"]])
})

test_that("when the clearance does not carry over, the browser downloads", {
  reset_cache()
  chrome <- fake_chrome()
  got <- NULL
  local_mocked_bindings(
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_clearance = function(session, url, timeout) a_clearance(url),
    bf_curl_download = function(...) stop("curl should not be used"),
    bf_browser_download = function(session, url, destfile, quiet, timeout) {
      got <<- url; writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- capture_messages(fetch("https://example.org/x.zip", dest))
  expect_match(paste(msgs, collapse = ""), "only for the browser")
  expect_identical(got, "https://example.org/x.zip")
  expect_true(chrome$is_closed())
  expect_null(bf_clearance_cache[["example.org"]])
})

test_that("a check that is not passed is a classed error, and the window closes", {
  reset_cache()
  chrome <- fake_chrome()
  local_mocked_bindings(
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_clearance = function(session, url, timeout) NULL)
  expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE),
               "not passed", class = "safeconduct_not_passed")
  expect_true(chrome$is_closed())
})

test_that("a Chrome that will not start is reported as unavailable", {
  reset_cache()
  local_mocked_bindings(
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) stop("no DevTools"))
  expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE),
               "no DevTools", class = "safeconduct_unavailable")
})

test_that("clearance() caches per host and refreshes on request", {
  reset_cache()
  opened <- 0L
  local_mocked_bindings(
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) { opened <<- opened + 1L; fake_chrome() },
    bf_wait_clearance = function(session, url, timeout) a_clearance(url))
  cl <- clearance("https://example.org/", quiet = TRUE)
  expect_s3_class(cl, "safeconduct_clearance")
  expect_identical(clearance("https://example.org/some/page", quiet = TRUE), cl)
  expect_identical(opened, 1L)
  clearance("https://example.org/", refresh = TRUE, quiet = TRUE)
  expect_identical(opened, 2L)
  reset_cache()
})

test_that("download_in_browser() goes straight to the window", {
  chrome <- fake_chrome()
  got <- NULL
  local_mocked_bindings(
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_clearance = function(session, url, timeout) a_clearance(url),
    bf_browser_download = function(session, url, destfile, quiet, timeout) {
      got <<- url; invisible(destfile)
    })
  download_in_browser("https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE)
  expect_identical(got, "https://example.org/x.zip")
  expect_true(chrome$is_closed())
})
