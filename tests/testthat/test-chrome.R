test_that("cookies become one header value", {
  expect_identical(
    sc_cookie_header(list(list(name = "cf_clearance", value = "abc"),
                          list(name = "x", value = "1"))),
    "cf_clearance=abc; x=1")
  expect_identical(sc_cookie_header(list()), "")
})

test_that("Chrome's port file is read", {
  profile <- withr::local_tempdir()
  writeLines(c("54321", "/devtools/browser/0d2e-4c"), file.path(profile, "DevToolsActivePort"),
             sep = "\n")
  expect_identical(sc_devtools_port(profile), 54321L)
  cat("61000\n/devtools/browser/0d2e", file = file.path(profile, "DevToolsActivePort"))
  expect_identical(sc_devtools_port(profile), 61000L)
  expect_error(sc_devtools_port(withr::local_tempdir(), timeout = 0.3),
               "did not start")
})

test_that("the browser can be switched off by option", {
  withr::local_options(safeconduct.browser = FALSE)
  ok <- browser_available()
  expect_false(ok)
  expect_match(attr(ok, "reason"), "safeconduct.browser")
})

test_that("the window has a default size, or takes the option", {
  expect_identical(sc_window_args(), "--window-size=1000,800")
  withr::local_options(safeconduct.window = c(1280, 1024))
  expect_identical(sc_window_args(), "--window-size=1280,1024")
  withr::local_options(safeconduct.window = "maximized")
  expect_identical(sc_window_args(), "--start-maximized")
  withr::local_options(safeconduct.window = "big")
  expect_error(sc_window_args(), "safeconduct.window")
})

test_that("the focus script names the selector and can be switched off", {
  js <- sc_focus_script(".card", 2)
  expect_match(js, '})(".card", 2)', fixed = TRUE)
  expect_match(js, "setProperty(prop, value, 'important')", fixed = TRUE)
  expect_null(sc_focus_script())
  withr::local_options(safeconduct.focus = FALSE)
  expect_null(sc_focus_script())
  withr::local_options(safeconduct.focus = "#challenge-stage")
  expect_match(sc_focus_script(), '("#challenge-stage", 1.5)', fixed = TRUE)
})

test_that("the headed mode follows the argument and the display", {
  local_mocked_bindings(sc_display_available = function() TRUE)
  expect_identical(sc_headed_mode(), "auto")
  expect_identical(sc_headed_mode("auto"), "auto")
  expect_identical(sc_headed_mode(NULL), "auto")
  expect_identical(sc_headed_mode(FALSE), "headless")
  expect_identical(sc_headed_mode(TRUE), "headed")
  expect_error(sc_headed_mode("yes"), "auto")

  local_mocked_bindings(sc_display_available = function() FALSE)
  expect_identical(sc_headed_mode(), "headless")
  expect_identical(sc_headed_mode("auto"), "headless")
  expect_identical(sc_headed_mode(TRUE), "headed")
})

cookies <- list(list(name = "cf_clearance", value = "abc"),
                list(name = "__cf_bm", value = "z"))

test_that("a page answered to the browser is a pass, with its cookies", {
  s <- fake_session(events = list(doc_event(200)), cookies = cookies)
  cl <- sc_wait_pass(s, "https://example.org/", timeout = 5)
  expect_s3_class(cl, "safeconduct_clearance")
  expect_identical(cl$host, "example.org")
  expect_identical(cl$cookie, "cf_clearance=abc; __cf_bm=z")
  expect_identical(cl$useragent, "Mozilla/5.0 (fake)")
  expect_true("navigate:https://example.org/" %in% s$calls)
  expect_output(print(cl), "example.org")
  expect_output(print(cl), "cf_clearance, __cf_bm")

  # A host that never checks the browser hands out no clearance cookie, and
  # that is a pass all the same.
  s <- fake_session(events = list(doc_event(200)),
                    cookies = list(list(name = "__cf_bm", value = "z")))
  expect_identical(sc_wait_pass(s, "https://example.org/", timeout = 5)$cookie,
                   "__cf_bm=z")
  expect_output(print(sc_clearance("https://example.org/", "UA", "")), "none")
})

test_that("a check is waited out, a redirect followed, an iframe ignored", {
  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(403, challenge_headers),
                                  doc_event(302), doc_event(200)),
                    cookies = cookies)
  cl <- sc_wait_pass(s, "https://example.org/", timeout = 10)
  expect_s3_class(cl, "safeconduct_clearance")

  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(200, frameId = "turnstile-iframe")))
  err <- expect_error(sc_wait_pass(s, "https://example.org/", timeout = 1.5),
                      "not passed", class = "safeconduct_not_passed")
  expect_identical(err$reason, "timeout")

  # The origin's own error pages are the host answering the browser.
  s <- fake_session(events = list(doc_event(404, cf_headers)))
  expect_s3_class(sc_wait_pass(s, "https://example.org/", 5), "safeconduct_clearance")
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r9")),
                    bodies = list(r9 = "<html>Forbidden by the origin</html>"))
  expect_s3_class(sc_wait_pass(s, "https://example.org/", 5), "safeconduct_clearance")
})

test_that("a block page ends the wait at once", {
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r2")),
                    bodies = list(r2 = block_page))
  started <- Sys.time()
  err <- expect_error(sc_wait_pass(s, "https://example.org/", timeout = 30),
                      "blocks this browser", class = "safeconduct_not_passed")
  expect_identical(err$reason, "blocked")
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 5)

  # A Cloudflare 403 whose body never becomes readable is taken for a block
  # after a short grace.
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "gone")))
  err <- expect_error(sc_wait_pass(s, "https://example.org/", timeout = 30),
                      class = "safeconduct_not_passed")
  expect_identical(err$reason, "blocked")
})

test_that("a browser that goes away during the wait is reported", {
  s <- fake_session()
  s$Browser$getVersion <- function() stop("connection lost")
  expect_error(sc_wait_pass(s, "https://example.org/", 5), "went away",
               class = "safeconduct_not_passed")
})

test_that("a browser download lands at its destination", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(
    doc_event(200, list(`content-type` = "application/zip"), mimeType = "application/zip"),
    dl_event("inProgress", "g1", 10, 20),
    dl_event("completed", "g1", 20, 20)))
  expect_identical(sc_browser_download(s, "https://example.org/file.zip", dest,
                                       quiet = TRUE), dest)
  expect_identical(readLines(dest), "data")
  expect_false(file.exists(file.path(dir, "g1")))
  expect_true("navigate:https://example.org/file.zip" %in% s$calls)
  # The browser lands the file under tempdir(), not next to the destination.
  expect_identical(s$download_dir, file.path(tempdir(), "safeconduct"))
  expect_false(file.exists(file.path(s$download_dir, "g1")))
})

test_that("a destination in a directory that does not exist is refused first", {
  s <- fake_session()
  expect_error(sc_browser_download(s, "https://example.org/file.zip",
                                   "/no/such/dir/file.zip", quiet = TRUE),
               "does not exist")
  expect_length(s$calls, 0)
})

test_that("a finished download is moved into place, by rename or copy", {
  dir <- withr::local_tempdir()
  from <- file.path(dir, "guid"); to <- file.path(dir, "file.zip")
  writeLines("data", from)
  expect_true(sc_move_file(from, to))
  expect_false(file.exists(from))
  expect_identical(readLines(to), "data")
  expect_false(sc_move_file(file.path(dir, "missing"), to))
})

test_that("a check at the file's URL runs and the download follows", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(
    doc_event(403, challenge_headers),
    doc_event(403, challenge_headers),
    doc_event(200, list(`content-type` = "application/zip"), mimeType = "application/zip"),
    dl_event("inProgress", "g1", 10, 20),
    dl_event("completed", "g1", 20, 20)))
  msgs <- capture_messages(sc_browser_download(s, "https://example.org/file.zip", dest))
  expect_match(paste(msgs, collapse = ""), "click the box")
  expect_identical(readLines(dest), "data")
  s <- fake_session(events = list(
    doc_event(403, challenge_headers),
    doc_event(200, list(`content-type` = "application/zip"), mimeType = "application/zip"),
    dl_event("completed", "g1", 20, 20)))
  msgs <- capture_messages(sc_browser_download(s, "https://example.org/file.zip", dest,
                                               headed = FALSE))
  expect_match(paste(msgs, collapse = ""), "may take two minutes")
  expect_match(paste(msgs, collapse = ""), "the download fails")
  expect_no_match(paste(msgs, collapse = ""), "click the box")
})

test_that("a check at the file's URL still running after its wait is not passed", {
  local_mocked_bindings(sc_check_wait = function(headed) 0.6)
  reset_hints()
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = rep(list(doc_event(403, challenge_headers)), 10))
  msgs <- character()
  err <- tryCatch(
    withCallingHandlers(
      sc_browser_download(s, "https://example.org/file.zip", dest, headed = FALSE,
                          fallback = TRUE, hints = TRUE),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) e)
  expect_s3_class(err, "safeconduct_not_passed")
  expect_identical(err$reason, "timeout")
  expect_match(conditionMessage(err), "not passed within")
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "a window is opened")
  expect_match(msgs, "Tip: rather than wait, headed = TRUE")
  expect_false(file.exists(dest))
  # A window waits for its person and says nothing about waiting.
  s <- fake_session(events = rep(list(doc_event(403, challenge_headers)), 10))
  msgs <- paste(capture_messages(tryCatch(
    sc_browser_download(s, "https://example.org/file.zip", dest, headed = TRUE),
    error = function(e) NULL)), collapse = "")
  expect_match(msgs, "click the box")
  expect_no_match(msgs, "Tip:")
})

test_that("the waits in the download loop are read in seconds past a minute", {
  # `Sys.time() - then` is a difftime in whatever unit fits the gap, minutes
  # past the first one, and compared with a bare number of seconds it let
  # every limit here slide once a loop had run a minute. The clock is moved
  # by hand so that the real limits are crossed in a few ticks, and a tick
  # count stops a loop that would otherwise spin for hours.
  ticking_clock <- function(step) {
    t0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
    n <- 0
    function() {
      n <<- n + 1
      if (n > 30) stop("the loop did not stop in time")
      t0 + n * step
    }
  }
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  # A check at the file's URL, with the real 150-second headless wait and
  # the clock moving 100 seconds a tick: timed out on the third tick.
  local_mocked_bindings(sc_now = ticking_clock(100))
  s <- fake_session(events = rep(list(doc_event(403, challenge_headers)), 30))
  err <- tryCatch(
    sc_browser_download(s, "https://example.org/file.zip", dest, quiet = TRUE,
                        headed = FALSE),
    error = function(e) e)
  expect_s3_class(err, "safeconduct_not_passed")
  expect_identical(err$reason, "timeout")
  # A transfer still going past the default hour, with bytes arriving every
  # tick so that only the overall timeout can end it.
  local_mocked_bindings(sc_now = ticking_clock(1000))
  s <- fake_session(events = lapply(1:30, function(i) dl_event("inProgress", "g9", i, 100)))
  expect_error(sc_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               "did not finish in 3600 seconds", class = "safeconduct_download_error")
  expect_true("cancel:g9" %in% s$calls)
  expect_true(sc_secs_since(as.POSIXct("2026-11-01", tz = "UTC") - 200,
                            as.POSIXct("2026-11-01", tz = "UTC")) > 150)
})

test_that("a check brings the window to the front, once", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  # A check at the file's URL in a window: raised once, when the check shows.
  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(403, challenge_headers),
                                  dl_event("completed", "g7", 10, 10)))
  sc_browser_download(s, "https://example.org/file.zip", dest, quiet = TRUE,
                      headed = TRUE)
  expect_identical(sum(s$calls == "front"), 1L)
  # Headless Chrome has no window to raise.
  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  dl_event("completed", "g8", 10, 10)))
  sc_browser_download(s, "https://example.org/file.zip", dest, quiet = TRUE,
                      headed = FALSE)
  expect_false("front" %in% s$calls)
  # The check at the root as well, for a session's window asked again.
  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(403, challenge_headers),
                                  doc_event(200)), cookies = cookies)
  sc_wait_pass(s, "https://example.org/", timeout = 10)
  expect_identical(sum(s$calls == "front"), 1L)
  # No check, nothing raised.
  s <- fake_session(events = list(doc_event(200)), cookies = cookies)
  sc_wait_pass(s, "https://example.org/", timeout = 5)
  expect_false("front" %in% s$calls)
})

test_that("a block page at the file's URL ends the download at once", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r2")),
                    bodies = list(r2 = block_page))
  expect_error(sc_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               "blocks this browser", class = "safeconduct_not_passed")
  expect_false(file.exists(dest))
})

test_that("a page where a file was expected is an error after a grace", {
  local_mocked_bindings(sc_page_grace = function() 0.6)
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(doc_event(404, list(`content-type` = "text/html"))))
  expect_error(sc_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE, timeout = 10),
               "page \\(status 404\\)", class = "safeconduct_download_error")
})

test_that("a cancelled browser download is an error and leaves nothing", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(
    dl_event("inProgress", "g2", 10, 20),
    dl_event("canceled", "g2", 10, 20)))
  expect_error(sc_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               class = "safeconduct_download_error")
  expect_false(file.exists(dest))
  expect_true("cancel:g2" %in% s$calls)
})

test_that("a browser download that overruns its timeout is cancelled", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(dl_event("inProgress", "g3", 1, 20)))
  expect_error(sc_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE, timeout = 0.7),
               "did not finish", class = "safeconduct_download_error")
  expect_true("cancel:g3" %in% s$calls)
})

test_that("a resource is read in the browser", {
  s <- fake_session(read = read_result(headers = list(`Content-Type` = "application/json",
                                                      `cf-ray` = "x")))
  r <- sc_browser_read(s, "https://example.org/feed.json")
  expect_identical(r$status_code, 200L)
  expect_identical(r$type, "application/json")
  expect_identical(rawToChar(r$content), '{"a":1}')
  expect_identical(r$headers[["cf-ray"]], "x")
  expect_identical(r$url, "https://example.org/feed.json")

  s <- fake_session(read = list(ok = FALSE, error = "TypeError: Failed to fetch"))
  expect_error(sc_browser_read(s, "https://example.org/feed.json"),
               "Failed to fetch", class = "safeconduct_download_error")
  s <- fake_session(read = NULL)
  expect_error(sc_browser_read(s, "https://example.org/feed.json", timeout = 7),
               "no answer within 7 seconds", class = "safeconduct_download_error")
})

test_that("a body too large to carry is refused fast, naming fetch()", {
  s <- fake_session(read = read_too_large(size = 40L * 1024L * 1024L))
  e <- expect_error(sc_browser_read(s, "https://example.org/feed.json"),
                    "too large to read", class = "safeconduct_download_error")
  expect_identical(e$reason, "too_large")
  expect_match(conditionMessage(e), "40 MB")
  expect_match(conditionMessage(e), "fetch\\(\\)")
})

test_that("a read in a browser that has gone away says so", {
  gone <- function(expr) stop("Session and underlying target have been closed.")
  s <- fake_session(read = gone)
  s$active <- FALSE
  e <- expect_error(sc_browser_read(s, "https://example.org/feed.json"),
                    "browser went away", class = "safeconduct_download_error")
  expect_identical(e$reason, "gone")
  # The protocol raised but the browser is still there: its own words.
  s <- fake_session(read = function(expr) stop("Timeout waiting for response"))
  e <- expect_error(sc_browser_read(s, "https://example.org/feed.json"),
                    "Timeout waiting for response")
  expect_null(e$reason)
})

test_that("a download in a browser that has gone away says so", {
  s <- fake_session()
  s$Browser$getVersion <- function() stop("Session has been closed.")
  e <- expect_error(
    sc_browser_download(s, "https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE),
    "browser went away", class = "safeconduct_download_error")
  expect_identical(e$reason, "gone")
})

test_that("the user-agent string is left alone, trimmed of Headless, or set", {
  headless_ua <- "Mozilla/5.0 (X11) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/155.0.0.0 Safari/537.36"
  s <- fake_session(ua = headless_ua)
  sc_set_useragent(s, NULL)
  expect_identical(s$ua, headless_ua)
  expect_length(s$calls, 0L)
  sc_set_useragent(s, TRUE)
  expect_identical(s$ua, sub("HeadlessChrome/", "Chrome/", headless_ua))
  expect_false(grepl("Headless", s$ua))
  # A window's string has no Headless in it: nothing is overridden.
  s <- fake_session(ua = "Mozilla/5.0 (fake) Chrome/155")
  sc_set_useragent(s, TRUE)
  expect_length(s$calls, 0L)
  sc_set_useragent(s, "Custom/1")
  expect_identical(s$ua, "Custom/1")
  expect_error(sc_set_useragent(s, 1), "one string")
  expect_error(sc_set_useragent(s, c("a", "b")), "one string")
  expect_error(sc_set_useragent(s, ""), "one string")
})

# The one test that opens a real browser. processx's supervisor leaves two
# fifo connections open in the session for good, which fails any downstream
# example under R CMD check; Chrome is launched without it.
test_that("opening and closing a browser leaves no connection behind", {
  skip_on_cran()
  skip_if_not(isTRUE(browser_available()), "no browser here")
  n0 <- nrow(showConnections())
  chrome <- sc_launch_chrome(headed = FALSE)
  chrome$close()
  expect_identical(nrow(showConnections()), n0)
  skip_if_not(sc_display_available(), "no display for a window")
  chrome <- sc_launch_chrome(headed = TRUE)
  chrome$close()
  expect_identical(nrow(showConnections()), n0)
})
