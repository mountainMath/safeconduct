test_that("cookies become one header value", {
  expect_identical(
    bf_cookie_header(list(list(name = "cf_clearance", value = "abc"),
                          list(name = "x", value = "1"))),
    "cf_clearance=abc; x=1")
  expect_identical(bf_cookie_header(list()), "")
})

test_that("Chrome's port file is read", {
  profile <- withr::local_tempdir()
  writeLines(c("54321", "/devtools/browser/0d2e-4c"), file.path(profile, "DevToolsActivePort"),
             sep = "\n")
  expect_identical(bf_devtools_port(profile), 54321L)
  cat("61000\n/devtools/browser/0d2e", file = file.path(profile, "DevToolsActivePort"))
  expect_identical(bf_devtools_port(profile), 61000L)
  expect_error(bf_devtools_port(withr::local_tempdir(), timeout = 0.3),
               "did not start")
})

test_that("the browser can be switched off by option", {
  withr::local_options(safeconduct.browser = FALSE)
  ok <- browser_available()
  expect_false(ok)
  expect_match(attr(ok, "reason"), "safeconduct.browser")
})

test_that("the window has a default size, or takes the option", {
  expect_identical(bf_window_args(), "--window-size=1000,800")
  withr::local_options(safeconduct.window = c(1280, 1024))
  expect_identical(bf_window_args(), "--window-size=1280,1024")
  withr::local_options(safeconduct.window = "maximized")
  expect_identical(bf_window_args(), "--start-maximized")
  withr::local_options(safeconduct.window = "big")
  expect_error(bf_window_args(), "safeconduct.window")
})

test_that("the focus script names the selector and can be switched off", {
  js <- bf_focus_script(".card", 2)
  expect_match(js, '})(".card", 2)', fixed = TRUE)
  expect_match(js, "setProperty(prop, value, 'important')", fixed = TRUE)
  expect_null(bf_focus_script())
  withr::local_options(safeconduct.focus = FALSE)
  expect_null(bf_focus_script())
  withr::local_options(safeconduct.focus = "#challenge-stage")
  expect_match(bf_focus_script(), '("#challenge-stage", 1.5)', fixed = TRUE)
})

test_that("the headed mode follows the display and the option", {
  local_mocked_bindings(bf_display_available = function() TRUE)
  withr::local_options(safeconduct.headed = NULL)
  expect_identical(bf_headed_mode(), "headed")
  withr::local_options(safeconduct.headed = FALSE)
  expect_identical(bf_headed_mode(), "headless")
  withr::local_options(safeconduct.headed = TRUE)
  expect_identical(bf_headed_mode(), "headed")
  withr::local_options(safeconduct.headed = "auto")
  expect_identical(bf_headed_mode(), "auto")

  local_mocked_bindings(bf_display_available = function() FALSE)
  withr::local_options(safeconduct.headed = NULL)
  expect_identical(bf_headed_mode(), "headless")
  withr::local_options(safeconduct.headed = "auto")
  expect_identical(bf_headed_mode(), "headless")
  withr::local_options(safeconduct.headed = TRUE)
  expect_identical(bf_headed_mode(), "headed")
})

cookies <- list(list(name = "cf_clearance", value = "abc"),
                list(name = "__cf_bm", value = "z"))

test_that("a page answered to the browser is a pass, with its cookies", {
  s <- fake_session(events = list(doc_event(200)), cookies = cookies)
  cl <- bf_wait_pass(s, "https://example.org/", timeout = 5)
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
  expect_identical(bf_wait_pass(s, "https://example.org/", timeout = 5)$cookie,
                   "__cf_bm=z")
  expect_output(print(bf_clearance("https://example.org/", "UA", "")), "none")
})

test_that("a check is waited out, a redirect followed, an iframe ignored", {
  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(403, challenge_headers),
                                  doc_event(302), doc_event(200)),
                    cookies = cookies)
  cl <- bf_wait_pass(s, "https://example.org/", timeout = 10)
  expect_s3_class(cl, "safeconduct_clearance")

  s <- fake_session(events = list(doc_event(403, challenge_headers),
                                  doc_event(200, frameId = "turnstile-iframe")))
  err <- expect_error(bf_wait_pass(s, "https://example.org/", timeout = 1.5),
                      "not passed", class = "safeconduct_not_passed")
  expect_identical(err$reason, "timeout")

  # The origin's own error pages are the host answering the browser.
  s <- fake_session(events = list(doc_event(404, cf_headers)))
  expect_s3_class(bf_wait_pass(s, "https://example.org/", 5), "safeconduct_clearance")
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r9")),
                    bodies = list(r9 = "<html>Forbidden by the origin</html>"))
  expect_s3_class(bf_wait_pass(s, "https://example.org/", 5), "safeconduct_clearance")
})

test_that("a block page ends the wait at once", {
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r2")),
                    bodies = list(r2 = block_page))
  started <- Sys.time()
  err <- expect_error(bf_wait_pass(s, "https://example.org/", timeout = 30),
                      "blocks this browser", class = "safeconduct_not_passed")
  expect_identical(err$reason, "blocked")
  expect_lt(as.numeric(Sys.time() - started, units = "secs"), 5)

  # A Cloudflare 403 whose body never becomes readable is taken for a block
  # after a short grace.
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "gone")))
  err <- expect_error(bf_wait_pass(s, "https://example.org/", timeout = 30),
                      class = "safeconduct_not_passed")
  expect_identical(err$reason, "blocked")
})

test_that("a browser that goes away during the wait is reported", {
  s <- fake_session()
  s$Browser$getVersion <- function() stop("connection lost")
  expect_error(bf_wait_pass(s, "https://example.org/", 5), "went away",
               class = "safeconduct_not_passed")
})

test_that("a browser download lands at its destination", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(
    doc_event(200, list(`content-type` = "application/zip"), mimeType = "application/zip"),
    dl_event("inProgress", "g1", 10, 20),
    dl_event("completed", "g1", 20, 20)))
  expect_identical(bf_browser_download(s, "https://example.org/file.zip", dest,
                                       quiet = TRUE), dest)
  expect_identical(readLines(dest), "data")
  expect_false(file.exists(file.path(dir, "g1")))
  expect_true("navigate:https://example.org/file.zip" %in% s$calls)
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
  msgs <- capture_messages(bf_browser_download(s, "https://example.org/file.zip", dest))
  expect_match(paste(msgs, collapse = ""), "click the box")
  expect_identical(readLines(dest), "data")
})

test_that("a block page at the file's URL ends the download at once", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(doc_event(403, cf_headers, requestId = "r2")),
                    bodies = list(r2 = block_page))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               "blocks this browser", class = "safeconduct_not_passed")
  expect_false(file.exists(dest))
})

test_that("a page where a file was expected is an error after a grace", {
  local_mocked_bindings(bf_page_grace = function() 0.6)
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(doc_event(404, list(`content-type` = "text/html"))))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE, timeout = 10),
               "page \\(status 404\\)", class = "safeconduct_download_error")
})

test_that("a cancelled browser download is an error and leaves nothing", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(
    dl_event("inProgress", "g2", 10, 20),
    dl_event("canceled", "g2", 10, 20)))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               class = "safeconduct_download_error")
  expect_false(file.exists(dest))
  expect_true("cancel:g2" %in% s$calls)
})

test_that("a browser download that overruns its timeout is cancelled", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(events = list(dl_event("inProgress", "g3", 1, 20)))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE, timeout = 0.7),
               "did not finish", class = "safeconduct_download_error")
  expect_true("cancel:g3" %in% s$calls)
})

test_that("a resource is read in the browser", {
  s <- fake_session(read = read_result(headers = list(`Content-Type` = "application/json",
                                                      `cf-ray` = "x")))
  r <- bf_browser_read(s, "https://example.org/feed.json")
  expect_identical(r$status_code, 200L)
  expect_identical(r$type, "application/json")
  expect_identical(rawToChar(r$content), '{"a":1}')
  expect_identical(r$headers[["cf-ray"]], "x")
  expect_identical(r$url, "https://example.org/feed.json")

  s <- fake_session(read = list(ok = FALSE, error = "TypeError: Failed to fetch"))
  expect_error(bf_browser_read(s, "https://example.org/feed.json"),
               "Failed to fetch", class = "safeconduct_download_error")
  s <- fake_session(read = NULL)
  expect_error(bf_browser_read(s, "https://example.org/feed.json"),
               class = "safeconduct_download_error")
})
