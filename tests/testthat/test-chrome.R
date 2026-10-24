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

test_that("the display guess follows the option", {
  withr::local_options(safeconduct.headed = FALSE)
  expect_false(bf_display_available())
  withr::local_options(safeconduct.headed = TRUE)
  expect_true(bf_display_available())
})

test_that("a clearance arrives from the cookies", {
  s <- fake_session(cookies = list(list(name = "cf_clearance", value = "abc"),
                                   list(name = "__cf_bm", value = "z")))
  cl <- bf_wait_clearance(s, "https://example.org/", timeout = 5)
  expect_s3_class(cl, "safeconduct_clearance")
  expect_identical(cl$host, "example.org")
  expect_identical(cl$cookie, "cf_clearance=abc; __cf_bm=z")
  expect_identical(cl$useragent, "Mozilla/5.0 (fake)")
  expect_output(print(cl), "example.org")
  expect_output(print(cl), "cf_clearance, __cf_bm")

  s <- fake_session(cookies = list(list(name = "__cf_bm", value = "z")))
  expect_null(bf_wait_clearance(s, "https://example.org/", timeout = 0.6))
})

test_that("a browser download lands at its destination", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(progress = list(
    list(state = "inProgress", guid = "g1", receivedBytes = 10, totalBytes = 20),
    list(state = "completed", guid = "g1", receivedBytes = 20, totalBytes = 20)))
  expect_identical(bf_browser_download(s, "https://example.org/file.zip", dest,
                                       quiet = TRUE), dest)
  expect_identical(readLines(dest), "data")
  expect_false(file.exists(file.path(dir, "g1")))
  expect_true("navigate:https://example.org/file.zip" %in% s$calls)
})

test_that("a cancelled browser download is an error and leaves nothing", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(progress = list(
    list(state = "inProgress", guid = "g2", receivedBytes = 10, totalBytes = 20),
    list(state = "canceled", guid = "g2", receivedBytes = 10, totalBytes = 20)))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE),
               class = "safeconduct_download_error")
  expect_false(file.exists(dest))
  expect_true("cancel:g2" %in% s$calls)
})

test_that("a browser download that overruns its timeout is cancelled", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "file.zip")
  s <- fake_session(progress = list(
    list(state = "inProgress", guid = "g3", receivedBytes = 1, totalBytes = 20)))
  expect_error(bf_browser_download(s, "https://example.org/file.zip", dest,
                                   quiet = TRUE, timeout = 0.7),
               "did not finish", class = "safeconduct_download_error")
  expect_true("cancel:g3" %in% s$calls)
})
