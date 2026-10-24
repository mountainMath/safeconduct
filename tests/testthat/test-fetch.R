# Nothing here reaches the network: probes and the browser are mocked, and the
# curl path is exercised on `file://` URLs.

unchallenged <- function(url, clearance = NULL)
  list(reachable = TRUE, status = 206L, challenged = FALSE, blocked = FALSE,
       type = "application/zip", size = 10, headers = list())
challenged <- function(url, clearance = NULL)
  list(reachable = TRUE, status = 403L, challenged = TRUE, blocked = FALSE,
       type = "text/html", size = NA_real_, headers = list())
blocked <- function(url, clearance = NULL)
  list(reachable = TRUE, status = 403L, challenged = FALSE, blocked = TRUE,
       type = "text/html", size = NA_real_, headers = list())
# Challenged for a plain client, served to one carrying a clearance.
challenged_plain <- function(url, clearance = NULL)
  if (is.null(clearance)) challenged(url) else unchallenged(url)

local_source <- function(env = parent.frame()) {
  src <- withr::local_tempfile(lines = "payload", .local_envir = env)
  paste0("file://", normalizePath(src))
}

# A browser that is there and passes at once.
local_passing_browser <- function(chrome = fake_chrome(), env = parent.frame()) {
  local_mocked_bindings(
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_pass = function(session, url, timeout) a_clearance(url),
    .env = env)
  chrome
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
                        bf_open_browser = function(...) stop("no browser wanted"))
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

# Pin how the browser is run, for tests whose subject is not that.
local_headed <- function(headed, env = parent.frame()) {
  local_mocked_bindings(bf_headed_mode = function(headed = "auto") {
    if (isTRUE(headed)) "headed" else if (isFALSE(headed)) "headless" else mode
  }, .env = env)
  mode <- if (isTRUE(headed)) "headed" else "headless"
  invisible(mode)
}

test_that("a clearance that carries over is used by curl and kept", {
  reset_cache()
  local_headed(TRUE)
  chrome <- local_passing_browser()
  local_mocked_bindings(
    bf_probe = challenged_plain,
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
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      writeLines("x", destfile); invisible(destfile)
    })
  fetch("https://example.org/z.zip", dest, quiet = TRUE)
  expect_true(reopened)
  expect_null(bf_clearance_cache[["example.org"]])
})

test_that("when the clearance does not carry over, the browser downloads", {
  reset_cache()
  local_headed(TRUE)
  chrome <- local_passing_browser()
  got <- NULL
  local_mocked_bindings(
    bf_probe = challenged,
    bf_curl_download = function(...) stop("curl should not be used"),
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      got <<- url; writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- capture_messages(fetch("https://example.org/x.zip", dest))
  expect_match(paste(msgs, collapse = ""), "under escort")
  expect_match(paste(msgs, collapse = ""), "window, which stays open")
  expect_identical(got, "https://example.org/x.zip")
  expect_true(chrome$is_closed())
  expect_null(bf_clearance_cache[["example.org"]])
})

test_that("a blocked URL is downloaded in the browser", {
  reset_cache()
  local_headed(FALSE)
  chrome <- local_passing_browser(fake_chrome(headed = FALSE))
  got <- NULL
  local_mocked_bindings(
    bf_probe = blocked,
    bf_curl_download = function(...) stop("curl should not be used"),
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      got <<- url; writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest)), collapse = "")
  expect_match(msgs, "turns plain requests away")
  expect_match(msgs, "headless")
  expect_match(msgs, "downloading under escort, in the browser\\.")
  expect_identical(got, "https://example.org/x.zip")
  expect_true(chrome$is_closed())
})

test_that("a check that is not passed is a classed error, and the window closes", {
  reset_cache()
  local_headed(TRUE)
  chrome <- fake_chrome()
  local_mocked_bindings(
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_pass = function(session, url, timeout)
      stop(bf_error("safeconduct_not_passed", "not passed", reason = "timeout")))
  expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE),
               "not passed", class = "safeconduct_not_passed")
  expect_true(chrome$is_closed())
})

test_that("by default a headless run that does not get through is retried in a window", {
  reset_cache()
  launched <- logical()
  chromes <- list()
  local_mocked_bindings(
    bf_display_available = function() TRUE,
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) {
      launched <<- c(launched, headed)
      ch <- fake_chrome(headed = headed)
      chromes[[length(chromes) + 1L]] <<- ch
      ch
    },
    bf_wait_pass = function(session, url, timeout) {
      if (!session$headed)
        stop(bf_error("safeconduct_not_passed", "blocked", reason = "blocked"))
      a_clearance(url)
    },
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest)), collapse = "")
  expect_identical(launched, c(FALSE, TRUE))
  expect_match(msgs, "Trying headless Chrome first")
  expect_match(msgs, "did not get through; opening a window")
  expect_true(chromes[[1]]$is_closed())
  expect_true(chromes[[2]]$is_closed())

  # Headless by choice is not retried.
  launched <- logical()
  expect_error(fetch("https://example.org/x.zip", dest, quiet = TRUE, headed = FALSE),
               class = "safeconduct_not_passed")
  expect_identical(launched, FALSE)
})

test_that("by default a check at the file's URL that headless Chrome does not pass is retried in a window", {
  reset_cache()
  reset_hints()
  launched <- logical()
  chromes <- list()
  downloads <- list()
  local_mocked_bindings(
    bf_display_available = function() TRUE,
    bf_probe = challenged,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) {
      launched <<- c(launched, headed)
      ch <- fake_chrome(headed = headed)
      chromes[[length(chromes) + 1L]] <<- ch
      ch
    },
    bf_wait_pass = function(session, url, timeout) a_clearance(url),
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      downloads[[length(downloads) + 1L]] <<- list(headed = headed, ...)
      if (!headed) stop(bf_error("safeconduct_not_passed", "still checking",
                                 reason = "timeout"))
      writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest)), collapse = "")
  expect_identical(launched, c(FALSE, TRUE))
  expect_true(downloads[[1]]$fallback)
  expect_false(isTRUE(downloads[[2]]$fallback))
  expect_match(msgs, "did not get through the check at the file's URL; opening a window")
  expect_true(chromes[[1]]$is_closed())
  expect_true(chromes[[2]]$is_closed())
  expect_identical(readLines(dest), "x")

  # Headless by choice is told the download fails, and not retried.
  launched <- logical(); downloads <- list()
  expect_error(fetch("https://example.org/x.zip", dest, quiet = TRUE, headed = FALSE),
               class = "safeconduct_not_passed")
  expect_identical(launched, FALSE)
  expect_false(downloads[[1]]$fallback)
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
    bf_wait_pass = function(session, url, timeout) a_clearance(url))
  cl <- clearance("https://example.org/", quiet = TRUE)
  expect_s3_class(cl, "safeconduct_clearance")
  expect_identical(clearance("https://example.org/some/page", quiet = TRUE), cl)
  expect_identical(opened, 1L)
  clearance("https://example.org/", refresh = TRUE, quiet = TRUE)
  expect_identical(opened, 2L)
  reset_cache()
})

test_that("download_in_browser() goes straight to the browser", {
  chrome <- local_passing_browser()
  got <- NULL
  local_mocked_bindings(
    bf_probe = function(...) stop("no probe wanted"),
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      got <<- url; invisible(destfile)
    })
  download_in_browser("https://example.org/x.zip", withr::local_tempfile(), quiet = TRUE)
  expect_identical(got, "https://example.org/x.zip")
  expect_true(chrome$is_closed())
})

test_that("read_page() reads with curl where it can", {
  local_mocked_bindings(bf_probe = unchallenged,
                        bf_open_browser = function(...) stop("no browser wanted"))
  r <- read_page(local_source())
  expect_identical(r$status_code, 0L)
  expect_identical(rawToChar(r$content), "payload\n")
  expect_error(read_page("file:///no/such/file.json"),
               class = "safeconduct_download_error")
})

test_that("read_page() reads in the browser where it must", {
  reset_cache()
  local_headed(TRUE)
  chrome <- local_passing_browser(fake_chrome(fake_session(read = read_result())))
  local_mocked_bindings(bf_probe = blocked)
  r <- read_page("https://example.org/feed.json", quiet = TRUE)
  expect_identical(r$status_code, 200L)
  expect_identical(rawToChar(r$content), '{"a":1}')
  expect_true(chrome$is_closed())

  # With a clearance curl can use, curl reads and the clearance is kept.
  reset_cache()
  chrome <- local_passing_browser()
  local_mocked_bindings(
    bf_probe = challenged_plain,
    bf_curl_fetch = function(url, clearance = NULL) list(url = url, clearance = clearance))
  r <- read_page("https://example.org/feed.json", quiet = TRUE)
  expect_s3_class(r$clearance, "safeconduct_clearance")
  expect_s3_class(bf_clearance_cache[["example.org"]], "safeconduct_clearance")
  r <- read_page("https://example.org/other.json", quiet = TRUE)
  expect_identical(r$url, "https://example.org/other.json")
  reset_cache()
})


test_that("a call counts as the console unless a package other than this one is on the stack", {
  ours <- topenv(environment(fetch))
  expect_true(bf_user_call(list(globalenv())))
  expect_true(bf_user_call(list(globalenv(), .BaseNamespaceEnv, ours, NULL)))
  f <- function() NULL
  environment(f) <- new.env(parent = globalenv())
  expect_true(bf_user_call(list(environment(f))))
  expect_false(bf_user_call(list(globalenv(), asNamespace("stats"), ours)))
  expect_false(bf_user_call(list(asNamespace("testthat"))))
  # Called from within testthat, the real stack has testthat on it.
  expect_false(bf_user_call())
})

test_that("tips are shown once per host, at the console, interactively, unless quiet", {
  reset_hints()
  local_mocked_bindings(bf_interactive = function() TRUE,
                        bf_stack_envs = function() list(globalenv()))
  expect_true(bf_hints_on(FALSE))
  expect_false(bf_hints_on(TRUE))
  msgs <- capture_messages(bf_hint(TRUE, "k:a", "do this for 'a'"))
  expect_match(msgs, "Tip: do this for 'a'")
  expect_identical(capture_messages(bf_hint(TRUE, "k:a", "do this for 'a'")), character())
  msgs <- capture_messages(bf_hint(TRUE, "k:b", "do this for 'b'"))
  expect_match(msgs, "Tip: do this for 'b'")
  expect_identical(capture_messages(bf_hint(FALSE, "k:c", "x")), character())

  local_mocked_bindings(bf_interactive = function() FALSE)
  expect_false(bf_hints_on(FALSE))
  local_mocked_bindings(bf_interactive = function() TRUE,
                        bf_stack_envs = function() list(asNamespace("stats")))
  expect_false(bf_hints_on(FALSE))

  # The tip is a classed message a caller can muffle on its own.
  reset_hints()
  seen <- NULL
  msgs <- capture_messages(withCallingHandlers(
    bf_hint(TRUE, "k:a", "x"),
    safeconduct_hint = function(c) { seen <<- c; invokeRestart("muffleMessage") }))
  expect_identical(msgs, character())
  expect_s3_class(seen, "safeconduct_hint")
  reset_hints()
})

test_that("a browser download and an auto fallback each leave a tip", {
  reset_cache(); reset_hints()
  local_mocked_bindings(
    bf_interactive = function() TRUE,
    bf_stack_envs = function() list(globalenv()),
    bf_display_available = function() TRUE,
    bf_probe = blocked,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) fake_chrome(headed = headed),
    bf_wait_pass = function(session, url, timeout) {
      if (!session$headed) stop(bf_error("safeconduct_not_passed", "blocked", reason = "blocked"))
      a_clearance(url)
    },
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest, headed = "auto")),
                collapse = "")
  expect_match(msgs, "headed = TRUE goes straight to one next time")
  expect_no_match(msgs, "useragent")
  expect_match(msgs, 'browser_session\\("https://example.org/", headed = TRUE\\)')
  expect_match(msgs, "fetch\\(url, destfile, session = s\\)")
  # Headless under its own name and blocked: the tips name useragent.
  reset_hints()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest, useragent = NULL)),
                collapse = "")
  expect_match(msgs, "useragent = TRUE drops the word")
  expect_match(msgs, 'browser_session\\("https://example.org/", headed = TRUE, useragent = NULL\\)')
  reset_hints()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest, headed = "auto")),
                collapse = "")
  # Second file from the host: no tips again, and none at all when quiet.
  msgs <- paste(capture_messages(fetch("https://example.org/y.zip", dest, headed = "auto")),
                collapse = "")
  expect_no_match(msgs, "Tip")
  reset_hints()
  expect_identical(capture_messages(fetch("https://example.org/y.zip", dest, quiet = TRUE,
                                          headed = "auto")), character())
  # Nor from inside another package.
  reset_hints()
  local_mocked_bindings(bf_stack_envs = function() list(asNamespace("stats")))
  msgs <- paste(capture_messages(fetch("https://example.org/y.zip", dest, headed = "auto")),
                collapse = "")
  expect_match(msgs, "opening a window")
  expect_no_match(msgs, "Tip")

  reset_hints()
  local_mocked_bindings(bf_stack_envs = function() list(globalenv()),
                        bf_browser_download = function(...) stop("no download"))
  chrome <- fake_chrome(fake_session(read = read_result()))
  local_mocked_bindings(bf_launch_chrome = function(headed) chrome,
                        bf_wait_pass = function(session, url, timeout) a_clearance(url))
  msgs <- paste(capture_messages(read_page("https://example.org/feed.json", headed = TRUE)),
                collapse = "")
  expect_match(msgs, 'browser_session\\("https://example.org/"\\)')
  expect_match(msgs, "read_page\\(url, session = s\\)")
  expect_no_match(msgs, "headed = TRUE goes")
  reset_hints(); reset_cache()
})

test_that("a bad headed argument is refused before a browser is launched", {
  local_mocked_bindings(bf_browser_unavailable_reason = function() NULL,
                        bf_probe = blocked,
                        bf_launch_chrome = function(headed) stop("should not launch"))
  expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(), headed = "yes"),
               "auto")
})

test_that("useragent reaches the browser before the check and the session tip", {
  reset_cache(); reset_hints()
  chrome <- fake_chrome(fake_session(ua = "Mozilla/5.0 (fake) HeadlessChrome/1"), headed = FALSE)
  local_mocked_bindings(
    bf_interactive = function() TRUE,
    bf_stack_envs = function() list(globalenv()),
    bf_probe = blocked,
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_pass = function(session, url, timeout) {
      expect_identical(session$ua, "Mozilla/5.0 (fake) Chrome/1")
      bf_clearance(url, session$ua, "")
    },
    bf_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      writeLines("x", destfile); invisible(destfile)
    })
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest, headed = FALSE)),
                collapse = "")
  expect_match(msgs, 'browser_session\\("https://example.org/"\\)')
  expect_identical(chrome$session$calls[[1]], "ua:Mozilla/5.0 (fake) Chrome/1")
  # The clearance reports the string the browser announced.
  reset_cache()
  cl <- clearance("https://example.org/", quiet = TRUE, headed = FALSE, useragent = TRUE)
  expect_identical(cl$useragent, "Mozilla/5.0 (fake) Chrome/1")
  expect_error(read_page("https://example.org/x.json", useragent = NA), "one string")
  reset_cache(); reset_hints()
})
