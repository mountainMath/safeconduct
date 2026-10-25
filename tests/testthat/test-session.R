local_session <- function(session = fake_session(read = read_result()),
                          headed = TRUE, env = parent.frame(), ...) {
  chrome <- fake_chrome(session, headed = headed)
  local_mocked_bindings(
    sc_browser_unavailable_reason = function() NULL,
    sc_launch_chrome = function(headed) chrome,
    sc_wait_pass = function(session, url, timeout) a_clearance(url),
    .env = env)
  list(chrome = chrome,
       s = browser_session("https://example.org/", quiet = TRUE, headed = headed, ...))
}

test_that("a session is a browser kept open on a host, until closed", {
  x <- local_session()
  s <- x$s
  expect_s3_class(s, "safeconduct_session")
  expect_identical(s$host, "example.org")
  expect_s3_class(s$clearance, "safeconduct_clearance")
  expect_output(print(s), "example.org': a Chrome window escorting")
  expect_false(x$chrome$is_closed())
  close(s)
  expect_true(x$chrome$is_closed())
  expect_output(print(s), "closed")
  close(s)
  expect_error(fetch("https://example.org/x.zip", withr::local_tempfile(), session = s),
               "has been closed", class = "safeconduct_unavailable")
  expect_error(read_page("https://example.org/x.json", session = s),
               class = "safeconduct_unavailable")
  expect_error(read_page("https://example.org/x.json", session = "nope"),
               "browser_session")
})

test_that("a session downloads and reads in its browser without probing", {
  x <- local_session(headed = FALSE)
  local_mocked_bindings(sc_probe = function(...) stop("no probe wanted"))
  got <- NULL
  local_mocked_bindings(sc_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
    got <<- list(session = session, url = url); invisible(destfile)
  })
  fetch("https://example.org/x.zip", withr::local_tempfile(), session = x$s, quiet = TRUE)
  expect_identical(got$session, x$chrome$session)
  expect_identical(got$url, "https://example.org/x.zip")

  r <- read_page("https://example.org/feed.json", session = x$s)
  expect_identical(rawToChar(r$content), '{"a":1}')
  expect_output(print(x$s), "headless")
  expect_false(x$chrome$is_closed())

  expect_error(read_page("https://other.example/feed.json", session = x$s),
               "own host")
  close(x$s)
})

test_that("a session passes the check again when the host asks", {
  n <- 0L
  reads <- function(expr) {
    n <<- n + 1L
    if (n == 1L) read_result(403, block_page,
                             headers = c(cf_headers, list(`cf-mitigated` = "challenge")))
    else read_result()
  }
  x <- local_session(fake_session(read = reads))
  passed <- 0L
  local_mocked_bindings(sc_wait_pass = function(session, url, timeout) {
    passed <<- passed + 1L
    expect_identical(url, "https://example.org/")
    sc_clearance(url, "UA", "cf_clearance=new")
  })
  msgs <- capture_messages(r <- read_page("https://example.org/feed.json", session = x$s))
  expect_match(paste(msgs, collapse = ""), "again")
  expect_identical(passed, 1L)
  expect_identical(r$status_code, 200L)
  expect_identical(x$s$clearance$cookie, "cf_clearance=new")
  close(x$s)
})

test_that("a session opens its browser again when it has gone away", {
  x <- local_session()
  x$chrome$session$active <- FALSE
  fresh <- fake_chrome(fake_session(read = read_result()), headed = TRUE)
  opened <- list()
  local_mocked_bindings(sc_launch_chrome = function(headed) {
    opened <<- c(opened, headed); fresh
  })
  msgs <- capture_messages(r <- read_page("https://example.org/feed.json", session = x$s))
  expect_match(paste(msgs, collapse = ""), "gone away.*window closed.*opening another")
  expect_identical(opened, list(TRUE))
  expect_true(x$chrome$is_closed())
  expect_identical(x$s$chrome$session, fresh$session)
  expect_identical(r$status_code, 200L)
  expect_true(isTRUE(x$s$open))
  # Quietly, when asked.
  fresh$session$active <- FALSE
  expect_silent(read_page("https://example.org/feed.json", session = x$s, quiet = TRUE))
  close(x$s)
})

test_that("a session opens its browser again when it goes away mid-read", {
  first <- fake_session(read = function(expr) {
    first$active <- FALSE
    stop("Session and underlying target have been closed.")
  })
  x <- local_session(first, headed = FALSE)
  fresh <- fake_chrome(fake_session(read = read_result()), headed = FALSE)
  local_mocked_bindings(sc_launch_chrome = function(headed) fresh)
  msgs <- capture_messages(r <- read_page("https://example.org/feed.json", session = x$s))
  expect_match(paste(msgs, collapse = ""), "gone away; opening another")
  expect_false(grepl("window", paste(msgs, collapse = "")))
  expect_identical(r$status_code, 200L)
  expect_identical(x$s$chrome$session, fresh$session)
  # Gone again during the retry: the error is reported, not retried for ever.
  fresh$session$active <- FALSE
  again <- fake_chrome(fake_session(read = function(expr) {
    again$session$active <- FALSE
    stop("Session has been closed.")
  }), headed = FALSE)
  local_mocked_bindings(sc_launch_chrome = function(headed) again)
  expect_error(read_page("https://example.org/feed.json", session = x$s, quiet = TRUE),
               "browser went away", class = "safeconduct_download_error")
  close(x$s)
})

test_that("a session that cannot open its browser again says why", {
  x <- local_session()
  x$chrome$session$active <- FALSE
  local_mocked_bindings(sc_browser_unavailable_reason = function() "no Chrome here")
  expect_error(read_page("https://example.org/feed.json", session = x$s, quiet = TRUE),
               "no Chrome here", class = "safeconduct_unavailable")
  local_mocked_bindings(sc_browser_unavailable_reason = function() NULL,
                        sc_wait_pass = function(session, url, timeout)
                          stop(sc_error("safeconduct_not_passed", "nope", reason = "blocked")))
  expect_error(read_page("https://example.org/feed.json", session = x$s, quiet = TRUE),
               class = "safeconduct_not_passed")
  # A session the user closed is not reopened.
  close(x$s)
  expect_error(read_page("https://example.org/feed.json", session = x$s),
               "has been closed", class = "safeconduct_unavailable")
})

test_that("a session downloads in a browser opened again", {
  x <- local_session(headed = FALSE)
  x$chrome$session$active <- FALSE
  fresh <- fake_chrome(fake_session(events = list(dl_event("completed", "g1", 4, 4))),
                       headed = FALSE)
  local_mocked_bindings(sc_launch_chrome = function(headed) fresh)
  dest <- withr::local_tempfile()
  msgs <- capture_messages(fetch("https://example.org/x.zip", dest, session = x$s))
  expect_match(paste(msgs, collapse = ""), "opening another")
  expect_identical(readLines(dest), "data")
  close(x$s)
})

test_that("a session's user agent is set before the check and kept through a reopen", {
  first <- fake_session(read = read_result(), ua = "Mozilla/5.0 (fake) HeadlessChrome/1")
  x <- local_session(first, headed = FALSE, useragent = TRUE)
  expect_identical(first$calls, "ua:Mozilla/5.0 (fake) Chrome/1")
  expect_true(x$s$useragent)
  first$active <- FALSE
  fresh <- fake_chrome(fake_session(read = read_result(),
                                    ua = "Mozilla/5.0 (fake) HeadlessChrome/1"), headed = FALSE)
  local_mocked_bindings(sc_launch_chrome = function(headed) fresh)
  r <- read_page("https://example.org/feed.json", session = x$s, quiet = TRUE)
  expect_identical(r$status_code, 200L)
  expect_identical(fresh$session$calls[[1]], "ua:Mozilla/5.0 (fake) Chrome/1")
  close(x$s)

  x <- local_session(fake_session(read = read_result()), headed = FALSE, useragent = "Mine/2")
  expect_identical(x$chrome$session$calls, "ua:Mine/2")
  expect_identical(x$s$useragent, "Mine/2")
  close(x$s)
  expect_error(browser_session("https://example.org/", useragent = 3), "one string")
})

test_that("a session's headless download that does not pass its check moves to a window", {
  chromes <- list()
  local_mocked_bindings(
    sc_display_available = function() TRUE,
    sc_browser_unavailable_reason = function() NULL,
    sc_launch_chrome = function(headed) {
      ch <- fake_chrome(headed = headed)
      chromes[[length(chromes) + 1L]] <<- ch
      ch
    },
    sc_wait_pass = function(session, url, timeout) a_clearance(url),
    sc_browser_download = function(session, url, destfile, quiet, timeout, headed, ...) {
      if (!headed) stop(sc_error("safeconduct_not_passed", "still checking",
                                 reason = "timeout"))
      writeLines("x", destfile); invisible(destfile)
    })
  s <- browser_session("https://example.org/", quiet = TRUE)
  expect_false(s$headed)
  expect_identical(s$mode, "auto")
  dest <- withr::local_tempfile()
  msgs <- paste(capture_messages(fetch("https://example.org/x.zip", dest, session = s)),
                collapse = "")
  expect_match(msgs, "did not get through the check at the file's URL on 'example.org'; opening a window for the session")
  expect_true(s$headed)
  expect_true(chromes[[1]]$is_closed())
  expect_false(chromes[[2]]$is_closed())
  expect_identical(readLines(dest), "x")

  # From then on the session's window is used, and reopened as a window.
  fetch("https://example.org/y.zip", dest, quiet = TRUE, session = s)
  expect_length(chromes, 2)
  close(s)
  expect_true(chromes[[2]]$is_closed())

  # A session headless by choice is not moved to a window.
  s <- browser_session("https://example.org/", quiet = TRUE, headed = FALSE)
  expect_error(fetch("https://example.org/x.zip", dest, quiet = TRUE, session = s),
               class = "safeconduct_not_passed")
  expect_false(s$headed)
  close(s)
})
