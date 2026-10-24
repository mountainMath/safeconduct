local_session <- function(session = fake_session(read = read_result()),
                          headed = TRUE, env = parent.frame()) {
  chrome <- fake_chrome(session, headed = headed)
  local_mocked_bindings(
    bf_browser_unavailable_reason = function() NULL,
    bf_launch_chrome = function(headed) chrome,
    bf_wait_pass = function(session, url, timeout) a_clearance(url),
    .env = env)
  withr::local_options(safeconduct.headed = headed, .local_envir = env)
  list(chrome = chrome, s = browser_session("https://example.org/", quiet = TRUE))
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
  local_mocked_bindings(bf_probe = function(...) stop("no probe wanted"))
  got <- NULL
  local_mocked_bindings(bf_browser_download = function(session, url, destfile, quiet, timeout) {
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
  local_mocked_bindings(bf_wait_pass = function(session, url, timeout) {
    passed <<- passed + 1L
    expect_identical(url, "https://example.org/")
    bf_clearance(url, "UA", "cf_clearance=new")
  })
  msgs <- capture_messages(r <- read_page("https://example.org/feed.json", session = x$s))
  expect_match(paste(msgs, collapse = ""), "again")
  expect_identical(passed, 1L)
  expect_identical(r$status_code, 200L)
  expect_identical(x$s$clearance$cookie, "cf_clearance=new")
  close(x$s)
})
