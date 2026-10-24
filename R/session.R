# A browser kept open on a host.
#
# `fetch()` and `read_page()` open and close a browser per call, which is
# right for a file now and then and wrong for a feed read every few minutes
# all evening: a window per read, and a check per window. A session is the
# browser kept open, on a page of the host, so that reads are made by that
# page and files downloaded in that browser. It is an environment, so that
# the clearance can be refreshed in place when the host asks the browser for
# a check again, and the browser replaced in place when it has gone away:
# a window someone closed, or a Chrome that crashed. A session is closed only
# by `close()`; a browser that went away is opened again at the next read,
# the way it was opened before, with a message saying so.

#' Keep a browser open on a host
#'
#' Opens a Chrome with a throwaway profile on the host, lets any browser
#' check run, and keeps it open, for [read_page()] and [fetch()] to use
#' through their `session` argument. Close it with [close()]; a session that
#' is not closed lives until the R session ends. Should the browser go away
#' meanwhile -- its window closed, say -- the next read or download opens
#' another, the same way, and says so.
#'
#' @inheritParams clearance
#' @param url A URL on the host. The browser is opened on the host's root.
#'   `headed` and `useragent` hold for the session's life, through any
#'   reopening of its browser.
#' @return An object of class `safeconduct_session`. Its `clearance` is what
#'   [clearance()] would return. Errors as [fetch()] does.
#' @examples
#' \dontrun{
#' s <- browser_session("https://results.vancouver.ca/")
#' r <- read_page("https://results.vancouver.ca/data/arearesults.json",
#'                session = s)
#' close(s)
#' }
#' @export
browser_session <- function(url, quiet = FALSE, wait = NULL, headed = "auto",
                            useragent = TRUE) {
  chrome <- bf_open_browser(url, quiet = quiet, wait = wait, headed = headed,
                            useragent = useragent, hints = bf_hints_on(quiet))
  s <- new.env(parent = emptyenv())
  s$host <- bf_url_host(url)
  s$root <- bf_host_root(url)
  s$chrome <- chrome
  s$clearance <- chrome$clearance
  s$headed <- chrome$headed
  s$mode <- chrome$mode
  s$useragent <- chrome$useragent
  s$wait <- chrome$wait
  s$time <- Sys.time()
  s$open <- TRUE
  reg.finalizer(s, function(e) if (isTRUE(e$open)) try(e$chrome$close(), silent = TRUE),
                onexit = TRUE)
  class(s) <- "safeconduct_session"
  s
}

#' @export
close.safeconduct_session <- function(con, ...) {
  if (isTRUE(con$open)) {
    con$chrome$close()
    con$open <- FALSE
  }
  invisible(NULL)
}

#' @export
print.safeconduct_session <- function(x, ...) {
  cat("<safeconduct session on '", x$host, "': ",
      if (x$headed) "a Chrome window" else "headless Chrome",
      if (isTRUE(x$open)) " escorting requests since " else ", opened ",
      format(x$time, "%Y-%m-%d %H:%M:%S"),
      if (!isTRUE(x$open)) ", now closed", ">\n", sep = "")
  invisible(x)
}

bf_session_check <- function(session) {
  if (!inherits(session, "safeconduct_session"))
    stop("`session` must be a browser session from `browser_session()`.",
         call. = FALSE)
  if (!isTRUE(session$open))
    stop(bf_error("safeconduct_unavailable", paste0(
      "The browser session on '", session$host, "' has been closed.")))
  session
}

# Is the session's browser still there? chromote marks its session inactive
# when the tab is closed, when Chrome quits and when the connection drops,
# but it learns of these only while a protocol call is in flight, so one
# cheap call is made first; on a tab that is gone it fails.
bf_session_alive <- function(s) {
  ses <- s$chrome$session
  tryCatch({ ses$Browser$getVersion(); isTRUE(ses$is_active()) },
           error = function(e) FALSE)
}

# Open the session's browser again, the way it was opened before (a window
# if a window got through, headless otherwise), and pass the check anew.
# `why` replaces the message for a reopening that is not about the browser
# having gone away: a window wanted for a check headless Chrome did not pass.
bf_session_reopen <- function(s, quiet = FALSE, why = NULL) {
  bf_message(quiet, why %||% paste0(
    "The browser on '", s$host, "' has gone away",
    if (s$headed) " (was its window closed?)", "; opening another."))
  try(s$chrome$close(), silent = TRUE)
  chrome <- bf_open_browser(s$root, quiet = quiet, wait = s$wait,
                            headed = s$headed, useragent = s$useragent,
                            hints = FALSE)
  s$chrome <- chrome
  s$clearance <- chrome$clearance
  s$headed <- chrome$headed
  invisible(s)
}

# Run `fn(chrome)` in the session's browser, opening the browser again first
# if it has gone away, and once more if it goes away during `fn` -- the
# window closed mid-read. A second disappearance is reported as the error
# it is.
bf_session_do <- function(session, fn, quiet = FALSE) {
  s <- bf_session_check(session)
  if (!bf_session_alive(s)) bf_session_reopen(s, quiet)
  tryCatch(fn(s$chrome), safeconduct_error = function(e) {
    if (!identical(e$reason, "gone")) stop(e)
    bf_session_reopen(s, quiet)
    fn(s$chrome)
  })
}

# Read in the session's browser. The page reads same-origin only, so the URL
# has to be on the session's host. A response that is a check or a block
# means the host wants the browser checked again: the check is passed at the
# root and the read made once more.
bf_session_read <- function(session, url, quiet = FALSE, timeout = 120) {
  s <- bf_session_check(session)
  if (!identical(bf_url_host(url), s$host))
    stop("The browser session is on '", s$host, "', and a session reads only ",
         "from its own host; '", url, "' is on '", bf_url_host(url), "'.",
         call. = FALSE)
  bf_session_do(s, quiet = quiet, function(chrome) {
    r <- bf_browser_read(chrome$session, url, timeout = timeout)
    if (bf_needs_browser(bf_describe_response(r$status_code, r$headers, r$content))) {
      bf_message(quiet, "'", s$host, "' is asking the escort for a check again; ",
                 "passing it.")
      s$clearance <- bf_wait_pass(chrome$session, s$root, s$wait)
      r <- bf_browser_read(chrome$session, url, timeout = timeout)
    }
    r
  })
}
