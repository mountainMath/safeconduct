# A browser kept open on a host.
#
# `fetch()` and `fetch_memory()` open and close a browser per call, which is
# right for a file now and then and wrong for a feed read every few minutes
# all evening: a window per read, and a check per window. A session is the
# browser kept open, on a page of the host, so that reads are made by that
# page and files downloaded in that browser. It is an environment, so that
# the clearance can be refreshed in place when the host asks the browser for
# a check again.

#' Keep a browser open on a host
#'
#' Opens a Chrome with a throwaway profile on the host, lets any browser
#' check run, and keeps it open, for [fetch_memory()] and [fetch()] to use
#' through their `session` argument. Close it with [close()]; a session that
#' is not closed lives until the R session ends.
#'
#' @inheritParams clearance
#' @param url A URL on the host. The browser is opened on the host's root.
#' @return An object of class `safeconduct_session`. Its `clearance` is what
#'   [clearance()] would return. Errors as [fetch()] does.
#' @examples
#' \dontrun{
#' s <- browser_session("https://results.vancouver.ca/")
#' r <- fetch_memory("https://results.vancouver.ca/data/arearesults.json",
#'                   session = s)
#' close(s)
#' }
#' @export
browser_session <- function(url, quiet = FALSE, wait = NULL, headed = NULL) {
  chrome <- bf_open_browser(url, quiet = quiet, wait = wait, headed = headed,
                            hints = bf_hints_on(quiet))
  s <- new.env(parent = emptyenv())
  s$host <- bf_url_host(url)
  s$root <- bf_host_root(url)
  s$chrome <- chrome
  s$clearance <- chrome$clearance
  s$headed <- chrome$headed
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
  cat("<safeconduct browser session on ", x$host, ", ",
      if (x$headed) "a window" else "headless", ", opened ",
      format(x$time, "%Y-%m-%d %H:%M:%S"),
      if (!isTRUE(x$open)) ", closed", ">\n", sep = "")
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
  r <- bf_browser_read(s$chrome$session, url, timeout = timeout)
  if (bf_needs_browser(bf_describe_response(r$status_code, r$headers, r$content))) {
    bf_message(quiet, "'", s$host, "' is asking the browser for a check again; ",
               "passing it.")
    s$clearance <- bf_wait_pass(s$chrome$session, s$root, s$wait)
    r <- bf_browser_read(s$chrome$session, url, timeout = timeout)
  }
  r
}
