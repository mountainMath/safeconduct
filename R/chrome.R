# Running Chrome.
#
# What was tried against Cloudflare's managed challenge, and what holds
# (October 2026):
#
#   * Headless Chrome never passes. Whatever the user agent says, the check
#     falls through to its interactive form, a "Verify you are human" box, and
#     sits there. Simulating a click on that box is not something this package
#     does: it is the one thing the check exists to ask a person for.
#   * A visible Chrome window launched with an empty, throwaway profile passes
#     in one to two seconds with nothing clicked -- usually. Cloudflare
#     escalates: after a dozen challenges in half an hour from one address it
#     showed the box to the window too. So the window is left on screen with a
#     message asking the user to click the box if it shows, and the wait is
#     long enough for them to. Clicked, the box yields a clearance like any
#     other.
#   * Chrome is launched directly, not through `chromote::Chrome$new()`, which
#     always passes `--headless`. `--remote-debugging-port=0` makes Chrome pick
#     a free port and write it to `DevToolsActivePort` in the profile
#     directory, which `chromote::ChromeRemote` then connects to. Nothing of
#     the user's own Chrome profile is touched: the profile is a fresh temp
#     directory, removed when the window closes.
#   * A new `ChromoteSession` applies a device-metrics override of 992 by
#     1323 pixels, chromote's stand-in viewport for headless work. In a real
#     window that makes the page lay itself out in an imaginary viewport
#     taller than the window, so the check page's centred card sat below the
#     fold whatever the window's size. The override is cleared right after
#     the session is made, and the viewport is then the window's.
#   * Without a display Chrome is launched headless through chromote's own
#     launcher. As of writing that does not pass the check, but it is the only
#     thing left to try there, and the check may change.

#' Can this package open a browser here?
#'
#' @return `TRUE`, or `FALSE` with a `reason` attribute saying what is
#'   missing: the `safeconduct.browser` option set to `FALSE`, no Chrome or
#'   Chromium found (set the `CHROMOTE_CHROME` environment variable to its
#'   path), or no display to show a window on and no headless fallback
#'   wanted.
#' @examples
#' browser_available()
#' attr(browser_available(), "reason")
#' @export
browser_available <- function() {
  reason <- bf_browser_unavailable_reason()
  structure(is.null(reason), reason = reason)
}

bf_browser_unavailable_reason <- function() {
  if (!isTRUE(getOption("safeconduct.browser", TRUE)))
    return("the `safeconduct.browser` option is FALSE")
  chrome <- tryCatch(suppressMessages(chromote::find_chrome()),
                     error = function(e) NULL)
  if (is.null(chrome) || !nzchar(chrome))
    return(paste0("no Chrome or Chromium was found (set the `CHROMOTE_CHROME` ",
                  "environment variable to its path)"))
  NULL
}

# Can a window be shown? Always on macOS and Windows; on Linux only under a
# display server. The `safeconduct.headed` option overrides the guess.
bf_display_available <- function() {
  opt <- getOption("safeconduct.headed", NULL)
  if (!is.null(opt)) return(isTRUE(opt))
  sysname <- Sys.info()[["sysname"]]
  if (sysname %in% c("Darwin", "Windows")) return(TRUE)
  nzchar(Sys.getenv("DISPLAY")) || nzchar(Sys.getenv("WAYLAND_DISPLAY"))
}

# How big the window is: 1000 by 800 pixels, or what the `safeconduct.window`
# option says, a `c(width, height)` in pixels or `"maximized"`. (Before the
# viewport override above was found, the card was cut off at every size and
# the window was opened maximized; that is no longer needed by default.)
bf_window_args <- function() {
  size <- getOption("safeconduct.window", c(1000L, 800L))
  if (identical(size, "maximized")) return("--start-maximized")
  if (!is.numeric(size) || length(size) != 2L || any(is.na(size)) || any(size < 200))
    stop("`options(safeconduct.window)` must be \"maximized\" or two pixel ",
         "sizes, c(width, height).", call. = FALSE)
  paste0("--window-size=", paste(as.integer(size), collapse = ","))
}

# Chrome's `DevToolsActivePort` file: the port on the first line, written once
# the DevTools server is listening.
bf_devtools_port <- function(profile, timeout = 15) {
  path <- file.path(profile, "DevToolsActivePort")
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (file.exists(path)) {
      port <- suppressWarnings(as.integer(readLines(path, n = 1L, warn = FALSE)))
      if (length(port) == 1L && !is.na(port) && port > 0L) return(port)
    }
    Sys.sleep(0.2)
  }
  stop("Chrome did not start a DevTools server within ", timeout, " seconds.",
       call. = FALSE)
}

# Start Chrome and attach a DevTools session to it. Returns a list with the
# `session` (a `chromote::ChromoteSession`), `headed` (whether a window is on
# screen) and `close()`, which shuts everything down and removes the profile.
bf_launch_chrome <- function(headed = bf_display_available()) {
  if (!headed) {
    chrome <- chromote::Chromote$new()
    session <- chromote::ChromoteSession$new(parent = chrome)
    return(list(
      session = session, headed = FALSE,
      close = function() try(chrome$close(), silent = TRUE)))
  }

  profile <- tempfile("safeconduct-chrome-")
  dir.create(profile)
  proc <- processx::process$new(
    chromote::find_chrome(),
    c("--remote-debugging-port=0",
      paste0("--user-data-dir=", profile),
      "--no-first-run", "--no-default-browser-check",
      "--disable-extensions", "--disable-sync",
      bf_window_args(),
      "about:blank"),
    stdout = NULL, stderr = NULL, supervise = TRUE, cleanup = TRUE)
  state <- new.env()
  # Ask Chrome to quit over the protocol and give it a moment, so that the
  # socket closes from Chrome's side; killing it mid-connection makes the
  # websocket library print an error. Then make sure.
  close <- function() {
    if (!is.null(state$chrome)) {
      # Chrome answers and drops the connection; chromote may report that as
      # an error, which is the expected outcome here.
      try(suppressWarnings(state$session$Browser$close()), silent = TRUE)
      state$chrome <- NULL
    }
    try(proc$wait(timeout = 2000), silent = TRUE)
    try(if (proc$is_alive()) proc$kill(), silent = TRUE)
    unlink(profile, recursive = TRUE)
  }
  tryCatch({
    port <- bf_devtools_port(profile)
    remote <- chromote::ChromeRemote$new(host = "127.0.0.1", port = port)
    state$chrome <- chromote::Chromote$new(browser = remote)
    state$session <- chromote::ChromoteSession$new(parent = state$chrome)
    invisible(state$session$Emulation$clearDeviceMetricsOverride())
  }, error = function(e) {
    close()
    stop(e)
  })
  list(session = state$session, headed = TRUE, close = close)
}

# The CDP cookie list as one `Cookie:` header value.
bf_cookie_header <- function(cookies) {
  paste(vapply(cookies, function(ck) paste0(ck$name, "=", ck$value),
               character(1)),
        collapse = "; ")
}

# Bringing the check into view.
#
# The challenge page puts the check -- the spinner, or the "Verify you are
# human" box -- in one element, surrounded by a heading, a footer and a good
# deal of margin, and in a window of the size used here the box can sit below
# the fold. While the check is pending, that element is pinned to the top left
# corner of the window and enlarged, and everything that is not it, inside it
# or above it is made invisible. The styles are set on the elements themselves
# through the CSSOM, because an added `<style>` element is subject to the
# page's Content Security Policy and was blocked on the one real check page
# tried, while element styles are not. It is a presentational change, as a
# user stylesheet or the browser's zoom would be: nothing in the element is
# read, filled or clicked, and the check's widget runs untouched inside it.
# It is opt-in: the `safeconduct.focus` option names the selector (`.card` on
# the pages met so far) and `safeconduct.zoom` the enlargement. Verified on a
# local mock page with a blocking CSP; the one real run with it on ended with
# the browser going away before the download, for reasons not established, so
# it is not on by default.
bf_focus_script <- function(selector = getOption("safeconduct.focus", NULL),
                            zoom = getOption("safeconduct.zoom", 1.5)) {
  if (isFALSE(selector) || is.null(selector)) return(NULL)
  sprintf(
    "(function(sel, zoom){
      if (window.__safeconductFocused) return 'kept';
      var el = document.querySelector(sel);
      if (!el) return 'absent';
      window.__safeconductFocused = true;
      var set = function(n, prop, value) { n.style.setProperty(prop, value, 'important'); };
      set(document.documentElement, 'overflow', 'hidden');
      set(document.body, 'margin', '0');
      set(el, 'position', 'fixed');
      set(el, 'top', '10px');
      set(el, 'left', '10px');
      set(el, 'margin', '0');
      set(el, 'max-width', 'none');
      set(el, 'width', 'calc(100vw / ' + zoom + ' - 20px)');
      set(el, 'zoom', zoom);
      set(el, 'z-index', '2147483647');
      var keep = new Set();
      for (var n = el; n; n = n.parentElement) keep.add(n);
      el.querySelectorAll('*').forEach(function(n) { keep.add(n); });
      document.body.querySelectorAll('*').forEach(function(n) {
        if (!keep.has(n)) set(n, 'visibility', 'hidden');
      });
      return 'focused';
    })(%s, %s)", encodeString(selector, quote = '"'), zoom)
}

bf_focus <- function(session, script) {
  if (is.null(script)) return(invisible(NULL))
  try(session$Runtime$evaluate(script), silent = TRUE)
  invisible(NULL)
}

# The cookie that says the check was passed. Cloudflare's; add others beside
# it as they are found.
bf_clearance_cookies <- c("cf_clearance")

# Load `url` in the session and wait for a clearance cookie. Returns the
# clearance -- the browser's user-agent string and its cookies for the host --
# or `NULL` if none arrived within `timeout` seconds.
bf_wait_clearance <- function(session, url, timeout) {
  invisible(session$Network$enable())
  invisible(session$Page$navigate(url, wait_ = FALSE))
  focus <- bf_focus_script()
  deadline <- Sys.time() + timeout
  repeat {
    Sys.sleep(0.5)
    bf_focus(session, focus)
    cookies <- tryCatch(session$Network$getCookies(urls = list(url))$cookies,
                        error = function(e) list())
    names <- vapply(cookies, function(ck) ck$name %||% "", character(1))
    if (any(bf_clearance_cookies %in% names)) {
      ua <- session$Runtime$evaluate("navigator.userAgent")$result$value
      return(bf_clearance(url, ua, bf_cookie_header(cookies)))
    }
    if (Sys.time() > deadline) return(NULL)
  }
}

bf_clearance <- function(url, useragent, cookie) {
  structure(list(host = bf_url_host(url), useragent = useragent,
                 cookie = cookie, time = Sys.time()),
            class = "safeconduct_clearance")
}

#' @export
print.safeconduct_clearance <- function(x, ...) {
  cat("<safeconduct clearance for ", x$host, ", ",
      format(x$time, "%Y-%m-%d %H:%M:%S"), ">\n", sep = "")
  cat("  user agent: ", x$useragent, "\n", sep = "")
  cat("  cookies:    ", paste(sub("=.*", "", strsplit(x$cookie, "; ")[[1]]),
                              collapse = ", "), "\n", sep = "")
  invisible(x)
}

# Download `url` through the browser into `destfile`. The file is written
# beside its destination under Chrome's own name for it and renamed on
# completion, so a transfer that fails leaves nothing at `destfile`. A
# transfer that makes no progress for two minutes, or runs past `timeout`
# seconds, is cancelled and raises a `safeconduct_download_error`.
#
# `Browser.setDownloadBehavior` with `allowAndName` lands the file as
# `<dir>/<guid>` and reports progress through `Browser.downloadProgress`.
# Events only reach R while a protocol call is in flight, so the wait loop
# makes a cheap synchronous call each turn.
bf_browser_download <- function(session, url, destfile, quiet = FALSE,
                                timeout = 3600) {
  dir <- normalizePath(dirname(destfile), mustWork = TRUE)
  invisible(session$Browser$setDownloadBehavior(
    behavior = "allowAndName", downloadPath = dir, eventsEnabled = TRUE))
  progress <- NULL
  unregister <- session$Browser$downloadProgress(
    callback_ = function(ev) progress <<- ev, wait_ = FALSE)
  on.exit(try(unregister(), silent = TRUE), add = TRUE)
  invisible(session$Page$navigate(url, wait_ = FALSE))

  started <- Sys.time()
  last_bytes <- -1
  last_change <- started
  last_report <- started
  fail <- function(why) {
    guid <- progress$guid
    if (!is.null(guid)) {
      try(session$Browser$cancelDownload(guid = guid), silent = TRUE)
      unlink(list.files(dir, pattern = paste0("^", guid), full.names = TRUE))
    }
    stop(bf_error("safeconduct_download_error", paste0(
      "Could not download '", url, "' in the browser: ", why)))
  }
  repeat {
    Sys.sleep(0.5)
    tryCatch(session$Browser$getVersion(),
             error = function(e) fail("the browser went away."))
    now <- Sys.time()
    state <- progress$state %||% "pending"
    if (identical(state, "completed")) break
    if (identical(state, "canceled")) fail("the browser cancelled it.")
    received <- progress$receivedBytes %||% 0
    if (received != last_bytes) { last_bytes <- received; last_change <- now }
    if (now - last_change > 120) fail("no data arrived for two minutes.")
    if (now - started > timeout) fail(paste0("it did not finish in ", timeout,
                                             " seconds."))
    if (!quiet && now - last_report > 10 && received > 0) {
      total <- progress$totalBytes %||% 0
      bf_message(quiet, sprintf("  %.0f MB%s", received / 1e6,
        if (total > 0) sprintf(" of %.0f", total / 1e6) else ""))
      last_report <- now
    }
  }
  from <- file.path(dir, progress$guid)
  if (!file.exists(from) || file.info(from)$size == 0)
    fail("the browser reported it complete but there is no file.")
  if (!file.rename(from, destfile)) {
    unlink(from)
    fail("the file could not be moved into place.")
  }
  invisible(destfile)
}
