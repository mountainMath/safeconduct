# Running Chrome.
#
# What was tried against Cloudflare, and what holds (October 2026):
#
#   * Headless Chrome under its own name does not pass a managed challenge:
#     the check falls through to its interactive form, a "Verify you are
#     human" box, and sits there. Simulating a click on that box is not
#     something this package does: it is the one thing the check exists to
#     ask a person for. Headless Chrome is not held up by a firewall rule
#     that blocks clients which do not look like a browser, and it has been
#     seen to pass a lighter, non-interactive check. Hence the "auto" mode:
#     headless first, a window only when the headless run ends on a check or
#     a block, so that a window is shown only when someone may need to click.
#     With `useragent = TRUE` (below) headless Chrome passed the managed
#     challenge at www12.statcan.gc.ca on 2026-10-24 with nothing clicked, at
#     the root and again at the file's URL, and downloaded 310 MB; the
#     clearance did not transfer to curl. How long that holds is Cloudflare's
#     to decide.
#   * A visible Chrome window launched with an empty, throwaway profile passes
#     in one to two seconds with nothing clicked -- usually. Cloudflare
#     escalates: after a dozen challenges in half an hour from one address it
#     showed the box to the window too. So the window is left on screen with a
#     message asking the user to click the box if it shows, and the wait is
#     long enough for them to. Clicked, the box yields a clearance like any
#     other.
#   * Whether the browser got through is read off the main frame's document
#     responses (`Network.responseReceived`), not off a cookie: a challenge is
#     a `403` with `cf-mitigated: challenge` followed, once passed, by the
#     page; a block is a `403` whose body is Cloudflare's block page and
#     nothing follows it; anything else is the host answering the browser.
#     A host that blocks plain clients but never challenges a browser hands
#     out no `cf_clearance`, so waiting for the cookie would wait for ever.
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
#   * Headless Chrome is launched through chromote's own launcher. Its
#     user-agent string says `HeadlessChrome`. The Vancouver hosts
#     (results.vancouver.ca, webtransfer.vancouver.ca) block headless Chrome
#     on that word alone: the same browser with `Chrome/` in place of
#     `HeadlessChrome/` is served, while curl is blocked whatever it says;
#     and with the word gone headless Chrome passed StatCan's managed
#     challenge too. So by default (`useragent = TRUE`) the word is dropped
#     and nothing else; the version and platform in the string stay what
#     they are. `useragent = NULL` leaves Chrome's own string.

#' Can this package open a browser here?
#'
#' @return `TRUE`, or `FALSE` with a `reason` attribute saying what is
#'   missing: the `safeconduct.browser` option set to `FALSE`, or no Chrome
#'   or Chromium found (set the `CHROMOTE_CHROME` environment variable to
#'   its path).
#' @examples
#' browser_available()
#' attr(browser_available(), "reason")
#' @export
browser_available <- function() {
  reason <- sc_browser_unavailable_reason()
  structure(is.null(reason), reason = reason)
}

sc_browser_unavailable_reason <- function() {
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
# display server.
sc_display_available <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (sysname %in% c("Darwin", "Windows")) return(TRUE)
  nzchar(Sys.getenv("DISPLAY")) || nzchar(Sys.getenv("WAYLAND_DISPLAY"))
}

# How the browser is run: "headed" (a window), "headless", or "auto"
# (headless first, a window if that does not get through). The `headed`
# argument of the public functions -- `"auto"` by default, `TRUE` or `FALSE`
# -- decides. "auto" without a display is plain headless.
sc_headed_mode <- function(headed = "auto") {
  headed <- headed %||% "auto"
  if (identical(headed, "auto"))
    return(if (sc_display_available()) "auto" else "headless")
  if (isTRUE(headed)) return("headed")
  if (isFALSE(headed)) return("headless")
  stop("`headed` must be \"auto\", TRUE or FALSE.", call. = FALSE)
}

# How big the window is: 1000 by 800 pixels, or what the `safeconduct.window`
# option says, a `c(width, height)` in pixels or `"maximized"`. (Before the
# viewport override above was found, the card was cut off at every size and
# the window was opened maximized; that is no longer needed by default.)
sc_window_args <- function() {
  size <- getOption("safeconduct.window", c(1000L, 800L))
  if (identical(size, "maximized")) return("--start-maximized")
  if (!is.numeric(size) || length(size) != 2L || any(is.na(size)) || any(size < 200))
    stop("`options(safeconduct.window)` must be \"maximized\" or two pixel ",
         "sizes, c(width, height).", call. = FALSE)
  paste0("--window-size=", paste(as.integer(size), collapse = ","))
}

# Chrome's `DevToolsActivePort` file: the port on the first line, written once
# the DevTools server is listening.
sc_devtools_port <- function(profile, timeout = 15) {
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
sc_launch_chrome <- function(headed = TRUE) {
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
      sc_window_args(),
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
    port <- sc_devtools_port(profile)
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

# The user-agent string the browser announces, from the `useragent` argument
# of the public functions: `TRUE` (the default) drops the word `Headless`
# from Chrome's own, `NULL` leaves it, a string is used as given. The override is set through
# the protocol before the first navigation and holds for the browser's life;
# it is what the clearance then reports too, so curl under the clearance
# says the same.
sc_useragent_check <- function(useragent) {
  if (is.null(useragent) || isTRUE(useragent)) return(useragent)
  if (is.character(useragent) && length(useragent) == 1L && !is.na(useragent) &&
      nzchar(useragent)) return(useragent)
  stop("`useragent` must be NULL, TRUE or one string.", call. = FALSE)
}

sc_set_useragent <- function(session, useragent) {
  useragent <- sc_useragent_check(useragent)
  if (is.null(useragent)) return(invisible(NULL))
  if (isTRUE(useragent)) {
    own <- session$Runtime$evaluate("navigator.userAgent")$result$value
    useragent <- sub("HeadlessChrome/", "Chrome/", own, fixed = TRUE)
    if (identical(useragent, own)) return(invisible(own))
  }
  invisible(session$Network$setUserAgentOverride(userAgent = useragent))
  invisible(useragent)
}

# The CDP cookie list as one `Cookie:` header value.
sc_cookie_header <- function(cookies) {
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
sc_focus_script <- function(selector = getOption("safeconduct.focus", NULL),
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

sc_focus <- function(session, script) {
  if (is.null(script)) return(invisible(NULL))
  try(session$Runtime$evaluate(script), silent = TRUE)
  invisible(NULL)
}

# Watching what the main frame is served.
#
# Events only reach R while a protocol call is in flight, so the loops below
# make a cheap synchronous call each turn, which also notices a browser that
# has gone away. `sc_watch_documents()` records the latest document response
# of the main frame -- the challenge iframe's documents are not it -- and
# `sc_document_kind()` says what it is:
#
#   "challenge"  the check is running (or waiting for a click)
#   "block"      Cloudflare's block page: the browser is refused too
#   "redirect"   a 3xx; the real answer follows
#   "pending"    a 403 from Cloudflare whose body is not readable yet
#   "page"       anything else: the host answered the browser
#   NULL         nothing yet
sc_watch_documents <- function(session) {
  invisible(session$Network$enable())
  frame <- tryCatch(session$Page$getFrameTree()$frameTree$frame$id,
                    error = function(e) NULL)
  w <- new.env(parent = emptyenv())
  w$doc <- NULL
  unregister <- session$Network$responseReceived(callback_ = function(ev) {
    if (!identical(ev$type, "Document")) return(invisible(NULL))
    if (!is.null(frame) && !is.null(ev$frameId) && !identical(ev$frameId, frame))
      return(invisible(NULL))
    h <- sc_parse_headers(ev$response$headers %||% list())
    w$doc <- list(requestId = ev$requestId, status = ev$response$status %||% 0,
                  headers = h, url = ev$response$url,
                  type = tolower(h[["content-type"]] %||%
                                   ev$response$mimeType %||% ""),
                  body = NULL)
  }, wait_ = FALSE)
  w$stop <- function() try(unregister(), silent = TRUE)
  w
}

sc_document_kind <- function(session, w) {
  doc <- w$doc
  if (is.null(doc)) return(NULL)
  if (is_challenge(doc$headers)) return("challenge")
  status <- as.integer(doc$status)
  if (status >= 300L && status < 400L) return("redirect")
  if (status == 403L &&
      identical(tolower(doc$headers[["server"]] %||% ""), "cloudflare")) {
    if (is.null(doc$body)) {
      body <- tryCatch(
        session$Network$getResponseBody(requestId = doc$requestId)$body,
        error = function(e) NULL)
      if (is.null(body)) return("pending")
      w$doc$body <- body
      doc$body <- body
    }
    if (is_block(403L, doc$headers, doc$body)) return("block")
  }
  "page"
}

# How a browser that is no longer there is reported: the message says so,
# and `reason = "gone"` lets a session tell it from other failures.
sc_gone_message <- function() "the browser went away: closed, or crashed."

# The protocol call that pumps events; a browser that has gone away raises.
sc_pump <- function(session, what = "safeconduct_not_passed") {
  tryCatch(session$Browser$getVersion(), error = function(e) {
    stop(sc_error(what, paste0("The browser check was not passed: ",
                               sc_gone_message()), reason = "gone"))
  })
  invisible(NULL)
}

# Load `url` in the session and wait for the host to answer the browser with
# something other than a check or a block. Returns the clearance -- the
# browser's user-agent string and its cookies for the host, whatever they are
# -- or raises `safeconduct_not_passed`, with `reason` `"blocked"` when the
# browser was shown the block page and `"timeout"` when the check was still
# pending after `timeout` seconds.
sc_wait_pass <- function(session, url, timeout) {
  w <- sc_watch_documents(session)
  on.exit(w$stop(), add = TRUE)
  invisible(session$Page$navigate(url, wait_ = FALSE))
  focus <- sc_focus_script()
  deadline <- Sys.time() + timeout
  pending_since <- NULL
  repeat {
    Sys.sleep(0.5)
    sc_pump(session)
    sc_focus(session, focus)
    kind <- sc_document_kind(session, w)
    if (identical(kind, "page")) return(sc_snapshot_clearance(session, url))
    if (identical(kind, "pending")) {
      pending_since <- pending_since %||% Sys.time()
      if (Sys.time() - pending_since > 3) kind <- "block"
    } else {
      pending_since <- NULL
    }
    if (identical(kind, "block")) {
      stop(sc_error("safeconduct_not_passed", paste0(
        "'", sc_url_host(url), "' blocks this browser too; there is no safe ",
        "conduct to be had here."), reason = "blocked"))
    }
    if (Sys.time() > deadline) {
      stop(sc_error("safeconduct_not_passed", paste0(
        "The browser check at '", sc_url_host(url), "' was not passed within ",
        timeout, " seconds."), reason = "timeout"))
    }
  }
}

# What the browser now holds for the host.
sc_snapshot_clearance <- function(session, url) {
  cookies <- tryCatch(session$Network$getCookies(urls = list(url))$cookies,
                      error = function(e) list())
  ua <- session$Runtime$evaluate("navigator.userAgent")$result$value
  sc_clearance(url, ua, sc_cookie_header(cookies))
}

sc_clearance <- function(url, useragent, cookie) {
  structure(list(host = sc_url_host(url), useragent = useragent,
                 cookie = cookie, time = Sys.time()),
            class = "safeconduct_clearance")
}

#' @export
print.safeconduct_clearance <- function(x, ...) {
  cat("<safeconduct clearance for '", x$host, "', issued ",
      format(x$time, "%Y-%m-%d %H:%M:%S"), ">\n", sep = "")
  cat("  user agent: ", x$useragent, "\n", sep = "")
  names <- if (nzchar(x$cookie)) sub("=.*", "", strsplit(x$cookie, "; ")[[1]])
           else "none"
  cat("  cookies:    ", paste(names, collapse = ", "), "\n", sep = "")
  invisible(x)
}

# Download `url` through the browser into `destfile`. The file is written
# beside its destination under Chrome's own name for it and renamed on
# completion, so a transfer that fails leaves nothing at `destfile`. A
# transfer that makes no progress for two minutes, or runs past `timeout`
# seconds, is cancelled and raises a `safeconduct_download_error`.
#
# `Browser.setDownloadBehavior` with `allowAndName` lands the file as
# `<dir>/<guid>` under `tempdir()` and reports progress through
# `Browser.downloadProgress`; the file is moved into place when complete.
# The main frame's documents are watched as well: a check at the file's URL
# runs and the download follows, a check still running after its wait and a
# block page end the attempt as `safeconduct_not_passed` (the check with
# `reason = "timeout"`, so that a caller may open a window instead), and an
# HTML page that starts no download within fifteen seconds is a
# `safeconduct_download_error` -- the server answered with a page, not the
# file.
sc_page_grace <- function() 15

# How long a check at the file's URL may run before the download is given up
# as not passed. A window waits for a person, as the root wait does. Headless
# Chrome has no one to wait for, but Cloudflare has let it through such a
# check with nothing clicked after holding it for about two minutes
# (www12.statcan.gc.ca, October 2026), so it is given a little more than that.
sc_check_wait <- function(headed) if (headed) 120 else 150

# What is said when a check turns up at the file's URL. `fallback` says
# whether a window follows should headless Chrome not get through in time.
sc_check_note <- function(headed, fallback = FALSE) {
  paste0("  The server is asking for a browser check at the file's URL first; ",
    if (headed) "if it shows a \"Verify you are human\" box, click the box."
    else paste0("headless Chrome passes it without a click, but it may take ",
                "two minutes.\n  If it has not passed by then, ",
                if (fallback) "a window is opened." else "the download fails."))
}

# Where the browser lands files before they are moved into place: under
# `tempdir()`, not next to the destination, so that nothing of the package's
# turns up in a working directory.
sc_download_dir <- function() {
  dir <- file.path(tempdir(), "safeconduct")
  if (!dir.exists(dir)) dir.create(dir)
  dir
}

# Move a finished download into place: a rename where the two are on one
# volume, a copy otherwise.
sc_move_file <- function(from, to) {
  if (isTRUE(suppressWarnings(file.rename(from, to)))) return(TRUE)
  ok <- isTRUE(file.copy(from, to, overwrite = TRUE))
  if (ok) unlink(from)
  ok
}

sc_browser_download <- function(session, url, destfile, quiet = FALSE,
                                timeout = 3600, headed = TRUE,
                                fallback = FALSE, hints = FALSE) {
  if (!dir.exists(dirname(destfile)))
    stop("The directory for '", destfile, "' does not exist.", call. = FALSE)
  dir <- sc_download_dir()
  invisible(session$Browser$setDownloadBehavior(
    behavior = "allowAndName", downloadPath = dir, eventsEnabled = TRUE))
  progress <- NULL
  unregister <- session$Browser$downloadProgress(
    callback_ = function(ev) progress <<- ev, wait_ = FALSE)
  on.exit(try(unregister(), silent = TRUE), add = TRUE)
  w <- sc_watch_documents(session)
  on.exit(w$stop(), add = TRUE)
  invisible(session$Page$navigate(url, wait_ = FALSE))

  started <- Sys.time()
  last_bytes <- -1
  last_change <- started
  last_report <- started
  page_since <- NULL
  check_since <- NULL
  told <- FALSE
  cleanup <- function() {
    guid <- progress$guid
    if (!is.null(guid)) {
      try(session$Browser$cancelDownload(guid = guid), silent = TRUE)
      unlink(list.files(dir, pattern = paste0("^", guid), full.names = TRUE))
    }
  }
  fail <- function(why, class = "safeconduct_download_error", reason = NULL) {
    cleanup()
    stop(sc_error(class, paste0(
      "Could not download '", url, "' in the browser: ", why), reason = reason))
  }
  repeat {
    Sys.sleep(0.5)
    tryCatch(session$Browser$getVersion(),
             error = function(e) fail(sc_gone_message(), reason = "gone"))
    now <- Sys.time()
    state <- progress$state %||% "pending"
    if (identical(state, "completed")) break
    if (identical(state, "canceled")) fail("the browser cancelled it.")
    if (is.null(progress)) {
      kind <- sc_document_kind(session, w)
      if (identical(kind, "block")) {
        fail(paste0("'", sc_url_host(url), "' blocks this browser too."),
             "safeconduct_not_passed")
      }
      if (identical(kind, "challenge")) {
        page_since <- NULL
        check_since <- check_since %||% now
        last_change <- now
        if (!told) {
          sc_message(quiet, sc_check_note(headed, fallback))
          if (!headed) {
            sc_hint(hints, paste0("headed:", sc_url_host(url)),
              "rather than wait, headed = TRUE opens a window at once, where ",
              "the check shows its box to click.")
          }
          told <- TRUE
        }
        if (now - check_since > sc_check_wait(headed)) {
          fail(paste0("the browser check at the file's URL was not passed ",
                      "within ", sc_check_wait(headed), " seconds."),
               "safeconduct_not_passed", reason = "timeout")
        }
      } else if (identical(kind, "page") && grepl("html", w$doc$type %||% "")) {
        page_since <- page_since %||% now
        if (now - page_since > sc_page_grace()) {
          fail(paste0("the server answered with a page (status ",
                      w$doc$status, "), not the file."))
        }
      }
    }
    received <- progress$receivedBytes %||% 0
    if (received != last_bytes) { last_bytes <- received; last_change <- now }
    if (now - last_change > 120) fail("no data arrived for two minutes.")
    if (now - started > timeout) fail(paste0("it did not finish in ", timeout,
                                             " seconds."))
    if (!quiet && now - last_report > 10 && received > 0) {
      total <- progress$totalBytes %||% 0
      sc_message(quiet, sprintf("  %.0f MB%s", received / 1e6,
        if (total > 0) sprintf(" of %.0f", total / 1e6) else ""))
      last_report <- now
    }
  }
  from <- file.path(dir, progress$guid)
  if (!file.exists(from) || file.info(from)$size == 0)
    fail("the browser reported it complete but there is no file.")
  if (!sc_move_file(from, destfile)) {
    unlink(from)
    fail("the file could not be moved into place.")
  }
  invisible(destfile)
}

# Reading a resource in the browser.
#
# The page the session holds -- a page on the host -- runs `fetch()` on the
# URL and hands back status, headers and the body. The body comes as base64
# so that binary resources survive the trip; the request is same-origin, so
# every response header is readable, including `cf-mitigated` on a check.
# This is for pages and feeds; a protocol message is not the place for a
# large file, which is what `sc_browser_download()` is for.
sc_read_script <- "(async function(url) {
  try {
    var r = await fetch(url, {cache: 'no-store', credentials: 'include',
                              redirect: 'follow'});
    var buf = new Uint8Array(await r.arrayBuffer());
    var bin = '';
    for (var i = 0; i < buf.length; i += 32768)
      bin += String.fromCharCode.apply(null, buf.subarray(i, i + 32768));
    var headers = {};
    r.headers.forEach(function(v, k) { headers[k] = v; });
    return {ok: true, status: r.status, url: r.url, headers: headers,
            body: btoa(bin)};
  } catch (e) {
    return {ok: false, error: String(e)};
  }
})(%s)"

# Returns a list like `curl::curl_fetch_memory()`'s: `url`, `status_code`,
# `type`, `headers` (a named list) and `content` (raw). Raises
# `safeconduct_download_error` when the request could not be made at all:
# the page's `fetch()` failed (the message is the browser's), the call did
# not come back in time, or the browser is no longer there (`reason`
# `"gone"`; a session opens another and reads again).
sc_browser_read <- function(session, url, timeout = 120) {
  js <- sprintf(sc_read_script, encodeString(url, quote = '"'))
  failed <- NULL
  res <- tryCatch(
    session$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE,
                             timeout_ = timeout * 1000),
    error = function(e) { failed <<- conditionMessage(e); NULL })
  v <- res$result$value
  if (is.null(v) || !isTRUE(v$ok)) {
    gone <- !isTRUE(tryCatch(session$is_active(), error = function(e) FALSE))
    why <- if (gone) sc_gone_message()
           else if (!is.null(v$error)) paste0("the page's request failed: ", v$error)
           else if (!is.null(failed)) failed
           else paste0("no answer within ", timeout, " seconds.")
    stop(sc_error("safeconduct_download_error", paste0(
      "Could not read '", url, "' in the browser: ", why),
      reason = if (gone) "gone"))
  }
  h <- sc_parse_headers(as.list(v$headers %||% list()))
  list(url = v$url %||% url, status_code = as.integer(v$status),
       type = tolower(h[["content-type"]] %||% NA_character_),
       headers = h, content = jsonlite::base64_dec(v$body %||% ""))
}
