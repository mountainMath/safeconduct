# The public flow.
#
# A plain request first; only a refusal that a browser gets past -- a browser
# check or a block page, see `R/probe.R` -- brings a browser into it, so a
# server that stops refusing is simply fetched. The browser is opened on the
# host's root and the host's answer to it watched (`sc_wait_pass()`). Once
# the host has answered, one ranged probe asks whether curl, carrying the
# browser's user agent and cookies, is served the resource. After a check it
# often is: the clearance is tied to the address and the user-agent string,
# and the handoff works -- until the vendor escalates, after which the same
# handoff is answered with the check again. After a block it is not: the
# rule is about the client, not a cookie. So when the handoff fails the
# browser downloads the file itself: it is the one client whose clearance is
# never in doubt. The handoff is kept because it is the lighter path when it
# works -- no window for the length of a transfer -- and because its
# clearance is reused for the next file from the same host without opening
# a browser at all.

# One clearance per host, kept for the session. Cloudflare's cookie lasts
# about half an hour; rather than track that, a cached clearance is probed
# and dropped when the probe says it has lapsed.
sc_clearance_cache <- new.env(parent = emptyenv())

#' Download a file, through a browser if the server insists on one
#'
#' Fetches `url` to `destfile`. A server that serves the file to a plain
#' request is fetched with curl and no browser is involved. A server that
#' refuses a plain request in a way a browser gets past -- a browser check
#' (see [is_challenge()]) or a block page (see [is_block()]) -- gets one: a
#' Chrome with a throwaway profile is opened on the host, the check runs if
#' there is one, and the file is then downloaded, through curl carrying the
#' clearance the browser was given where the server accepts that, and in the
#' browser itself where it does not. A clearance that curl can use is kept
#' for the rest of the R session, so later files from the same host need no
#' browser.
#'
#' Nothing is simulated. When the check shows a "Verify you are human" box,
#' the window stays on screen for up to two minutes for the person at the
#' screen to click it; nothing in this package clicks it for them.
#'
#' A file downloaded in the browser lands under [tempdir()] and is moved to
#' `destfile` when complete; nothing is written next to `destfile` meanwhile.
#'
#' @section Requirements:
#' A Chrome or Chromium installation, found by [chromote::find_chrome()] (set
#' the `CHROMOTE_CHROME` environment variable if it is somewhere unusual).
#' By default Chrome runs headless first, and a window is opened only when
#' the headless run ends on a check or a block and there is a display to
#' show it on; the `headed` argument changes that. Headless Chrome under its
#' own name does not pass a Cloudflare managed challenge, as of October
#' 2026, but with the word `Headless` dropped from its user-agent string,
#' which is the default, it has. Set `options(safeconduct.browser = FALSE)`
#' to never open a browser. The window is 1000 by 800 pixels;
#' `options(safeconduct.window = c(width, height))` opens one of another size
#' in pixels, and `"maximized"` a maximized one. Experimentally,
#' `options(safeconduct.focus = "<css selector>")` pins the element that
#' holds the check to the window's top left corner, enlarged by
#' `options(safeconduct.zoom)` (1.5 by default), and hides the rest of the
#' page; unset, the page is shown as it is.
#'
#' @section Tips:
#' At the console, in an interactive session, once a host has been found to
#' need a window or the browser for the transfer, a tip says how to skip the
#' finding out next time: `headed = TRUE`, or a [browser_session()] for
#' several files or reads from the host. A tip is shown once per host and R
#' session, and never when `quiet` is `TRUE`. Calls made from another
#' package get no tips, since that package's user cannot act on them; nothing
#' needs to be set for that. The tips are messages with the class
#' `safeconduct_hint` as well, should anyone want to muffle them.
#'
#' @param url The file's URL.
#' @param destfile Where to write it. A failed transfer leaves nothing there.
#' @param quiet Suppress progress messages.
#' @param headed How to run the browser, when one is needed: `"auto"`, the
#'   default, tries headless Chrome first and opens a window only when the
#'   headless run does not get through, so that a window is shown only when
#'   someone may need to click; `TRUE` opens a window at once and `FALSE`
#'   never does. Without a display `"auto"` is headless. A host may run a
#'   second check at the file's URL, after the one at its root; headless
#'   Chrome has passed such a check with nothing clicked, but only after
#'   about two minutes, where a window shows a box that gets it through as
#'   soon as it is clicked. `"auto"` waits those minutes and opens a window
#'   if they pass without; `TRUE` skips the wait.
#' @param useragent The user-agent string the browser announces. Headless
#'   Chrome's own calls it `HeadlessChrome`, and some hosts refuse it on that
#'   word alone. `TRUE`, the default, drops the word, so that the string says
#'   the Chrome version and platform it is and nothing it is not; `NULL`
#'   leaves Chrome's own string; a string is used as given. Set before the
#'   browser's first request, and what [clearance()] then reports.
#' @param timeout Seconds a transfer in the browser may take.
#' @param session A browser session from [browser_session()]. When given,
#'   the file is downloaded in that browser without any probing, and the
#'   browser stays open afterwards; `headed` and `useragent` are then the
#'   session's. A browser that has gone away -- its window closed -- is
#'   opened again first.
#' @return `destfile`, invisibly. Errors are classed under
#'   `safeconduct_error`: `safeconduct_unavailable` when no browser can be
#'   opened here (its message says what is missing),
#'   `safeconduct_not_passed` when the check was not passed in time or the
#'   server blocks the browser as well, and `safeconduct_download_error`
#'   when the transfer itself failed.
#' @examples
#' \dontrun{
#' url <- paste0("https://www12.statcan.gc.ca/census-recensement/2021/",
#'               "geo/sip-pis/rnf-frr/files-fichiers/lrnf000r21a_e.zip")
#' fetch(url, "lrnf000r21a_e.zip")
#' }
#' @export
fetch <- function(url, destfile, quiet = FALSE, timeout = 3600, session = NULL,
                  headed = "auto", useragent = TRUE) {
  if (!is.null(session)) {
    s <- sc_session_check(session)
    return(sc_session_do(s, quiet = quiet, function(chrome) {
      fallback <- !chrome$headed && identical(s$mode, "auto")
      tryCatch(
        sc_browser_download(chrome$session, url, destfile, quiet = quiet,
                            timeout = timeout, headed = chrome$headed,
                            fallback = fallback),
        safeconduct_not_passed = function(e) {
          if (!fallback || !identical(e$reason, "timeout")) stop(e)
          s$headed <- TRUE
          sc_session_reopen(s, quiet, why = paste0(
            "Headless Chrome did not get through the check at the file's URL ",
            "on '", s$host, "'; opening a window for the session."))
          sc_browser_download(s$chrome$session, url, destfile, quiet = quiet,
                              timeout = timeout, headed = TRUE)
        })
    }))
  }
  hints <- sc_hints_on(quiet)
  p <- sc_probe(url)
  if (!sc_needs_browser(p)) return(sc_curl_download(url, destfile, quiet = quiet))

  host <- sc_url_host(url)
  cached <- sc_clearance_cache[[host]]
  if (!is.null(cached)) {
    if (!sc_needs_browser(sc_probe(url, cached))) {
      return(sc_curl_download(url, destfile, quiet = quiet, clearance = cached))
    }
    rm(list = host, envir = sc_clearance_cache)
  }

  sc_with_browser(url, quiet = quiet, what = p, headed = headed,
                  useragent = useragent, hints = hints, function(chrome) {
    if (!sc_needs_browser(sc_probe(url, chrome$clearance))) {
      sc_message(quiet, "  Passed; downloading with curl under the browser's clearance.")
      assign(host, chrome$clearance, envir = sc_clearance_cache)
      chrome$close()
      return(sc_curl_download(url, destfile, quiet = quiet,
                              clearance = chrome$clearance))
    }
    sc_message(quiet, "  Passed, but the clearance does not transfer to curl; ",
               "downloading under escort, in the browser",
               if (chrome$headed) " window, which stays open until it is done."
               else ".")
    used <- sc_escorted_download(chrome, url, destfile, quiet, timeout, hints)
    sc_hint_session(hints, url, "fetch", used)
    invisible(destfile)
  })
}

# Download in the browser the host let through. In auto mode, should
# headless Chrome not get through a check the host runs at the file's URL
# within its wait, a window is opened and the download made there: the box
# such a check shows can be clicked by the person at the window, which
# headless Chrome has no one to do. Returns the browser the download was
# made in, for the tips; a window opened here is closed before returning.
sc_escorted_download <- function(chrome, url, destfile, quiet = FALSE,
                                 timeout = 3600, hints = FALSE) {
  fallback <- !chrome$headed && identical(chrome$mode, "auto")
  tryCatch({
    sc_browser_download(chrome$session, url, destfile, quiet = quiet,
                        timeout = timeout, headed = chrome$headed,
                        fallback = fallback, hints = hints)
    chrome
  }, safeconduct_not_passed = function(e) {
    if (!fallback || !identical(e$reason, "timeout")) stop(e)
    host <- sc_url_host(url)
    sc_message(quiet, "  Headless Chrome did not get through the check at the ",
               "file's URL; opening a window.", sc_window_note())
    chrome$close()
    window <- sc_open_attempt(url, TRUE, NULL, chrome$useragent)
    if (inherits(window, "error")) stop(window)
    on.exit(window$close(), add = TRUE)
    window$fell_back <- TRUE
    window$mode <- chrome$mode
    window$useragent <- chrome$useragent
    sc_hint(hints, paste0("headed:", host),
      "a window is what gets through at '", host, "'; ",
      "headed = TRUE goes straight to one next time.")
    sc_browser_download(window$session, url, destfile, quiet = quiet,
                        timeout = timeout, headed = TRUE)
    window
  })
}

#' Read a resource into memory, through a browser if the server insists
#'
#' The counterpart of [fetch()] for a page or a feed rather than a file: the
#' response is returned instead of written to disk, in the shape of
#' [curl::curl_fetch_memory()]'s. The same path is taken: curl where a plain
#' request is served, curl carrying the browser's clearance where the server
#' accepts that, and the browser itself where it does not, in which case the
#' resource is read by the page the browser holds on the host.
#'
#' Without a `session`, a browser is opened and closed per call. For a feed
#' that is read every few minutes, open a [browser_session()] once and pass
#' it: the resource is then read in that browser, and if the server asks the
#' browser for a check again meanwhile, the check is passed and the read
#' repeated.
#'
#' @inheritParams fetch
#' @param session A browser session from [browser_session()] on the
#'   resource's host. When given, the resource is read in that browser
#'   without any probing.
#' @param timeout Seconds a read in the browser may take.
#' @return A list: `url` (the final URL), `status_code`, `type` (the
#'   `content-type`, lower case), `headers` (a named list) and `content`
#'   (raw). An HTTP error status is returned, not raised; `content` is then
#'   the error page. Errors are classed as [fetch()]'s are. Parse it with
#'   [page_text()], [page_json()] or [page_html()].
#' @examples
#' \dontrun{
#' r <- read_page("https://results.vancouver.ca/data/arearesults.json")
#' page_json(r)
#' }
#' @export
read_page <- function(url, session = NULL, quiet = FALSE, timeout = 120,
                      headed = "auto", useragent = TRUE) {
  if (!is.null(session)) return(sc_session_read(session, url, quiet, timeout))
  hints <- sc_hints_on(quiet)
  p <- sc_probe(url)
  if (!sc_needs_browser(p)) return(sc_curl_fetch(url))

  host <- sc_url_host(url)
  cached <- sc_clearance_cache[[host]]
  if (!is.null(cached)) {
    if (!sc_needs_browser(sc_probe(url, cached))) return(sc_curl_fetch(url, cached))
    rm(list = host, envir = sc_clearance_cache)
  }

  sc_with_browser(url, quiet = quiet, what = p, headed = headed,
                  useragent = useragent, hints = hints, function(chrome) {
    if (!sc_needs_browser(sc_probe(url, chrome$clearance))) {
      sc_message(quiet, "  Passed; reading with curl under the browser's clearance.")
      assign(host, chrome$clearance, envir = sc_clearance_cache)
      chrome$close()
      return(sc_curl_fetch(url, chrome$clearance))
    }
    sc_message(quiet, "  Passed, but the clearance does not transfer to curl; ",
               "reading under escort, in the browser.")
    out <- sc_browser_read(chrome$session, url, timeout = timeout)
    sc_hint_session(hints, url, "read_page", chrome)
    out
  })
}

#' Get a clearance for a host from a browser
#'
#' Opens a Chrome on the host, lets any browser check run, and returns what
#' the browser then holds: its user-agent string and its cookies, which
#' [clearance_handle()] puts on a curl request. Whether the server then serves curl is
#' the server's call -- see [fetch()], which falls back to downloading in the
#' browser when it does not. A host that blocks plain clients without ever
#' checking a browser yields a clearance all the same, with whatever cookies
#' it set; curl will usually not be served with it, since such a block is
#' about the client rather than a cookie. Clearances are cached per host for
#' the R session.
#'
#' @inheritParams fetch
#' @param refresh Get a new clearance even when a cached one is at hand.
#' @param wait Seconds to wait for the check to pass. The default is two
#'   minutes when a window is shown, so that a person can click a box if one
#'   appears, and thirty seconds headless.
#' @return An object of class `safeconduct_clearance`: a list with `host`,
#'   `useragent`, `cookie` (one `Cookie:` header value) and `time`. Errors as
#'   [fetch()] does.
#' @examples
#' \dontrun{
#' cl <- clearance("https://www12.statcan.gc.ca/")
#' curl::curl_fetch_memory("https://www12.statcan.gc.ca/", clearance_handle(cl))
#' }
#' @export
clearance <- function(url, refresh = FALSE, quiet = FALSE, wait = NULL,
                      headed = "auto", useragent = TRUE) {
  host <- sc_url_host(url)
  if (!refresh) {
    cached <- sc_clearance_cache[[host]]
    if (!is.null(cached)) return(cached)
  }
  chrome <- sc_open_browser(url, quiet = quiet, wait = wait, headed = headed,
                            useragent = useragent, hints = sc_hints_on(quiet))
  chrome$close()
  assign(host, chrome$clearance, envir = sc_clearance_cache)
  chrome$clearance
}

#' Download a file in a browser
#'
#' Opens a Chrome with a throwaway profile on the host, lets any browser
#' check run, and downloads `url` in it; a window stays open until the
#' transfer is done. [fetch()] does this only when it has to; this is for
#' when the browser is wanted regardless.
#'
#' @inheritParams fetch
#' @return `destfile`, invisibly. Errors as [fetch()] does.
#' @examples
#' \dontrun{
#' download_in_browser("https://example.org/large.zip", "large.zip")
#' }
#' @export
download_in_browser <- function(url, destfile, quiet = FALSE, timeout = 3600,
                                headed = "auto", useragent = TRUE) {
  hints <- sc_hints_on(quiet)
  sc_with_browser(url, quiet = quiet, headed = headed, useragent = useragent,
                  hints = hints, function(chrome) {
    sc_escorted_download(chrome, url, destfile, quiet, timeout, hints)
    invisible(destfile)
  })
}

# Open a browser on the host and run `fn(chrome)` in it; the browser is
# closed afterwards whatever happens. `fn` may close it earlier itself.
sc_with_browser <- function(url, fn, quiet = FALSE, wait = NULL, what = NULL,
                            headed = "auto", useragent = TRUE, hints = FALSE) {
  chrome <- sc_open_browser(url, quiet = quiet, wait = wait, what = what,
                            headed = headed, useragent = useragent, hints = hints)
  on.exit(chrome$close(), add = TRUE)
  invisible(fn(chrome))
}

# Open a browser on the host's root and wait for the host to answer it.
# Returns the launched browser with its `clearance` added, or raises
# `safeconduct_unavailable` / `safeconduct_not_passed`. In "auto" mode a
# headless attempt that ends on a check or a block is made again in a window.
# `what` is the probe that brought the browser in, for the message;
# `useragent` the string the browser is to announce (see
# `sc_set_useragent()`); `hints` whether tips may be shown (see `sc_hint()`).
sc_open_browser <- function(url, quiet = FALSE, wait = NULL, what = NULL,
                            headed = "auto", useragent = TRUE, hints = FALSE) {
  useragent <- sc_useragent_check(useragent)
  reason <- sc_browser_unavailable_reason()
  if (!is.null(reason)) {
    stop(sc_error("safeconduct_unavailable", paste0(
      "'", url, "' needs a browser, and no browser can be opened here: ",
      reason, ".")))
  }
  mode <- sc_headed_mode(headed)
  headed <- identical(mode, "headed")
  host <- sc_url_host(url)
  sc_message(quiet,
    if (isTRUE(what$challenged)) paste0("'", host, "' is asking for a browser check; sending a browser through.")
    else if (isTRUE(what$blocked)) paste0("'", host, "' turns plain requests away; sending a browser through.")
    else paste0("Sending a browser through to '", host, "'."),
    if (headed) sc_window_note()
    else if (identical(mode, "auto")) "\n  Trying headless Chrome first."
    else "\n  Using headless Chrome.")
  chrome <- sc_open_attempt(url, headed, wait, useragent)
  if (inherits(chrome, "safeconduct_not_passed") && identical(mode, "auto")) {
    blocked <- identical(chrome$reason, "blocked")
    sc_message(quiet, "  Headless Chrome did not get through; opening a window.",
               sc_window_note())
    chrome <- sc_open_attempt(url, TRUE, wait, useragent)
    if (!inherits(chrome, "error")) {
      chrome$fell_back <- TRUE
      sc_hint(hints, paste0("headed:", host),
        "a window is what gets through at '", host, "'; ",
        "headed = TRUE goes straight to one next time.",
        if (blocked && is.null(useragent)) paste0(
          "\n       Some hosts block headless Chrome on the word \"Headless\" in ",
          "its user-agent string alone; useragent = TRUE drops the word."))
    }
  }
  if (inherits(chrome, "error")) stop(chrome)
  chrome$useragent <- useragent
  chrome$mode <- mode
  chrome
}

sc_window_note <- function() {
  paste0("\n  Opening a Chrome window; this usually takes a few seconds.",
         "\n  If it shows a \"Verify you are human\" box, click the box.")
}

# One launch and pass. Returns the browser, or the `safeconduct_not_passed`
# condition after closing it. A window on a screen can be clicked by whoever
# is at it, whether or not R is interactive, so its wait is for the person,
# not the check; headless Chrome has no one to wait for.
sc_open_attempt <- function(url, headed, wait = NULL, useragent = TRUE) {
  wait <- wait %||% if (headed) 120 else 30
  chrome <- tryCatch(sc_launch_chrome(headed), error = function(e) {
    stop(sc_error("safeconduct_unavailable", paste0(
      "Could not start Chrome: ", conditionMessage(e))))
  })
  chrome$wait <- wait
  tryCatch({
    sc_set_useragent(chrome$session, useragent)
    chrome$clearance <- sc_wait_pass(chrome$session, sc_host_root(url), wait)
    chrome
  }, safeconduct_not_passed = function(e) {
    chrome$close()
    e
  })
}

# Indirection so the flow can be tested without a network.
sc_probe <- function(url, clearance = NULL) probe(url, clearance)

# Fetch with curl, converting any failure -- unreachable host, HTTP error, an
# empty result -- into a `safeconduct_download_error`. curl writes to a
# temporary file and renames it on success, so a failed fetch leaves nothing
# behind. There is no overall timeout, but a transfer that stalls below a
# kilobyte a second for two minutes is abandoned.
sc_curl_download <- function(url, destfile, quiet = FALSE, clearance = NULL) {
  h <- clearance_handle(clearance, connecttimeout = 60,
              low_speed_limit = 1024, low_speed_time = 120)
  err <- NULL
  ok <- tryCatch({
    curl::curl_download(url, destfile, quiet = quiet, mode = "wb", handle = h)
    TRUE
  }, error = function(e) { err <<- conditionMessage(e); FALSE })
  ok <- ok && file.exists(destfile) && file.info(destfile)$size > 0
  if (!ok) {
    if (file.exists(destfile)) unlink(destfile)
    stop(sc_error("safeconduct_download_error", paste0(
      "Could not download '", url, "'",
      if (!is.null(err)) paste0(": ", err) else ".")))
  }
  invisible(destfile)
}

# Fetch into memory with curl, in the shape `read_page()` returns. Only a
# failure to get any response is an error; an HTTP error status is returned.
sc_curl_fetch <- function(url, clearance = NULL) {
  res <- tryCatch(
    curl::curl_fetch_memory(url, clearance_handle(clearance, connecttimeout = 60)),
    error = function(e) {
      stop(sc_error("safeconduct_download_error", paste0(
        "Could not read '", url, "': ", conditionMessage(e))))
    })
  h <- sc_parse_headers(rawToChar(res$headers))
  list(url = res$url, status_code = as.integer(res$status_code),
       type = tolower(h[["content-type"]] %||% NA_character_),
       headers = h, content = res$content)
}
