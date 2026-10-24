# The public flow.
#
# A plain request first; only a browser check (`is_challenge()`) brings a
# browser into it, so a server that stops checking is simply fetched. Once
# the window has passed the check, one ranged probe asks whether curl,
# carrying the browser's user agent and cookies, is served the resource. The
# clearance is tied to the address and the user-agent string, and the handoff
# works -- until the vendor escalates, after which the same handoff is
# answered with the check again. So when it fails the browser downloads the
# file itself: it is the one client whose clearance is never in doubt. The
# handoff is kept because it is the lighter path when it works -- no window
# for the length of a transfer -- and because its clearance is reused for
# the next file from the same host without opening a browser at all.

# One clearance per host, kept for the session. Cloudflare's cookie lasts
# about half an hour; rather than track that, a cached clearance is probed
# and dropped when the probe says it has lapsed.
bf_clearance_cache <- new.env(parent = emptyenv())

#' Download a file, through a browser if the server insists on one
#'
#' Fetches `url` to `destfile`. A server that serves the file to a plain
#' request is fetched with curl and no browser is involved. A server that
#' answers with a browser check (see [is_challenge()]) gets one: a Chrome
#' window with a throwaway profile is opened, the check runs, and the file is
#' then downloaded, through curl carrying the clearance the browser was
#' given where the server accepts that, and in the browser window itself
#' where it does not. A clearance that curl can use is kept for the rest of
#' the session, so later files from the same host need no window.
#'
#' Nothing is simulated. When the check shows a "Verify you are human" box,
#' the window stays on screen for up to two minutes for the person at the
#' screen to click it; nothing in this package clicks it for them.
#'
#' @section Requirements:
#' A Chrome or Chromium installation, found by [chromote::find_chrome()] (set
#' the `CHROMOTE_CHROME` environment variable if it is somewhere unusual),
#' and a display to show the window on. Without a display Chrome is run
#' headless, which, as of October 2026, Cloudflare's check does not pass.
#' Set `options(safeconduct.browser = FALSE)` to never open a browser, and
#' `options(safeconduct.headed = TRUE)` or `FALSE` to override the guess
#' about the display. The window is 1000 by 800 pixels;
#' `options(safeconduct.window = c(width, height))` opens one of another size
#' in pixels, and `"maximized"` a maximized one. Experimentally, `options(safeconduct.focus = "<css
#' selector>")` pins the element that holds the check to the window's top
#' left corner, enlarged by `options(safeconduct.zoom)` (1.5 by default), and
#' hides the rest of the page; unset, the page is shown as it is.
#'
#' @param url The file's URL.
#' @param destfile Where to write it. A failed transfer leaves nothing there.
#' @param quiet Suppress progress messages.
#' @param timeout Seconds a transfer in the browser may take.
#' @return `destfile`, invisibly. Errors are classed under
#'   `safeconduct_error`: `safeconduct_unavailable` when no browser can be
#'   opened here (its message says what is missing),
#'   `safeconduct_not_passed` when the check was not passed in time, and
#'   `safeconduct_download_error` when the transfer itself failed.
#' @examples
#' \dontrun{
#' url <- paste0("https://www12.statcan.gc.ca/census-recensement/2021/",
#'               "geo/sip-pis/rnf-frr/files-fichiers/lrnf000r21a_e.zip")
#' fetch(url, "lrnf000r21a_e.zip")
#' }
#' @export
fetch <- function(url, destfile, quiet = FALSE, timeout = 3600) {
  p <- bf_probe(url)
  if (!p$challenged) return(bf_curl_download(url, destfile, quiet = quiet))

  host <- bf_url_host(url)
  cached <- bf_clearance_cache[[host]]
  if (!is.null(cached)) {
    if (!bf_probe(url, cached)$challenged) {
      return(bf_curl_download(url, destfile, quiet = quiet, clearance = cached))
    }
    rm(list = host, envir = bf_clearance_cache)
  }

  chrome <- bf_open_and_clear(url, quiet = quiet)
  on.exit(chrome$close(), add = TRUE)

  if (!bf_probe(url, chrome$clearance)$challenged) {
    bf_message(quiet, "  Passed; downloading.")
    assign(host, chrome$clearance, envir = bf_clearance_cache)
    chrome$close()
    on.exit()
    return(bf_curl_download(url, destfile, quiet = quiet,
                            clearance = chrome$clearance))
  }
  bf_message(quiet, "  Passed, but only for the browser; downloading in the ",
             "browser window, which stays open until it is done.")
  bf_browser_download(chrome$session, url, destfile, quiet = quiet,
                      timeout = timeout)
}

#' Get a clearance for a host from a browser
#'
#' Opens a Chrome window on the host, lets the browser check run, and returns
#' what the browser was given: its user-agent string and its cookies, which
#' [handle()] puts on a curl request. Whether the server then serves curl is
#' the server's call -- see [fetch()], which falls back to downloading in the
#' browser when it does not. Clearances are cached per host for the session.
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
#' curl::curl_fetch_memory("https://www12.statcan.gc.ca/", handle(cl))
#' }
#' @export
clearance <- function(url, refresh = FALSE, quiet = FALSE, wait = NULL) {
  host <- bf_url_host(url)
  if (!refresh) {
    cached <- bf_clearance_cache[[host]]
    if (!is.null(cached)) return(cached)
  }
  chrome <- bf_open_and_clear(url, quiet = quiet, wait = wait)
  chrome$close()
  assign(host, chrome$clearance, envir = bf_clearance_cache)
  chrome$clearance
}

#' Download a file in a browser window
#'
#' Opens a Chrome window with a throwaway profile, lets any browser check
#' run, and downloads `url` in the window, which stays open until the
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
download_in_browser <- function(url, destfile, quiet = FALSE, timeout = 3600) {
  chrome <- bf_open_and_clear(url, quiet = quiet)
  on.exit(chrome$close(), add = TRUE)
  bf_browser_download(chrome$session, url, destfile, quiet = quiet,
                      timeout = timeout)
}

# Open a browser on the host and wait for the check to pass. Returns the
# launched browser with its `clearance` added, or raises
# `safeconduct_unavailable` / `safeconduct_not_passed`.
bf_open_and_clear <- function(url, quiet = FALSE, wait = NULL) {
  reason <- bf_browser_unavailable_reason()
  if (!is.null(reason)) {
    stop(bf_error("safeconduct_unavailable", paste0(
      "'", url, "' is behind a browser check, and no browser can be opened ",
      "here: ", reason, ".")))
  }
  headed <- bf_display_available()
  # A window on a screen can be clicked by whoever is at it, whether or not R
  # is interactive, so the wait is for the person, not the check. Headless
  # Chrome has no one to wait for.
  wait <- wait %||% if (headed) 120 else 30
  bf_message(quiet,
    "'", bf_url_host(url), "' is asking for a browser check.",
    if (headed) paste0(
      "\n  Opening a Chrome window to pass it; this usually takes a few ",
      "seconds.\n  If it shows a \"Verify you are human\" box, click the box.")
    else "\n  No display is available, so trying a headless Chrome.")

  chrome <- tryCatch(bf_launch_chrome(headed), error = function(e) {
    stop(bf_error("safeconduct_unavailable", paste0(
      "Could not start Chrome: ", conditionMessage(e))))
  })
  clearance <- tryCatch(
    bf_wait_clearance(chrome$session, bf_host_root(url), wait),
    error = function(e) NULL)
  if (is.null(clearance)) {
    chrome$close()
    stop(bf_error("safeconduct_not_passed", paste0(
      "The browser check at '", bf_url_host(url), "' was not passed within ",
      wait, " seconds.")))
  }
  chrome$clearance <- clearance
  chrome
}

# Indirection so the flow can be tested without a network.
bf_probe <- function(url, clearance = NULL) probe(url, clearance)

# Fetch with curl, converting any failure -- unreachable host, HTTP error, an
# empty result -- into a `safeconduct_download_error`. curl writes to a
# temporary file and renames it on success, so a failed fetch leaves nothing
# behind. There is no overall timeout, but a transfer that stalls below a
# kilobyte a second for two minutes is abandoned.
bf_curl_download <- function(url, destfile, quiet = FALSE, clearance = NULL) {
  h <- handle(clearance, connecttimeout = 60,
              low_speed_limit = 1024, low_speed_time = 120)
  err <- NULL
  ok <- tryCatch({
    curl::curl_download(url, destfile, quiet = quiet, mode = "wb", handle = h)
    TRUE
  }, error = function(e) { err <<- conditionMessage(e); FALSE })
  ok <- ok && file.exists(destfile) && file.info(destfile)$size > 0
  if (!ok) {
    if (file.exists(destfile)) unlink(destfile)
    stop(bf_error("safeconduct_download_error", paste0(
      "Could not download '", url, "'",
      if (!is.null(err)) paste0(": ", err) else ".")))
  }
  invisible(destfile)
}
