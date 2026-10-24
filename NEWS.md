# safeconduct 0.1.0

* A Cloudflare block page -- a `403` that says "Sorry, you have been blocked"
  and carries no check -- is now recognized as a refusal a browser gets past.
  `probe()` reports it as `blocked`, `is_block()` is the test, and `fetch()`
  brings in a browser for it as it does for a check.
* Whether the browser got through is read off the page the host answers it
  with, not off the `cf_clearance` cookie. Hosts that block plain clients but
  never check a browser hand out no such cookie and used to time out; they
  now pass at once. A host that blocks the browser too is reported within a
  second or two as `safeconduct_not_passed`, instead of after the timeout.
* `options(safeconduct.headed = "auto")` tries headless Chrome first and opens
  a window only when the headless run ends on a check or a block.
* `fetch_memory()` reads a page or a feed into memory by the same route
  `fetch()` takes, reading in the browser where only the browser is served.
* `browser_session()` keeps a browser open on a host for `fetch_memory()` and
  `fetch()` to use through their `session` argument, for a feed read every few
  minutes. A check the host asks for again meanwhile is passed and the read
  repeated.
* `download_in_browser()` no longer waits for a clearance cookie before
  starting the transfer, and a check at the file's URL is let run.
* `headed` is an argument of every function that may open a browser --
  `TRUE` for a window, `FALSE` for headless, `"auto"` to try headless first --
  so that one call can take a window and the next not. The
  `safeconduct.headed` option is its default.
* At the console, in an interactive session, once a host has been found to
  need a window or the browser for the transfer, a tip says how to skip the
  finding out next time: `headed = TRUE`, or a `browser_session()`. Tips come
  once per host and R session, and not for calls made from another package.

# safeconduct 0.0.1

* First version. `fetch()` downloads a file, opening a Chrome window to pass a
  Cloudflare browser check when the server asks for one; `clearance()`,
  `handle()`, `probe()`, `is_challenge()`, `browser_available()` and
  `download_in_browser()` expose the pieces.
