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
* Headless Chrome goes first, and a window is opened only when the headless
  run ends on a check or a block; `headed = TRUE` opens one at once, `FALSE`
  never does.
* `read_page()` reads a page or a feed into memory by the same route
  `fetch()` takes, reading in the browser where only the browser is served.
* `browser_session()` keeps a browser open on a host for `read_page()` and
  `fetch()` to use through their `session` argument, for a feed read every few
  minutes. A check the host asks for again meanwhile is passed and the read
  repeated.
* `useragent`, an argument of every function that may open a browser, sets
  the user-agent string the browser announces. Headless Chrome's own string
  says `HeadlessChrome`, and some hosts refuse it on that word alone
  (results.vancouver.ca is one) while serving the same browser once the word
  is gone; with the word gone it has also passed a managed challenge with
  nothing clicked. So the default, `TRUE`, drops the word and nothing else;
  `NULL` leaves Chrome's own string, and a string is used as given. A
  `browser_session()` keeps it for its life.
* A session whose browser has gone away -- its window closed, or Chrome
  crashed -- opens another at the next read or download, the way it was
  opened before, and says so; it used to fail with "Could not read '...' in
  the browser." and nothing more. A read or download in a browser that goes
  away is now reported as that, and the error carries `reason = "gone"`.
* `handle()` is now `clearance_handle()`, so that its name says what it carries
  and does not read as curl's own.
* `download_in_browser()` no longer waits for a clearance cookie before
  starting the transfer, and a check at the file's URL is let run.
* `headed` is an argument of every function that may open a browser --
  `"auto"` to try headless first, `TRUE` for a window, `FALSE` for headless --
  so that one call can take a window and the next not. There is no option
  for it: the default adapts to the machine, and a `browser_session()` keeps
  the choice made for it.
* At the console, in an interactive session, once a host has been found to
  need a window or the browser for the transfer, a tip says how to skip the
  finding out next time: `headed = TRUE`, or a `browser_session()`. Tips come
  once per host and R session, and not for calls made from another package.

# safeconduct 0.0.1

* A getting-started vignette, `vignette("safeconduct")`, walks through a
  Statistics Canada file behind a browser check and a Vancouver election file
  behind a block: what a plain request gets, what `probe()` says, and what
  `fetch()` does.
* A check at the file's URL, after the one at the host's root, is now
  announced with what to expect: headless Chrome passes it without a click
  but may take two minutes, and a tip says `headed = TRUE` opens a window at
  once, where the check's box gets it through when clicked. Should headless
  Chrome not pass within its wait, `headed = "auto"` opens a window and
  downloads there, as it already did for the check at the root; a session
  opened in auto mode moves to a window the same way. The check used to be
  given up as "no data arrived for two minutes".
* A file downloaded in the browser lands under `tempdir()` and is moved to
  `destfile` when complete, instead of landing next to `destfile` under a
  temporary name.
* First version. `fetch()` downloads a file, opening a Chrome window to pass a
  Cloudflare browser check when the server asks for one; `clearance()`,
  `handle()`, `probe()`, `is_challenge()`, `browser_available()` and
  `download_in_browser()` expose the pieces.
