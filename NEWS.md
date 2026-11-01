# safeconduct 0.1.0

First release.

## Why this package exists

On October 2, 2026 Statistics Canada moved parts of its website behind a
Cloudflare browser check, among them the census geography and correspondence
files on www12.statcan.gc.ca. A plain HTTP request from R, whether through
`download.file()`, `curl` or anything built on them, has been answered with a
`403` check page instead of the file ever since, so those files became
unreachable from R by the usual means. That broke functionality in the
cancensus package, which downloads such files on its users' behalf, and it
broke every script that fetched them directly.

Statistics Canada is not alone. The City of Vancouver's Open Data portal
has moved its large file downloads behind a Cloudflare check as well. The
VancouvR package is itself unaffected, since the API it talks to and the
download links it serves work as before and nothing in the package starts a
download, but a user who follows one of those links programmatically fails
unless the download goes through safeconduct. As more public data moves
behind such checks, the need for this is broader than one package or one
agency.

safeconduct restores that access as a drop-in replacement for the download:
where a script called `download.file(url, destfile)`, it calls
`safeconduct::fetch(url, destfile)`, and a package does the same inside its
own download helper. Where the server serves the file to a plain request, that
is what happens and no browser is involved. Where it answers with a browser
check or a block page, a real Chrome with a throwaway profile is opened on the
host, the check runs, and the file is fetched: through `curl` carrying the
clearance the browser was given where the server accepts that, and in the
browser itself where it does not. The price is a dependency on an installed
Chrome or Chromium, and, when a check insists on a click, a window that waits
for the person at the screen to click it. Nothing is simulated and nothing is
clicked on anyone's behalf, so a check that wants a click cannot be passed
where no one is present, say in a container or on a build server.

## What is in it

* `fetch(url, destfile)` downloads a file, bringing in a browser only when a
  plain request is refused in a way a browser gets past. A clearance that
  `curl` can use is kept for the rest of the R session, so later files from
  the same host need no browser. A file downloaded in the browser lands under
  `tempdir()` and is moved to `destfile` when complete; nothing is written
  next to the destination meanwhile.
* `read_page(url)` is `fetch()` for a page or a feed: the response comes back
  in memory, shaped like `curl::curl_fetch_memory()`'s, by the same route,
  read in the browser where only the browser is served.
* `page_text()`, `page_json()` and `page_html()` parse a `read_page()`
  response, taking the charset from its headers and raising a non-2xx status
  as a classed `safeconduct_http_error`. Each also takes a URL, so
  `page_html(url)` stands in for `rvest::read_html(url)`.
* `browser_session(url)` keeps one browser open on a host for repeated
  `read_page()` and `fetch()` calls through their `session` argument. A check
  the host asks for again meanwhile is passed and the read repeated; a browser
  that has gone away, its window closed, is reopened at the next call.
* `probe(url)` says what a server serves at a URL and whether a browser is
  needed; `is_challenge(headers)` recognizes a Cloudflare browser check by its
  `cf-mitigated: challenge` header, and `is_block(status, headers, body)` the
  "Sorry, you have been blocked" page that carries no check.
* `clearance(url)` returns the user-agent string and cookies a browser was
  given on a host, and `clearance_handle(clearance)` puts them on a `curl`
  handle, for requests the package does not make itself.
* `browser_available()` says whether a browser can be opened on this machine,
  and why not if not. `download_in_browser()` is the browser transfer on its
  own.
* `headed`, an argument of every function that may open a browser, is
  `"auto"` by default: headless Chrome goes first, and a window is opened only
  when the headless run ends on a check or a block and there is a display to
  show it on. `TRUE` opens a window at once, `FALSE` never does.
* `useragent`, likewise an argument of every such function, is `TRUE` by
  default: the word `Headless` is dropped from Chrome's user-agent string and
  nothing else, because some hosts refuse headless Chrome on that word alone
  while serving the same browser once it is gone. `NULL` leaves Chrome's own
  string, and a string is used as given.
* Errors are classed under `safeconduct_error`: `safeconduct_unavailable` when
  no browser can be opened here, `safeconduct_not_passed` when a check was not
  passed in time or the server blocks the browser too, and
  `safeconduct_download_error` when the transfer failed. A calling package can
  catch these and turn them into instructions to download by hand.
  `options(safeconduct.browser = FALSE)` never opens a browser.
* At the console, once a host has been found to need a window or the browser
  for the transfer, a tip says how to skip the finding out next time. Tips come
  once per host and R session, and not for calls made from another package.
* A getting-started vignette, `vignette("safeconduct")`, walks through a
  Statistics Canada file behind a browser check and a Vancouver election file
  behind a block: what a plain request gets, what `probe()` says, and what
  `fetch()` does.
