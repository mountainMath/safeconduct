# safeconduct

The goal of **safeconduct** is to handle file download or webpage access in situations when a plain request is refused. It strives to handle a variety of different situations in a way that minimize the need for user interactions via a simple interface that abstracts complications away.

Fetch files and pages through a real browser when a plain request is refused.

Documentation, with the function reference, is at
<https://mountainmath.github.io/safeconduct/>.

Some servers refuse any request that does not come from a web browser.
Cloudflare is the common case, and it refuses in two ways: a browser check, a
`403` page that runs a script, sets a `cf_clearance` cookie and reloads; or a
block, a `403` page that says "Sorry, you have been blocked" and sets nothing,
because the rule is about what the client looks like. `curl`,
`download.file()` and friends never get past either. A browser does, and this
package lets a browser do what a browser is allowed to do:

```r
safeconduct::fetch(
  "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/rnf-frr/files-fichiers/lrnf000r21a_e.zip",
  "lrnf000r21a_e.zip")
```

If the server serves the file to a plain request, that is what happens and no
browser is involved. If it refuses in a way a browser gets past, a Chrome with
a throwaway profile is opened on the host, the check runs if there is one, and
the file is downloaded: through `curl` carrying the clearance the browser was
given where the server accepts that, and in the browser itself where it does
not. A clearance that `curl` can use is kept for the rest of the R session, so
later files from the same host need no browser.

Nothing is simulated. When the check shows a "Verify you are human" box, the
window stays on screen for up to two minutes for the person at the screen to
click it. Nothing in this package clicks it for them, and the browser is not
made to look like anything it is not.

## Pages and feeds

`fetch_memory()` is `fetch()` for a page or a feed: the response comes back
in memory, in the shape `curl::curl_fetch_memory()` returns, by the same
route. For a feed that is read every few minutes, keep one browser open on
the host instead of opening one per read:

```r
s <- safeconduct::browser_session("https://results.vancouver.ca/")
r <- safeconduct::fetch_memory("https://results.vancouver.ca/data/arearesults.json",
                               session = s)
jsonlite::fromJSON(rawToChar(r$content))
close(s)
```

The read is made by the page the browser holds on the host. Should the server
ask the browser for a check again meanwhile, the check is passed and the read
repeated. `fetch(url, destfile, session = s)` downloads a file in the same
browser.

## Windows, and when one is shown

By default a window is shown when there is a display, since a Cloudflare
managed challenge is not passed by headless Chrome. Headless Chrome does get
past some refusals, so `headed = "auto"` tries it first and opens a window
only when the headless run ends on a check or a block, which it finds out
within a couple of seconds. `headed = FALSE` never shows a window, and
`headed = TRUE` always does. The argument is on every function that may open
a browser; `options(safeconduct.headed = ...)` sets its default for the R
session.

At the console, once a host has been found to need a window or the browser
for the transfer, a tip says how to skip the finding out next time:
`headed = TRUE`, or a `browser_session()` for several files or reads from
the host. Tips come once per host and R session.

Headless Chrome announces itself as `HeadlessChrome` in its user-agent string
and this package leaves that as it is. Firewall rules that block clients which
do not look like a browser block it on that account, so on such hosts a window
is what works.

## Limitations

A check that asks for a click waits for a person, so a download that must run
unattended cannot rely on this package where such a check is shown. A server
that blocks the browser too, say by address, is reported as such within a
second or two; nothing more can be done from here.

## Prerequisites

* [Chrome or Chromium](https://www.google.com/chrome/), found by
  `chromote::find_chrome()`. Set the `CHROMOTE_CHROME` environment variable if
  it is somewhere unusual.
* A display to show a window on, for checks that need one. Without one Chrome
  runs headless.

`options(safeconduct.browser = FALSE)` never opens a browser; `fetch()` then
raises a classed `safeconduct_unavailable` error for a refused URL, which a
calling package can catch and turn into instructions to download by hand.

## For package authors

The errors are classed under `safeconduct_error`:

| class | meaning |
|---|---|
| `safeconduct_unavailable` | no browser can be opened here; the message says what is missing |
| `safeconduct_not_passed` | the check was not passed in time, or the server blocks the browser too |
| `safeconduct_download_error` | the transfer failed |

`clearance(url)` returns the browser's user-agent string and cookies for a
host, and `handle(clearance)` puts them on a `curl` handle, for requests this
package does not make itself. `probe(url)` says what a server serves at a URL,
and whether it is a browser check (`is_challenge(headers)`) or a block page
(`is_block(status, headers, body)`).

The tips shown at the console are not shown for calls made from another
package: they name arguments of this package's functions, which that
package's user cannot pass. Nothing needs to be set for that. Should a tip
need muffling all the same, it is a message with the class `safeconduct_hint`
as well.

## Installation

```r
# install.packages("pak")
pak::pak("mountainMath/safeconduct")
```
