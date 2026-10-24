# safeconduct

Fetch files through a real browser when a plain request is refused.

Some servers answer any request that does not come from a web browser with a
browser check instead of the file. Cloudflare's managed challenge is the common
case: a `403` page that runs a script, sets a `cf_clearance` cookie, and
reloads. `curl`, `download.file()` and friends never get past it. A browser
does, and this package lets a browser do what a browser is allowed to do:

```r
safeconduct::fetch(
  "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/rnf-frr/files-fichiers/lrnf000r21a_e.zip",
  "lrnf000r21a_e.zip")
```

If the server serves the file to a plain request, that is what happens and no
browser is involved. If it answers with a browser check, a Chrome window with
a throwaway profile is opened on the host, the check runs, and the file is
downloaded: through `curl` carrying the clearance the browser was given where
the server accepts that, and in the browser window itself where it does not.
A clearance that `curl` can use is kept for the rest of the session, so later
files from the same host need no window.

Nothing is simulated. When the check shows a "Verify you are human" box, the
window stays on screen for up to two minutes for the person at the screen to
click it. Nothing in this package clicks it for them.

## Limitations

This package provides a way past browser checks in interactive sessions. It does not help where a download needs to be fully automated: a check that asks for a click waits for a person, and a headless browser does not pass.

## Prerequisites

* [Chrome or Chromium](https://www.google.com/chrome/), found by
  `chromote::find_chrome()`. Set the `CHROMOTE_CHROME` environment variable if
  it is somewhere unusual.
* A display to show the window on. Without one Chrome runs headless, which, as
  of October 2026, Cloudflare's check does not pass.

`options(safeconduct.browser = FALSE)` never opens a browser; `fetch()` then
raises a classed `safeconduct_unavailable` error for a challenged URL, which a
calling package can catch and turn into instructions to download by hand.

## For package authors

The errors are classed under `safeconduct_error`:

| class | meaning |
|---|---|
| `safeconduct_unavailable` | no browser can be opened here; the message says what is missing |
| `safeconduct_not_passed` | the check was not passed in time |
| `safeconduct_download_error` | the transfer failed |

`clearance(url)` returns the browser's user-agent string and cookies for a
host, and `handle(clearance)` puts them on a `curl` handle, for requests this
package does not make itself. `probe(url)` says what a server serves at a URL,
and `is_challenge(headers)` whether a response is a browser check.

## Installation

```r
# install.packages("pak")
pak::pak("mountainMath/safeconduct")
```
