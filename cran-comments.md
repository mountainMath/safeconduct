## About this submission

This is a new package.

### What it does, and why

On October 2, 2026 Statistics Canada moved parts of its website behind a
Cloudflare browser check, among them the census geography and correspondence
files on www12.statcan.gc.ca. A plain HTTP request from R, through
`download.file()`, `curl` or anything built on them, has since been answered
with a 403 check page instead of the file, so those files became unreachable
from R by the usual means. That broke functionality in the cancensus package,
which downloads such files on its users' behalf, and in every script that
fetched them directly.

Statistics Canada is not alone. The City of Vancouver's Open Data portal
has moved its large file downloads behind a Cloudflare check as well. The
VancouvR package is itself unaffected, since the API it talks to and the
download links it serves work as before and nothing in the package starts a
download, but a user who follows one of those links programmatically fails
unless the download goes through safeconduct. As more public data moves
behind such checks, the need for this is broader than one package or one
agency.

safeconduct restores that access as a drop-in replacement for the download:
`safeconduct::fetch(url, destfile)` in place of `download.file(url, destfile)`.
It makes the plain request first, and when the file is served, that is the
end of it and no browser is involved. When the server answers with a browser
check or a block page, it opens a real Chrome with a throwaway profile on the
host, lets the check run, and fetches the file: through curl carrying the
clearance the browser was given where the server accepts that, and in the
browser itself where it does not. The price is a dependency on an installed
Chrome or Chromium, and, when a check insists on a click, a window that waits
for the person at the screen to click it.

Nothing is simulated. The browser is a real one, a check that asks for a
click is left to a person, and the browser is not disguised: the one change
to it is that the word "Headless" is dropped from its user-agent string by
default (the `useragent` argument controls this), because some hosts refuse
headless Chrome on that word alone while serving the same browser once it is
gone. The string still says which Chrome and platform it is. A check that
wants a click cannot be passed where no one is present, and the package says
so rather than pretending otherwise.

### For the reviewer

* The package needs Chrome or Chromium, declared in `SystemRequirements` and
  located through `chromote::find_chrome()`. Nothing that runs during
  `R CMD check` needs it: `browser_available()` reports it missing and
  `fetch()` raises a classed `safeconduct_unavailable` error.
* Tests are offline. The chromote CDP surface is replaced with a scripted
  stand-in in `tests/testthat/helper-fake.R`, so no test touches the network
  or starts a browser.
* Examples that would open a browser or go to the network are in `\dontrun{}`,
  as their results depend on what a third-party server does on the day. The
  remaining examples run offline.
* The vignette is precomputed: its chunks open a browser, talk to servers that
  refuse plain requests, and can wait minutes on a check, so it is knitted by
  the author (from `vignettes.orig/`, which is `.Rbuildignore`d) into the
  `vignettes/safeconduct.Rmd` that ships with the output baked in. Building
  it needs neither a browser nor a network.
* Nothing is written outside `tempdir()` other than the `destfile` the user
  names. The Chrome profile is a temporary directory removed when the browser
  closes, and a file downloaded in the browser lands under `tempdir()` before
  it is moved to `destfile`.
* Nothing is changed in the user's session: no options are set, and the one
  option the package reads, `safeconduct.browser`, is a kill switch that
  never opens a browser when `FALSE`.

## Test environments

* local macOS (aarch64-apple-darwin23), R 4.6.1, `R CMD check --as-cran`
* win-builder, R-release (R 4.6.2) and R-devel (2026-10-31 r90820)
* R-hub, `linux` (R-devel on Ubuntu) and `ubuntu-release`

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

## Downstream dependencies

There are none; this is a new package.
