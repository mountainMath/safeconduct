# Precompute the vignettes.
#
# The vignettes open a Chrome, talk to servers that refuse plain requests, and
# wait on Cloudflare checks that take minutes, none of which a package build, a
# CRAN check or the pkgdown action should depend on. So each vignette is
# *written* here, in `vignettes.orig/`, and knitted into the `vignettes/`
# directory that ships, where the `.Rmd` that R CMD build sees holds the
# results already.
#
# Both directories hold plain `.Rmd` files, so an editor treats the source as
# R Markdown; `vignettes.orig/` is .Rbuildignore'd, so none of it is shipped.
#
#   Rscript vignettes.orig/precompute.R               # all of them
#   Rscript vignettes.orig/precompute.R safeconduct   # just this one
#
# Vignettes get the default values of the `params` in their YAML header, as
# with rmarkdown::render(). A chunk that fails stops the knitting, unless it
# says `error = TRUE`, for the chunks that show a refusal.
#
# A window may open: `headed = "auto"` opens one when headless Chrome does not
# get through, and a "Verify you are human" box in it is for whoever runs this
# to click. Commit the resulting `vignettes/*.Rmd` together with any figures
# it wrote.

# Where this script lives, so it works from the package root or from anywhere
# else. `--file=` is absent when the script is sourced rather than Rscript'ed,
# in which case fall back to the package root's own layout.
this <- grep("^--file=", commandArgs(), value = TRUE)
src_dir <- if (length(this)) dirname(sub("^--file=", "", this[1])) else
  "vignettes.orig"
src_dir <- normalizePath(src_dir, mustWork = TRUE)
out_dir <- normalizePath(file.path(src_dir, "..", "vignettes"), mustWork = TRUE)

devtools::load_all(file.path(src_dir, ".."), quiet = TRUE)

# The package shows its tips only in an interactive session and only when
# called from the console, neither of which knitting is. The vignette is a
# transcript of a console session, so show them as the console would.
ns <- asNamespace("safeconduct")
unlockBinding("bf_hints_on", ns)
assign("bf_hints_on", function(quiet) !isTRUE(quiet), envir = ns)
lockBinding("bf_hints_on", ns)

available <- sub("\\.Rmd$", "", list.files(src_dir, pattern = "\\.Rmd$"))
wanted <- sub("\\.Rmd$", "", commandArgs(trailingOnly = TRUE))
if (!length(wanted)) wanted <- available

unknown <- setdiff(wanted, available)
if (length(unknown)) {
  stop("No such vignette source: ", paste(unknown, collapse = ", "),
       ". Available: ", paste(available, collapse = ", "))
}

# knit from within `vignettes/`, because `fig.path` is relative to the working
# directory and the figures have to land beside the `.Rmd` that ships.
old <- setwd(out_dir)
on.exit(setwd(old), add = TRUE)

for (v in wanted) {
  message("knitting ", v)
  # figures of an earlier run that the vignette no longer produces would ship otherwise
  unlink(list.files(".", pattern = paste0("^", v, "-.*\\.png$")))
  src <- file.path(src_dir, paste0(v, ".Rmd"))
  # knitr::knit(), unlike rmarkdown::render(), doesn't define the `params` of
  # the YAML header, so set them up the same way, with their default values.
  env <- new.env(parent = globalenv())
  env$params <- lapply(knitr::knit_params(readLines(src, warn = FALSE)),
                       function(p) p$value)
  # a failing chunk would otherwise end up as an error message in the vignette
  knitr::opts_chunk$set(error = FALSE)
  # the package's once-per-host tips and kept cookies start over for each
  # vignette, so that what a vignette shows doesn't depend on which others
  # were knitted before it
  rm(list = ls(ns$bf_hints), envir = ns$bf_hints)
  rm(list = ls(ns$bf_clearance_cache), envir = ns$bf_clearance_cache)
  knitr::knit(src, paste0(v, ".Rmd"), envir = env)

  # Say so in the file itself, so an edit to the wrong copy is obvious.
  lines <- readLines(paste0(v, ".Rmd"), warn = FALSE)
  end <- if (identical(trimws(lines[1]), "---")) {
    which(trimws(lines[-1]) == "---")[1] + 1L
  } else 0L
  writeLines(append(lines, c(
    paste0("<!-- Generated from vignettes.orig/", v, ".Rmd -- do not edit. -->"),
    "<!-- Rebuild with: Rscript vignettes.orig/precompute.R -->"),
    after = end), paste0(v, ".Rmd"))
}
