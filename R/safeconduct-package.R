#' @keywords internal
"_PACKAGE"

# jsonlite is used for two things, decoding the base64 body a browser read
# returns and `page_json()`; it is a dependency of chromote already, so it
# costs nothing. xml2 is only suggested, for `page_html()`.

`%||%` <- function(x, y) if (is.null(x)) y else x

sc_message <- function(quiet, ...) {
  if (!isTRUE(quiet)) message(...)
  invisible(NULL)
}

# One class per thing that can go wrong, all under `safeconduct_error`, so a
# caller can tell "no browser here" from "the check was not passed" from "the
# transfer failed" without parsing messages. Further fields (`reason`) ride
# along for the package's own use.
sc_error <- function(class, message, ...) {
  structure(class = c(class, "safeconduct_error", "error", "condition"),
            list(message = message, call = NULL, ...))
}

sc_url_host <- function(url) sub("^https?://([^/]+)(/.*)?$", "\\1", url)

# The page to pass the check on: the host's root. A clearance cookie is scoped
# to the host, so any page serves, and the root is one that cannot start a
# download in the browser the way the file's own URL would.
sc_host_root <- function(url) paste0(sub("^(https?://[^/]+)(/.*)?$", "\\1", url), "/")

# Tips for next time.
#
# Once `fetch()` has found out how a host has to be handled, the person at the
# console can be told how to skip the finding out next time: `headed = TRUE`
# when only a window got through, or a session that keeps one browser open
# for several files or reads. A tip is shown once per host and R session, in
# interactive sessions only, never when `quiet`, and only when the call came
# from the console -- the top level, a script, or a function written there --
# rather than from another package, whose user cannot act on the tip and
# whose author should not have to turn it off. Tips are messages with the
# class `safeconduct_hint` as well, for anyone who wants to muffle them.
sc_hints <- new.env(parent = emptyenv())

sc_interactive <- function() interactive()

# Whether the public function was called from the console: no function on the
# call stack belongs to a package other than this one and base (so `lapply()`
# and the like at the console still count as the console).
sc_user_call <- function(envs = sc_stack_envs()) {
  for (env in envs) {
    if (is.null(env)) next
    top <- topenv(env)
    if (!identical(top, globalenv()) && !identical(top, .BaseNamespaceEnv) &&
        !identical(top, topenv(environment(sc_user_call))))
      return(FALSE)
  }
  TRUE
}

sc_stack_envs <- function() {
  n <- sys.nframe() - 1L
  lapply(seq_len(n), function(i) environment(sys.function(i)))
}

sc_hints_on <- function(quiet) {
  !isTRUE(quiet) && sc_interactive() && sc_user_call()
}

sc_hint <- function(hints, key, ...) {
  if (!isTRUE(hints) || !is.null(sc_hints[[key]])) return(invisible(FALSE))
  sc_hints[[key]] <- TRUE
  message(structure(class = c("safeconduct_hint", "message", "condition"),
                    list(message = paste0("  Tip: ", paste0(...), "\n"), call = NULL)))
  invisible(TRUE)
}

# The session tip, for `fetch()` and `read_page()`, naming the mode that
# worked when it had to be found out.
sc_hint_session <- function(hints, url, fn, chrome) {
  host <- sc_url_host(url)
  sc_hint(hints, paste0("session:", host),
    "for more from '", host, "', keep one browser open instead of one per call:",
    "\n       s <- browser_session(\"", sc_host_root(url), "\"",
    if (isTRUE(chrome$fell_back)) ", headed = TRUE",
    if (!isTRUE(chrome$useragent)) paste0(", useragent = ", deparse(chrome$useragent)),
    ")",
    "\n       ", fn, "(url, ", if (fn == "fetch") "destfile, ", "session = s)",
    "\n       close(s)")
}
