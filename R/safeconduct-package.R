#' @keywords internal
"_PACKAGE"

`%||%` <- function(x, y) if (is.null(x)) y else x

bf_message <- function(quiet, ...) {
  if (!isTRUE(quiet)) message(...)
  invisible(NULL)
}

# One class per thing that can go wrong, all under `safeconduct_error`, so a
# caller can tell "no browser here" from "the check was not passed" from "the
# transfer failed" without parsing messages.
bf_error <- function(class, message) {
  structure(class = c(class, "safeconduct_error", "error", "condition"),
            list(message = message, call = NULL))
}

bf_url_host <- function(url) sub("^https?://([^/]+)(/.*)?$", "\\1", url)

# The page to pass the check on: the host's root. A clearance cookie is scoped
# to the host, so any page serves, and the root is one that cannot start a
# download in the browser the way the file's own URL would.
bf_host_root <- function(url) paste0(sub("^(https?://[^/]+)(/.*)?$", "\\1", url), "/")
