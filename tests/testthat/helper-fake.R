# A stand-in for a chromote session: enough of the CDP surface that the
# download loop and the clearance wait touch, scripted in advance.
#
# `progress` is a list of `Browser.downloadProgress` events to deliver, one per
# `Browser.getVersion()` call (the loop's event pump). An event with state
# `completed` also writes the file Chrome would have written.
fake_session <- function(progress = list(), cookies = list(), payload = "data") {
  callback <- NULL
  n <- 0L
  dir <- NULL
  session <- new.env()
  session$calls <- character()
  session$Browser <- list(
    setDownloadBehavior = function(behavior, downloadPath, eventsEnabled) {
      dir <<- downloadPath
      session$calls <- c(session$calls, "setDownloadBehavior")
      list()
    },
    downloadProgress = function(callback_, wait_) {
      callback <<- callback_
      function() invisible(NULL)
    },
    getVersion = function() {
      n <<- n + 1L
      if (n <= length(progress)) {
        ev <- progress[[n]]
        if (identical(ev$state, "completed"))
          writeLines(payload, file.path(dir, ev$guid))
        callback(ev)
      }
      list(product = "Chrome/1")
    },
    cancelDownload = function(guid) {
      session$calls <- c(session$calls, paste0("cancel:", guid))
      list()
    })
  session$Page <- list(navigate = function(url, wait_) {
    session$calls <- c(session$calls, paste0("navigate:", url))
    list()
  })
  session$Network <- list(
    enable = function() list(),
    getCookies = function(urls) list(cookies = cookies))
  session$Runtime <- list(
    evaluate = function(expr) list(result = list(value = "Mozilla/5.0 (fake)")))
  session
}

fake_chrome <- function(session = fake_session(), headed = TRUE) {
  closed <- FALSE
  list(session = session, headed = headed,
       close = function() closed <<- TRUE,
       is_closed = function() closed)
}

a_clearance <- function(url = "https://example.org/x.zip") {
  bf_clearance(url, "Mozilla/5.0 (fake)", "cf_clearance=abc; other=1")
}

reset_cache <- function() {
  rm(list = ls(bf_clearance_cache), envir = bf_clearance_cache)
}
