# A stand-in for a chromote session: enough of the CDP surface that the
# document wait, the download loop and the in-browser read touch, scripted in
# advance.
#
# `events` is a list of events to deliver, one per `Browser.getVersion()` call
# (the loops' event pump). Make them with `dl_event()` (a
# `Browser.downloadProgress` event; `completed` also writes the file Chrome
# would have written) and `doc_event()` (a `Network.responseReceived` event
# for a document). `bodies` maps request ids to the bodies
# `Network.getResponseBody()` hands out; `read` is what `Runtime.evaluate()`
# returns for anything but the user-agent query, either a value or a
# function of the expression.
fake_session <- function(events = list(), cookies = list(), payload = "data",
                         bodies = list(), read = NULL) {
  dl_callback <- NULL
  doc_callback <- NULL
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
      dl_callback <<- callback_
      function() invisible(NULL)
    },
    getVersion = function() {
      n <<- n + 1L
      if (n <= length(events)) {
        ev <- events[[n]]
        if (!is.null(ev$download)) {
          if (identical(ev$download$state, "completed"))
            writeLines(payload, file.path(dir, ev$download$guid))
          if (!is.null(dl_callback)) dl_callback(ev$download)
        }
        if (!is.null(ev$document) && !is.null(doc_callback)) doc_callback(ev$document)
      }
      list(product = "Chrome/1")
    },
    cancelDownload = function(guid) {
      session$calls <- c(session$calls, paste0("cancel:", guid))
      list()
    })
  session$Page <- list(
    navigate = function(url, wait_) {
      session$calls <- c(session$calls, paste0("navigate:", url))
      list()
    },
    getFrameTree = function() list(frameTree = list(frame = list(id = "main"))))
  session$Network <- list(
    enable = function() list(),
    getCookies = function(urls) list(cookies = cookies),
    responseReceived = function(callback_, wait_) {
      doc_callback <<- callback_
      function() invisible(NULL)
    },
    getResponseBody = function(requestId) {
      body <- bodies[[requestId]]
      if (is.null(body)) stop("No resource with given identifier found")
      list(body = body, base64Encoded = FALSE)
    })
  session$Runtime <- list(
    evaluate = function(expr, ...) {
      if (identical(expr, "navigator.userAgent"))
        return(list(result = list(value = "Mozilla/5.0 (fake)")))
      session$calls <- c(session$calls, "evaluate")
      value <- if (is.function(read)) read(expr) else read
      list(result = list(value = value))
    })
  session
}

dl_event <- function(state, guid, received = 0, total = 0) {
  list(download = list(state = state, guid = guid, receivedBytes = received,
                       totalBytes = total))
}

doc_event <- function(status, headers = list(), frameId = "main",
                      requestId = "r1", url = "https://example.org/",
                      mimeType = "text/html") {
  list(document = list(
    type = "Document", frameId = frameId, requestId = requestId,
    response = list(status = status, headers = headers, url = url,
                    mimeType = mimeType)))
}

block_page <- paste0("<html><head><title>Attention Required! | Cloudflare",
                     "</title></head><body><div id=\"cf-error-details\">",
                     "Sorry, you have been blocked</div></body></html>")
cf_headers <- list(server = "cloudflare", `content-type` = "text/html")
challenge_headers <- c(cf_headers, list(`cf-mitigated` = "challenge"))

# What the read script returns for a response.
read_result <- function(status = 200, body = '{"a":1}',
                        headers = list(`content-type` = "application/json")) {
  list(ok = TRUE, status = status, url = "https://example.org/feed.json",
       headers = headers, body = jsonlite::base64_enc(charToRaw(body)))
}

fake_chrome <- function(session = fake_session(), headed = TRUE) {
  closed <- FALSE
  session$headed <- headed
  list(session = session, headed = headed, wait = if (headed) 120 else 30,
       close = function() closed <<- TRUE,
       is_closed = function() closed)
}

a_clearance <- function(url = "https://example.org/x.zip") {
  bf_clearance(url, "Mozilla/5.0 (fake)", "cf_clearance=abc; other=1")
}

reset_cache <- function() {
  rm(list = ls(bf_clearance_cache), envir = bf_clearance_cache)
}
