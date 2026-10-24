test_that("a Cloudflare challenge is recognized by its header", {
  expect_true(is_challenge("HTTP/2 403\r\ncf-mitigated: challenge\r\n\r\n"))
  expect_true(is_challenge(list(`cf-mitigated` = "Challenge")))
  expect_false(is_challenge("HTTP/2 403\r\ncontent-type: text/html\r\n\r\n"))
  expect_false(is_challenge(list()))
  expect_true(is_challenge(charToRaw("HTTP/2 403\r\ncf-mitigated: challenge\r\n\r\n")))
})

test_that("a response is described from its headers", {
  d <- bf_describe_response(206, paste0(
    "HTTP/2 206\r\nContent-Type: Application/ZIP\r\n",
    "content-range: bytes 0-0/310000000\r\ncontent-length: 1\r\n\r\n"))
  expect_true(d$reachable)
  expect_identical(d$status, 206L)
  expect_false(d$challenged)
  expect_identical(d$type, "application/zip")
  expect_identical(d$size, 310000000)

  d <- bf_describe_response(200, "HTTP/2 200\r\ncontent-length: 4096\r\n\r\n")
  expect_identical(d$size, 4096)
  expect_identical(d$type, NA_character_)

  d <- bf_describe_response(403, "HTTP/2 403\r\ncf-mitigated: challenge\r\n\r\n")
  expect_true(d$challenged)
  expect_identical(d$size, NA_real_)
})

test_that("an unreachable URL probes as unreachable", {
  d <- probe("file:///this/path/does/not/exist/at/all.zip")
  expect_false(d$reachable)
  expect_identical(d$status, NA_integer_)
  expect_false(d$challenged)
})

test_that("a handle takes a clearance", {
  expect_s3_class(handle(), "curl_handle")
  expect_s3_class(handle(a_clearance(), range = "0-0"), "curl_handle")
})

test_that("URLs are split as expected", {
  expect_identical(bf_url_host("https://www12.statcan.gc.ca/a/b.zip"),
                   "www12.statcan.gc.ca")
  expect_identical(bf_url_host("http://example.org"), "example.org")
  expect_identical(bf_host_root("https://www12.statcan.gc.ca/a/b.zip"),
                   "https://www12.statcan.gc.ca/")
  expect_identical(bf_host_root("https://example.org"), "https://example.org/")
})
