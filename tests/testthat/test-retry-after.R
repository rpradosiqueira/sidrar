test_that("Retry-After seconds are honored silently for 429 and 503", {
  old_options <- options(sidrar.retries = 3L)
  on.exit(options(old_options), add = TRUE)
  status <- 429L
  calls <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(..., times, quiet) {
      expect_identical(times, 1L)
      expect_true(quiet)
      calls <<- calls + 1L
      if (calls == 1L) {
        fake_http_response(
          status = status,
          headers = list("rEtRy-AfTeR" = " 60 ")
        )
      } else {
        fake_http_response(body = "success")
      }
    },
    .package = "httr"
  )
  for (status in c(429L, 503L)) {
    calls <- 0L
    waits <- numeric()
    expect_silent(result <- sidrar:::.sidra_request("https://example.test"))
    expect_identical(result, "success")
    expect_identical(calls, 2L)
    expect_identical(waits, 60)
  }
})

test_that("real httr attempts honor Retry-After without its quiet-mode backoff", {
  old_options <- options(sidrar.retries = 3L)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    request_perform = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = if (calls == 1L) 429L else 200L,
        headers = list("retry-after" = "60")
      )
    },
    backoff_full_jitter = function(...) {
      stop("httr must not run its own retry loop")
    },
    .package = "httr"
  )
  expect_silent(sidrar:::.sidra_request("https://example.test"))
  expect_identical(calls, 2L)
  expect_identical(waits, 60)
})

test_that("HTTP-date Retry-After supports standard and obsolete HTTP formats", {
  now <- as.POSIXct("2026-09-17 12:00:00", tz = "GMT")
  dates <- c(
    "Thu, 17 Sep 2026 12:00:30 GMT",
    "Thursday, 17-Sep-26 12:00:30 GMT",
    "Thu Sep 17 12:00:30 2026"
  )
  for (date in dates) {
    response <- fake_http_response(headers = list("retry-after" = date))
    expect_identical(sidrar:::.sidra_retry_after(response, now), 30)
  }
  # Prefer the conservative wait when the client's clock is ahead.
  response <- fake_http_response(headers = list(
    "Retry-After" = dates[[1L]],
    "Date" = "Thu, 17 Sep 2026 12:00:00 GMT"
  ))
  expect_identical(sidrar:::.sidra_retry_after(response, now + 60), 30)
  expect_identical(sidrar:::.sidra_retry_after(response, now - 5), 35)
})

test_that("HTTP-date Retry-After controls the request loop", {
  old_options <- options(sidrar.retries = 2L)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    .sidra_retry_now = function() as.POSIXct("2026-09-17 12:00:00", tz = "GMT"),
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = if (calls == 1L) 503L else 200L,
        headers = list("Retry-After" = "Thu, 17 Sep 2026 12:00:20 GMT")
      )
    },
    .package = "httr"
  )
  expect_silent(sidrar:::.sidra_request("https://example.test"))
  expect_identical(waits, 20)
  expect_identical(calls, 2L)
})

test_that("invalid, missing and past Retry-After values keep finite backoff", {
  old_options <- options(sidrar.retries = 2L)
  on.exit(options(old_options), add = TRUE)
  cases <- list(
    list(), list("retry-after" = ""), list("retry-after" = NA_character_),
    list("retry-after" = "garbage"), list("retry-after" = "-20"),
    list("retry-after" = "NaN"), list("retry-after" = "Inf"),
    list("retry-after" = "0.5"), list("retry-after" = "1e3"),
    list("retry-after" = c("20", "40")), list("retry-after" = 20),
    list("retry-after" = "20", "Retry-After" = "40"),
    list("retry-after" = "Thu, 17 Sep 2026 12:00:30 GMT trailing"),
    list("retry-after" = "Thu, 99 Sep 2026 12:00:30 GMT"),
    list("retry-after" = "Wed, 16 Sep 2026 12:00:00 GMT"),
    list("retry-after" = "0")
  )
  headers <- list()
  calls <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    .sidra_retry_now = function() as.POSIXct("2026-09-17 12:00:00", tz = "GMT"),
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.75
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = if (calls == 1L) 429L else 200L, headers = headers
      )
    },
    .package = "httr"
  )
  for (headers in cases) {
    calls <- 0L
    waits <- numeric()
    expect_silent(sidrar:::.sidra_request("https://example.test"))
    expect_identical(waits, 0.75)
    expect_identical(calls, 2L)
  }
})

test_that("excessive Retry-After fails without sleeping or another request", {
  old_options <- options(sidrar.retries = 3L)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  value <- "61"
  testthat::local_mocked_bindings(
    .sidra_retry_now = function() as.POSIXct("2026-09-17 12:00:00", tz = "GMT"),
    .sidra_retry_sleep = function(...) stop("Must not sleep"),
    .sidra_retry_pause = function(...) stop("Must not calculate backoff")
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = 503L, body = "unavailable",
        headers = list("retry-after" = value)
      )
    },
    .package = "httr"
  )
  for (value in c("61", "3600", strrep("9", 400L),
                  "Thu, 17 Sep 2026 13:00:00 GMT")) {
    calls <- 0L
    error <- expect_error(
      sidrar:::.sidra_request("https://example.test"),
      "No further request was sent",
      class = "sidrar_retry_after_error"
    )
    expect_s3_class(error, "sidrar_http_error")
    expect_identical(calls, 1L)
    expect_identical(error$status_code, 503L)
    expect_identical(error$response_body, "unavailable")
    expect_identical(error$url, "https://example.test")
    expect_identical(error$retry_after_header, value)
    expect_gt(error$retry_after, 60)
    expect_identical(error$attempts, 1L)
  }
})

test_that("retry count is the total attempt count and does not sleep at exhaustion", {
  old_options <- options(sidrar.retries = 3L)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  value <- "2"
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(status = 429L, headers = list("Retry-After" = value))
    },
    .package = "httr"
  )
  for (attempts in c(1L, 2L, 3L)) {
    options(sidrar.retries = attempts)
    calls <- 0L
    waits <- numeric()
    error <- expect_error(
      sidrar:::.sidra_request("https://example.test"), class = "sidrar_http_error"
    )
    expect_identical(calls, attempts)
    expect_identical(waits, rep(2, attempts - 1L))
    expect_identical(error$retry_after, 2)
    expect_identical(error$retry_after_header, "2")
    expect_identical(error$status_code, 429L)
    expect_identical(error$attempts, attempts)
  }
  options(sidrar.retries = 1L)
  calls <- 0L
  waits <- numeric()
  value <- "61"
  error <- expect_error(
    sidrar:::.sidra_request("https://example.test"), class = "sidrar_http_error"
  )
  expect_false(inherits(error, "sidrar_retry_after_error"))
  expect_identical(error$retry_after, 61)
  expect_identical(error$retry_after_header, "61")
  expect_identical(error$attempts, 1L)
  expect_identical(calls, 1L)
  expect_length(waits, 0L)
})

test_that("browser challenges and terminal errors never trigger backoff", {
  calls <- 0L
  response <- NULL
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(...) stop("Must not sleep"),
    .sidra_retry_pause = function(...) stop("Must not calculate backoff")
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      response
    },
    .package = "httr"
  )
  for (status in c(200L, 403L, 503L)) {
    calls <- 0L
    response <- fake_http_response(
      status = status, headers = list(
        "cf-mitigated" = "challenge", "retry-after" = "300"
      )
    )
    expect_error(sidrar:::.sidra_request("https://example.test"),
                 class = "sidrar_challenge_error")
    expect_identical(calls, 1L)
  }
  for (status in c(400L, 401L, 403L, 404L)) {
    calls <- 0L
    response <- fake_http_response(status = status)
    expect_error(sidrar:::.sidra_request("https://example.test"),
                 class = "sidrar_http_error")
    expect_identical(calls, 1L)
  }
})

test_that("transport retries preserve final classifications and request config", {
  old_options <- options(sidrar.retries = 2L, sidrar.timeout = 7)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  seen <- list()
  current_error <- simpleError("Timeout was reached")
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(verb, url, ..., times, quiet) {
      calls <<- calls + 1L
      seen[[calls]] <<- list(verb = verb, url = url, args = list(...))
      expect_identical(times, 1L)
      expect_true(quiet)
      stop(current_error)
    },
    .package = "httr"
  )
  for (case in list(
    list(message = "Timeout was reached", class = "sidrar_timeout_error"),
    list(message = "SSL certificate problem", class = "sidrar_tls_error")
  )) {
    calls <- 0L
    waits <- numeric()
    current_error <- simpleError(case$message)
    error <- expect_error(
      sidrar:::.sidra_request("https://example.test"), class = case$class
    )
    expect_s3_class(error, "sidrar_http_error")
    expect_identical(calls, 2L)
    expect_identical(waits, 0.5)
    expect_identical(error$status_code, NA_integer_)
    expect_identical(error$response_body, "")
    expect_identical(error$url, "https://example.test")
    for (request in seen) {
      expect_identical(request$verb, "GET")
      expect_identical(request$url, "https://example.test")
      expect_identical(request$args[[1L]]$headers[["Accept"]], "application/json")
      expect_match(request$args[[2L]]$options$useragent, "^sidrar/")
      expect_identical(request$args[[3L]]$options$timeout_ms, 7000)
      expect_null(request$args[[3L]]$options$ssl_verifypeer)
    }
  }
})

test_that("unhinted retry backoff remains bounded", {
  for (attempt in c(1L, 2L, 3L, 1000L)) {
    pause <- sidrar:::.sidra_retry_pause(attempt)
    expect_true(is.finite(pause))
    expect_gte(pause, 0.5)
    expect_lte(pause, 4)
  }
})
