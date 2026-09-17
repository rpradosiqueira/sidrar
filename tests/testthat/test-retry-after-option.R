test_that("custom Retry-After limits permit waits through their boundary", {
  old_options <- options(sidrar.retries = 2L, sidrar.retry_after_max = 120)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  value <- "90"
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = if (calls == 1L) 429L else 200L,
        body = "success", headers = list("Retry-After" = value)
      )
    },
    .package = "httr"
  )

  cases <- list(
    list(maximum = 120, delay = 90),
    list(maximum = 120, delay = 120),
    list(maximum = 2.5, delay = 2)
  )
  for (case in cases) {
    options(sidrar.retry_after_max = case$maximum)
    calls <- 0L
    waits <- numeric()
    value <- as.character(case$delay)
    expect_silent(result <- sidrar:::.sidra_request("https://example.test"))
    expect_identical(result, "success")
    expect_identical(calls, 2L)
    expect_identical(waits, case$delay)
  }
})

test_that("exceeding a configured Retry-After limit never retries early", {
  old_options <- options(sidrar.retries = 3L, sidrar.retry_after_max = NULL)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  value <- "61"
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(...) stop("Must not sleep"),
    .sidra_retry_pause = function(...) stop("Must not calculate backoff")
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = 503L, body = "unavailable",
        headers = list("Retry-After" = value)
      )
    },
    .package = "httr"
  )

  cases <- list(
    list(option = NULL, maximum = 60, delay = 61),
    list(option = 120, maximum = 120, delay = 121),
    list(option = 10, maximum = 10, delay = 20),
    list(option = 2.5, maximum = 2.5, delay = 3),
    list(option = 0.25, maximum = 0.25, delay = 1)
  )
  for (case in cases) {
    options(sidrar.retry_after_max = case$option)
    calls <- 0L
    value <- as.character(case$delay)
    error <- expect_error(
      sidrar:::.sidra_request("https://example.test"),
      "No further request was sent", class = "sidrar_retry_after_error"
    )
    expect_s3_class(error, "sidrar_http_error")
    expect_identical(calls, 1L)
    expect_identical(error$status_code, 503L)
    expect_identical(error$url, "https://example.test")
    expect_identical(error$response_body, "unavailable")
    expect_identical(error$retry_after, case$delay)
    expect_equal(error$retry_after_max, case$maximum)
    expect_identical(error$retry_after_header, value)
    expect_identical(error$attempts, 1L)
    expect_match(conditionMessage(error), as.character(case$maximum), fixed = TRUE)
  }
})

test_that("HTTP-date waits use the same configured limit as delay-seconds", {
  old_options <- options(sidrar.retries = 2L, sidrar.retry_after_max = 120)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  date <- "Thu, 17 Sep 2026 12:01:30 GMT"
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
        headers = list("Retry-After" = date)
      )
    },
    .package = "httr"
  )

  expect_silent(sidrar:::.sidra_request("https://example.test"))
  expect_identical(calls, 2L)
  expect_identical(waits, 90)

  calls <- 0L
  waits <- numeric()
  date <- "Thu, 17 Sep 2026 12:02:01 GMT"
  error <- expect_error(
    sidrar:::.sidra_request("https://example.test"),
    class = "sidrar_retry_after_error"
  )
  expect_identical(calls, 1L)
  expect_length(waits, 0L)
  expect_identical(error$retry_after, 121)
  expect_equal(error$retry_after_max, 120)
  expect_identical(error$retry_after_header, date)
  expect_match(conditionMessage(error), "120", fixed = TRUE)
})

test_that("invalid Retry-After limit options restore the 60-second default", {
  old_options <- options(sidrar.retries = 2L, sidrar.retry_after_max = NULL)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L
  waits <- numeric()
  value <- "61"
  testthat::local_mocked_bindings(
    .sidra_retry_sleep = function(seconds) waits <<- c(waits, seconds),
    .sidra_retry_pause = function(...) 0.5
  )
  testthat::local_mocked_bindings(
    RETRY = function(...) {
      calls <<- calls + 1L
      fake_http_response(
        status = if (calls == 1L) 429L else 200L,
        headers = list("Retry-After" = value)
      )
    },
    .package = "httr"
  )

  invalid <- list(
    NULL, NA_real_, NA_integer_, NaN, Inf, -Inf, -1, 0,
    "120", c(60, 120), numeric(), list(120), list(NULL), TRUE, 1 + 1i
  )
  for (option in invalid) {
    options(sidrar.retry_after_max = option)
    calls <- 0L
    waits <- numeric()
    value <- "61"
    error <- expect_error(
      sidrar:::.sidra_request("https://example.test"),
      class = "sidrar_retry_after_error"
    )
    expect_identical(calls, 1L)
    expect_length(waits, 0L)
    expect_equal(error$retry_after_max, 60)
    expect_identical(error$retry_after, 61)

    calls <- 0L
    waits <- numeric()
    value <- "60"
    expect_silent(sidrar:::.sidra_request("https://example.test"))
    expect_identical(calls, 2L)
    expect_identical(waits, 60)
  }
})
