checkpoint_queries <- function() {
  lapply(2020:2022, function(year) {
    sidra_query(api = paste0("/t/1/n1/1/v/1/p/", year, "/h/n"))
  })
}

checkpoint_response <- function(url) {
  year <- sub(".*?/p/([0-9]+).*", "\\1", url)
  list(text = paste0('[{"D1C":"', year, '","V":"1"}]'),
       url = url, response_header = FALSE)
}

test_that("checkpoint resumes only verified completed batches", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  calls <- character()
  fail <- TRUE
  local_mocked_bindings(.sidra_values_request = function(url) {
    calls <<- c(calls, url)
    if (fail && grepl("2021", url)) stop("interrupted")
    checkpoint_response(url)
  }, .package = "sidrar")
  queries <- checkpoint_queries()
  err <- expect_error(sidra_collect(queries, checkpoint = directory), "interrupted")
  expect_identical(err$completed_batches, 1L)
  expect_identical(err$batch_index, 2L)
  expect_false(dir.exists(file.path(directory, ".sidrar-lock")))
  fail <- FALSE
  calls <- character()
  result <- sidra_collect(queries, checkpoint = directory, provenance = TRUE)
  expect_length(calls, 2L)
  expect_identical(result$D1C, as.character(2020:2022))
  expect_identical(sidra_provenance(result)$resumed, c(TRUE, FALSE, FALSE))
  times <- sidra_provenance(result)$batch_accessed_at
  calls <- character()
  resumed <- sidra_collect(queries, checkpoint = directory, provenance = TRUE)
  expect_length(calls, 0L)
  expect_identical(sidra_provenance(resumed)$batch_accessed_at, times)
  expect_true(all(sidra_provenance(resumed)$resumed))
  expect_equal(resumed[, names(result)], result[, names(result)], ignore_attr = TRUE)
})

test_that("checkpoint refuses changed configuration without overwriting", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(.sidra_values_request = checkpoint_response, .package = "sidrar")
  queries <- checkpoint_queries()
  sidra_collect(queries, checkpoint = directory)
  manifest <- file.path(directory, "sidrar-checkpoint.rds")
  checksum <- tools::md5sum(manifest)
  expect_error(sidra_collect(rev(queries), checkpoint = directory),
               class = "sidrar_checkpoint_error")
  expect_error(sidra_collect(queries, checkpoint = directory, value_type = "both"),
               class = "sidrar_checkpoint_error")
  expect_error(sidra_collect(queries, checkpoint = directory, batch_size = 1),
               class = "sidrar_checkpoint_error")
  expect_error(sidra_collect(queries, checkpoint = directory, resume = FALSE),
               "already exists")
  expect_identical(tools::md5sum(manifest), checksum)
  expect_false(dir.exists(file.path(directory, ".sidrar-lock")))
})

test_that("corrupt batches, foreign directories and concurrent writers fail safely", {
  directory <- tempfile()
  foreign <- tempfile()
  on.exit(unlink(c(directory, foreign), recursive = TRUE), add = TRUE)
  local_mocked_bindings(.sidra_values_request = checkpoint_response, .package = "sidrar")
  queries <- checkpoint_queries()
  sidra_collect(queries, checkpoint = directory)
  path <- file.path(directory, "batch-000001.rds")
  saveRDS(list(data = "damaged"), path)
  expect_error(sidra_collect(queries, checkpoint = directory), "checksum")
  expect_false(dir.exists(file.path(directory, ".sidrar-lock")))
  dir.create(file.path(directory, ".sidrar-lock"))
  expect_error(sidra_collect(queries, checkpoint = directory), "locked")
  expect_true(dir.exists(file.path(directory, ".sidrar-lock")))
  dir.create(foreign)
  saveRDS("user data", file.path(foreign, "mine.rds"))
  expect_error(sidra_collect(queries, checkpoint = foreign), "not empty")
  expect_identical(readRDS(file.path(foreign, "mine.rds")), "user data")
})

test_that("schema drift is not persisted and successful batches survive", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(.sidra_values_request = function(url) {
    result <- checkpoint_response(url)
    if (grepl("2021", url)) result$text <- '[{"Different":"1","V":"2"}]'
    result
  }, .package = "sidrar")
  expect_error(sidra_collect(checkpoint_queries(), checkpoint = directory),
               class = "sidrar_batch_schema_error")
  expect_true(file.exists(file.path(directory, "batch-000001.meta.rds")))
  expect_false(file.exists(file.path(directory, "batch-000002.meta.rds")))
  expect_false(file.exists(file.path(directory, "batch-000002.rds")))
})

test_that("checkpoint input validation occurs before networking", {
  local_mocked_bindings(.sidra_values_request = function(...) stop("network"),
                        .package = "sidrar")
  for (bad in list(NA, "", character(), 1, c("a", "b"))) {
    expect_error(sidra_collect(checkpoint_queries(), checkpoint = bad), "checkpoint")
  }
  for (bad in list(NA, NULL, 1, c(TRUE, FALSE))) {
    expect_error(sidra_collect(checkpoint_queries(), resume = bad), "resume")
  }
  for (bad in list(0, -1, Inf, NA, 1.5, "2")) {
    expect_error(sidra_collect(checkpoint_queries(), batch_size = bad), "positive integer")
  }
})

test_that("automatic period splitting freezes relative selectors for resume", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  period_calls <- 0L
  calls <- character()
  fail <- TRUE
  local_mocked_bindings(
    sidra_periods = function(table, ...) {
      period_calls <<- period_calls + 1L
      data.frame(table_id = "1", period_id = as.character(2020:2022))
    },
    .sidra_values_request = function(url) {
      calls <<- c(calls, url)
      if (fail && grepl("2021", url)) stop("interrupted")
      checkpoint_response(url)
    }, .package = "sidrar"
  )
  url <- "/t/1/n1/1/v/1/p/all/h/n"
  expect_error(sidra_collect(url, batch_size = 1, checkpoint = directory), "interrupted")
  expect_identical(period_calls, 1L)
  fail <- FALSE
  calls <- character()
  result <- sidra_collect(url, batch_size = 1, checkpoint = directory)
  expect_identical(period_calls, 1L)
  expect_length(calls, 2L)
  expect_identical(result$D1C, as.character(2020:2022))
})

test_that("checkpoint freezes relative periods even without batch_size", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  period_calls <- 0L
  fail <- TRUE
  urls <- character()
  local_mocked_bindings(
    sidra_periods = function(table, ...) {
      period_calls <<- period_calls + 1L
      data.frame(table_id = "1", period_id = c("2020", "2021"))
    },
    .sidra_values_request = function(url) {
      urls <<- c(urls, url)
      if (fail && grepl("/v/2/", url, fixed = TRUE)) stop("interrupted")
      checkpoint_response(url)
    }, .package = "sidrar"
  )
  queries <- list(sidra_query(api = "/t/1/n1/1/v/1/p/last/h/n"),
                  sidra_query(api = "/t/1/n1/1/v/2/p/last/h/n"))
  expect_error(sidra_collect(queries, checkpoint = directory), "interrupted")
  expect_identical(period_calls, 2L)
  fail <- FALSE
  result <- sidra_collect(queries, checkpoint = directory)
  expect_identical(period_calls, 2L)
  expect_true(all(grepl("/p/2021/", urls, fixed = TRUE)))
  expect_identical(result$D1C, rep("2021", 2L))
})

test_that("a crash before completion record leaves earlier batches resumable", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  calls <- character()
  local_mocked_bindings(.sidra_values_request = function(url) {
    calls <<- c(calls, url)
    if (grepl("2021", url)) stop("interrupted")
    checkpoint_response(url)
  }, .package = "sidrar")
  queries <- checkpoint_queries()
  expect_error(sidra_collect(queries, checkpoint = directory), "interrupted")
  # Simulate a data file saved before its completion marker could be committed.
  saveRDS(list(data = "orphan"), file.path(directory, "batch-000002.rds"))
  checksum <- tools::md5sum(file.path(directory, "sidrar-checkpoint.rds"))
  local_mocked_bindings(.sidra_values_request = checkpoint_response, .package = "sidrar")
  result <- sidra_collect(queries, checkpoint = directory, provenance = TRUE)
  expect_identical(result$D1C, as.character(2020:2022))
  expect_identical(sidra_provenance(result)$resumed, c(TRUE, FALSE, FALSE))
  expect_identical(tools::md5sum(file.path(directory, "sidrar-checkpoint.rds")), checksum)
})

test_that("resuming does not conceal incomplete-coverage warnings", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  calls <- 0L
  local_mocked_bindings(.sidra_values_request = function(url) {
    calls <<- calls + 1L
    warning(structure(list(message = "Missing selected period", call = NULL,
                           missing = list(period = "2020")),
                      class = c("sidrar_incomplete_warning", "warning", "condition")))
    checkpoint_response(url)
  }, .package = "sidrar")
  query <- checkpoint_queries()[[1L]]
  expect_warning(sidra_collect(query, checkpoint = directory),
                 class = "sidrar_incomplete_warning")
  repeated <- expect_warning(sidra_collect(query, checkpoint = directory),
                             class = "sidrar_incomplete_warning")
  expect_identical(calls, 1L)
  expect_identical(repeated$missing, list(period = "2020"))
})

test_that("warnings treated as errors do not commit a partial batch", {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(.sidra_values_request = function(url) {
    warning("incomplete coverage")
    checkpoint_response(url)
  }, .package = "sidrar")
  expect_error(sidra_collect(checkpoint_queries()[[1L]], checkpoint = directory),
               "incomplete coverage")
  expect_false(file.exists(file.path(directory, "batch-000001.meta.rds")))
  expect_false(dir.exists(file.path(directory, ".sidrar-lock")))
})
