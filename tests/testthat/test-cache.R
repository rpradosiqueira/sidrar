test_that("metadata cache entries honor ttl and refresh", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  now <- as.POSIXct("2026-08-25 12:00:00", tz = "UTC")

  expect_false(
    sidrar:::.sidra_cache_get(
      "catalog", cache = TRUE, cache_dir = directory, now = now
    )$hit
  )
  sidrar:::.sidra_cache_set(
    "catalog", data.frame(code = "1"), cache = TRUE, ttl = 60,
    cache_dir = directory, now = now
  )

  cached <- sidrar:::.sidra_cache_get(
    "catalog", cache = TRUE, cache_dir = directory, now = now + 30
  )
  expect_true(cached$hit)
  expect_identical(cached$value$code, "1")
  expect_false(
    sidrar:::.sidra_cache_get(
      "catalog", cache = TRUE, refresh = TRUE,
      cache_dir = directory, now = now + 30
    )$hit
  )
  expect_false(
    sidrar:::.sidra_cache_get(
      "catalog", cache = TRUE, cache_dir = directory, now = now + 61
    )$hit
  )
})

test_that("cache inspection and clearing remain inside the selected directory", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  unrelated <- file.path(directory, "keep.txt")
  writeLines("keep", unrelated)
  unrelated_rds <- file.path(directory, "other-package.rds")
  saveRDS(list(owner = "other"), unrelated_rds)

  sidrar:::.sidra_cache_set(
    "metadata-7060", list(table = "7060"), cache = TRUE,
    cache_dir = directory
  )

  info <- sidra_cache_info(directory)
  expect_identical(info$key, "metadata-7060")
  expect_true(info$valid)
  expect_identical(sidra_cache_clear(directory), 1L)
  expect_true(file.exists(unrelated))
  expect_true(file.exists(unrelated_rds))
  expect_identical(nrow(sidra_cache_info(directory)), 0L)
})

test_that("cache options reject ambiguous inputs", {
  expect_error(
    sidrar:::.sidra_cache_get("x", cache = NA),
    "'cache' must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    sidrar:::.sidra_cache_set("x", 1, cache = TRUE, ttl = 0),
    "positive number"
  )
  expect_error(sidra_cache_info(" "), "non-empty path")
})

test_that("cached value fetches only on misses or refresh", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  calls <- 0L
  fetch <- function() {
    calls <<- calls + 1L
    paste0("value-", calls)
  }

  first <- sidrar:::.sidra_cached_value(
    "periods-1", fetch, cache = TRUE, cache_dir = directory
  )
  second <- sidrar:::.sidra_cached_value(
    "periods-1", fetch, cache = TRUE, cache_dir = directory
  )
  refreshed <- sidrar:::.sidra_cached_value(
    "periods-1", fetch, cache = TRUE, refresh = TRUE,
    cache_dir = directory
  )

  expect_identical(first, "value-1")
  expect_identical(second, "value-1")
  expect_identical(refreshed, "value-2")
  expect_identical(calls, 2L)
})

test_that("malformed, expired, and mismatched cache entries are safe misses", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  now <- as.POSIXct(Sys.time(), tz = "UTC")

  malformed <- list(
    schema_version = 1L,
    key = "malformed",
    fetched_at = now,
    expires_at = c(now + 30, now + 60),
    value = "must-not-be-used"
  )
  saveRDS(
    malformed,
    sidrar:::.sidra_cache_path("malformed", directory)
  )
  expect_false(
    sidrar:::.sidra_cache_get(
      "malformed", cache = TRUE, cache_dir = directory, now = now
    )$hit
  )

  expired <- list(
    schema_version = 1L,
    key = "expired",
    fetched_at = now - 120,
    expires_at = now - 60,
    value = "stale"
  )
  saveRDS(expired, sidrar:::.sidra_cache_path("expired", directory))

  mismatched <- list(
    schema_version = 1L,
    key = "another-key",
    fetched_at = Sys.time(),
    expires_at = Sys.time() + 3600,
    value = "wrong-entry"
  )
  saveRDS(mismatched, sidrar:::.sidra_cache_path("requested", directory))
  expect_false(
    sidrar:::.sidra_cache_get(
      "requested", cache = TRUE, cache_dir = directory, now = now
    )$hit
  )

  info <- sidra_cache_info(directory)
  expect_false(info$valid[info$key == "expired"])
  expect_false(info$valid[info$key == "requested"])
  expect_false(any(info$valid))
})

test_that("cache entries require finite chronological timestamps", {
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  entry <- list(
    schema_version = 1L,
    key = "x",
    fetched_at = now,
    expires_at = now + 60,
    value = 1
  )

  expect_true(sidrar:::.sidra_valid_cache_entry(entry))

  entry$expires_at <- entry$fetched_at - 1
  expect_false(sidrar:::.sidra_valid_cache_entry(entry))

  entry$expires_at <- structure(Inf, class = c("POSIXct", "POSIXt"))
  expect_false(sidrar:::.sidra_valid_cache_entry(entry))
})

test_that("cache replacement stages complete files in the same directory", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  sidrar:::.sidra_cache_set(
    "catalog", "old", cache = TRUE, cache_dir = directory
  )

  renames <- 0L
  testthat::local_mocked_bindings(
    .sidra_file_rename = function(from, to) {
      renames <<- renames + 1L
      if (renames == 1L) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "sidrar"
  )
  sidrar:::.sidra_cache_set(
    "catalog", "new", cache = TRUE, cache_dir = directory
  )

  path <- sidrar:::.sidra_cache_path("catalog", directory)
  expect_identical(readRDS(path)$value, "new")
  expect_identical(renames, 3L)
  expect_false(any(grepl("[.]tmp$", list.files(directory))))
})

test_that("failed cache replacement restores the previous complete entry", {
  directory <- tempfile("sidrar-cache-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  sidrar:::.sidra_cache_set(
    "catalog", "old", cache = TRUE, cache_dir = directory
  )

  renames <- 0L
  testthat::local_mocked_bindings(
    .sidra_file_rename = function(from, to) {
      renames <<- renames + 1L
      if (renames %in% c(1L, 3L)) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "sidrar"
  )
  expect_error(
    sidrar:::.sidra_cache_set(
      "catalog", "new", cache = TRUE, cache_dir = directory
    ),
    "Unable to write"
  )

  path <- sidrar:::.sidra_cache_path("catalog", directory)
  expect_identical(readRDS(path)$value, "old")
  expect_identical(renames, 4L)
  expect_false(any(grepl("[.]tmp$", list.files(directory))))
})
