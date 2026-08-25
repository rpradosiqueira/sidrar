#' Inspect the sidrar metadata cache
#'
#' Lists entries created explicitly by the discovery functions when their
#' `cache` argument is enabled. Value responses downloaded by [get_sidra()]
#' are never cached by these helpers.
#'
#' @param cache_dir Optional cache directory. By default, the platform-specific
#'   user cache directory for `sidrar` is used.
#'
#' @return A data frame with one row per sidrar cache file: normalized `key`,
#'   absolute `path`, byte `size`, `fetched_at`, `expires_at`, and `valid`.
#'   `valid` is `TRUE` only for a structurally readable, unexpired entry whose
#'   stored key matches its file name.
#' @seealso [sidra_cache_clear()]
#' @export
sidra_cache_info <- function(cache_dir = NULL) {
  directory <- .sidra_cache_dir(cache_dir)
  empty <- data.frame(
    key = character(),
    path = character(),
    size = numeric(),
    fetched_at = as.POSIXct(character(), tz = "UTC"),
    expires_at = as.POSIXct(character(), tz = "UTC"),
    valid = logical(),
    stringsAsFactors = FALSE
  )

  if (!dir.exists(directory)) {
    return(empty)
  }

  files <- list.files(
    directory,
    pattern = "^sidrar-[A-Za-z0-9_.-]+\\.rds$",
    full.names = TRUE
  )
  if (length(files) == 0L) {
    return(empty)
  }

  rows <- lapply(files, function(path) {
    info <- file.info(path)
    entry <- tryCatch(readRDS(path), error = function(e) NULL)
    fallback_key <- sub(
      "\\.rds$",
      "",
      sub("^sidrar-", "", basename(path))
    )
    readable <- .sidra_valid_cache_entry(entry)
    matching_key <- readable && identical(entry$key, fallback_key)
    valid <- matching_key && entry$expires_at > Sys.time()

    data.frame(
      key = if (matching_key) {
        entry$key
      } else {
        fallback_key
      },
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      size = unname(info$size),
      fetched_at = if (readable) {
        entry$fetched_at
      } else {
        as.POSIXct(NA, tz = "UTC")
      },
      expires_at = if (readable) {
        entry$expires_at
      } else {
        as.POSIXct(NA, tz = "UTC")
      },
      valid = valid,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  result <- result[order(result$key), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Clear the sidrar metadata cache
#'
#' Removes only cache entries created by `sidrar`; the cache directory itself
#' and unrelated files are preserved.
#'
#' @inheritParams sidra_cache_info
#'
#' @return The number of removed entries, invisibly.
#' @seealso [sidra_cache_info()]
#' @export
sidra_cache_clear <- function(cache_dir = NULL) {
  directory <- .sidra_cache_dir(cache_dir)
  if (!dir.exists(directory)) {
    return(invisible(0L))
  }

  files <- list.files(
    directory,
    pattern = "^sidrar-[A-Za-z0-9_.-]+\\.rds$",
    full.names = TRUE
  )
  if (length(files) == 0L) {
    return(invisible(0L))
  }

  unlink(files, force = TRUE)
  invisible(as.integer(sum(!file.exists(files))))
}

.sidra_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L ||
        is.na(cache_dir) || !nzchar(trimws(cache_dir))) {
      stop("'cache_dir' must be a non-empty path", call. = FALSE)
    }
    return(path.expand(cache_dir))
  }

  if (exists("R_user_dir", envir = asNamespace("tools"), inherits = FALSE)) {
    return(tools::R_user_dir("sidrar", which = "cache"))
  }

  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA", unset = tempdir())
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    base <- file.path(path.expand("~"), "Library", "Caches")
  } else {
    base <- Sys.getenv(
      "XDG_CACHE_HOME",
      unset = file.path(path.expand("~"), ".cache")
    )
  }
  file.path(base, "sidrar")
}

.sidra_cache_key <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  if (length(parts) == 0L || anyNA(parts)) {
    stop("cache keys must contain non-missing values", call. = FALSE)
  }
  key <- paste(as.character(parts), collapse = "-")
  key <- gsub("[^A-Za-z0-9_.-]+", "-", key)
  key <- gsub("(^-+|-+$)", "", key)
  if (!nzchar(key)) {
    stop("cache key is empty after normalization", call. = FALSE)
  }
  key
}

.sidra_cache_path <- function(key, cache_dir = NULL) {
  file.path(
    .sidra_cache_dir(cache_dir),
    paste0("sidrar-", .sidra_cache_key(key), ".rds")
  )
}

.sidra_valid_cache_entry <- function(entry) {
  is.list(entry) &&
    identical(entry$schema_version, 1L) &&
    is.character(entry$key) && length(entry$key) == 1L &&
    !is.na(entry$key) && nzchar(entry$key) &&
    inherits(entry$fetched_at, "POSIXct") &&
    length(entry$fetched_at) == 1L && !is.na(entry$fetched_at) &&
    is.finite(as.numeric(entry$fetched_at)) &&
    inherits(entry$expires_at, "POSIXct") &&
    length(entry$expires_at) == 1L && !is.na(entry$expires_at) &&
    is.finite(as.numeric(entry$expires_at)) &&
    entry$expires_at >= entry$fetched_at &&
    "value" %in% names(entry)
}

.sidra_file_rename <- function(from, to) {
  suppressWarnings(file.rename(from, to))
}

.sidra_replace_cache_file <- function(from, to) {
  if (.sidra_file_rename(from, to)) {
    return(TRUE)
  }
  if (!file.exists(to)) {
    return(FALSE)
  }

  backup <- tempfile(
    "sidrar-cache-backup-",
    tmpdir = dirname(to),
    fileext = ".tmp"
  )
  if (!.sidra_file_rename(to, backup)) {
    return(FALSE)
  }

  on.exit({
    if (file.exists(backup) && !file.exists(to)) {
      .sidra_file_rename(backup, to)
    }
  }, add = TRUE)

  if (!.sidra_file_rename(from, to)) {
    return(FALSE)
  }

  unlink(backup, force = TRUE)
  TRUE
}

.sidra_cache_get <- function(
  key,
  cache = FALSE,
  refresh = FALSE,
  cache_dir = NULL,
  now = Sys.time()
) {
  .validate_cache_options(cache, refresh)
  if (!cache || refresh) {
    return(list(hit = FALSE, value = NULL))
  }

  key <- .sidra_cache_key(key)
  path <- .sidra_cache_path(key, cache_dir)
  if (!file.exists(path)) {
    return(list(hit = FALSE, value = NULL))
  }

  entry <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!.sidra_valid_cache_entry(entry) ||
      !identical(entry$key, key) ||
      entry$expires_at <= now) {
    return(list(hit = FALSE, value = NULL))
  }

  list(hit = TRUE, value = entry$value)
}

.sidra_cache_set <- function(
  key,
  value,
  cache = FALSE,
  ttl = 1800,
  cache_dir = NULL,
  now = Sys.time()
) {
  .validate_cache_options(cache, refresh = FALSE, ttl = ttl)
  if (!cache) {
    return(invisible(value))
  }

  directory <- .sidra_cache_dir(cache_dir)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Unable to create the sidrar cache directory", call. = FALSE)
  }

  key <- .sidra_cache_key(key)
  entry <- list(
    schema_version = 1L,
    key = key,
    fetched_at = as.POSIXct(now, tz = "UTC"),
    expires_at = as.POSIXct(now + ttl, tz = "UTC"),
    value = value
  )
  path <- .sidra_cache_path(key, directory)
  temporary <- tempfile("sidrar-cache-", tmpdir = directory, fileext = ".tmp")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(entry, temporary, version = 2)
  if (!.sidra_replace_cache_file(temporary, path)) {
    stop("Unable to write the sidrar cache entry", call. = FALSE)
  }
  invisible(value)
}

.sidra_cached_value <- function(
  key,
  fetch,
  cache = FALSE,
  refresh = FALSE,
  ttl = 1800,
  cache_dir = NULL
) {
  .validate_cache_options(cache, refresh, ttl)
  if (!is.function(fetch)) {
    stop("'fetch' must be a function", call. = FALSE)
  }

  cached <- .sidra_cache_get(
    key = key,
    cache = cache,
    refresh = refresh,
    cache_dir = cache_dir
  )
  if (cached$hit) {
    return(cached$value)
  }

  value <- fetch()
  .sidra_cache_set(
    key = key,
    value = value,
    cache = cache,
    ttl = ttl,
    cache_dir = cache_dir
  )
  value
}

.validate_cache_options <- function(
  cache,
  refresh,
  ttl = 1800
) {
  if (!is.logical(cache) || length(cache) != 1L || is.na(cache)) {
    stop("'cache' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(ttl) || length(ttl) != 1L || is.na(ttl) ||
      !is.finite(ttl) || ttl <= 0) {
    stop("'ttl' must be a positive number of seconds", call. = FALSE)
  }
  invisible(TRUE)
}
