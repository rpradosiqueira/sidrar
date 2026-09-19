.sidra_checkpoint_abort <- function(message, ...) {
  .sidrar_abort(message, "sidrar_checkpoint_error", ...)
}

.sidra_checkpoint_validate <- function(directory, resume) {
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) {
    stop("'resume' must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(directory) &&
      (!is.character(directory) || length(directory) != 1L ||
       is.na(directory) || !nzchar(trimws(directory)))) {
    stop("'checkpoint' must be NULL or a non-empty directory path", call. = FALSE)
  }
}

.sidra_collection_identity <- function(queries, value_type, batch_size) {
  list(
    queries = lapply(queries, function(query) {
      url <- .normalize_api_url(query$url)
      .validate_sidra_url_semantics(url)
      if (!is.logical(query$header) || length(query$header) != 1L ||
          is.na(query$header)) {
        stop("Invalid query header setting", call. = FALSE)
      }
      representation <- if (!is.null(value_type)) value_type else
        if (is.null(query$parameters$value_type)) "numeric" else
          match.arg(query$parameters$value_type, c("numeric", "character", "both"))
      list(url = url, header = query$header, value_type = representation)
    }),
    batch_size = batch_size,
    package_version = as.character(utils::packageVersion("sidrar"))
  )
}

.sidra_checkpoint_open <- function(directory, identity, resume) {
  if (is.null(directory)) return(list(directory = NULL, manifest = NULL))
  directory <- path.expand(directory)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    .sidra_checkpoint_abort("Unable to create the checkpoint directory")
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  lock <- file.path(directory, ".sidrar-lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    .sidra_checkpoint_abort(paste(
      "Checkpoint is locked. Confirm no collector is running before",
      "manually removing its .sidrar-lock directory."
    ), checkpoint = directory)
  }
  success <- FALSE
  on.exit(if (!success) unlink(lock, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "sidrar-checkpoint.rds")
  manifest <- NULL
  if (file.exists(path)) {
    if (!resume) {
      .sidra_checkpoint_abort("Checkpoint already exists; use a new directory")
    }
    manifest <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.list(manifest) || !identical(manifest$schema_version, 1L) ||
        !identical(manifest$identity, identity) ||
        !is.list(manifest$queries) || length(manifest$queries) == 0L ||
        !all(vapply(manifest$queries, inherits, logical(1), "sidra_query")) ||
        !is.list(manifest$entries) ||
        length(manifest$entries) != length(manifest$queries)) {
      .sidra_checkpoint_abort(paste(
        "Checkpoint is invalid or does not match the queries, batch size,",
        "value type, or package version; use a new directory."
      ))
    }
    # Validate every frozen URL before any cached result is trusted.
    if (!identical(manifest$plan_identity,
                   .sidra_collection_identity(manifest$queries, NULL, NULL))) {
      .sidra_checkpoint_abort("Checkpoint frozen query inventory is invalid")
    }
    # Immutable completion records avoid replacing the manifest on Windows.
    for (index in seq_along(manifest$queries)) {
      completed <- file.path(directory, sprintf("batch-%06d.meta.rds", index))
      if (file.exists(completed)) {
        entry <- tryCatch(readRDS(completed), error = function(e) NULL)
        if (is.null(entry)) {
          .sidra_checkpoint_abort("Checkpoint completion record is invalid",
                                 path = completed)
        }
        manifest$entries[[index]] <- entry
      }
    }
  } else {
    contents <- setdiff(list.files(directory, all.files = TRUE),
                        c(".", "..", ".sidrar-lock"))
    if (length(contents)) {
      .sidra_checkpoint_abort("Checkpoint directory is not empty and has no manifest")
    }
  }
  success <- TRUE
  list(directory = directory, lock = lock, manifest = manifest)
}

.sidra_checkpoint_unlock <- function(state) {
  if (!is.null(state$lock)) unlink(state$lock, recursive = TRUE)
  invisible(NULL)
}

.sidra_checkpoint_save <- function(value, path, replace = FALSE) {
  temporary <- tempfile("sidrar-checkpoint-", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 2)
  saved <- if (replace) .sidra_replace_cache_file(temporary, path) else {
    !file.exists(path) && .sidra_file_rename(temporary, path)
  }
  if (!saved) {
    .sidra_checkpoint_abort("Unable to save the checkpoint safely", path = path)
  }
  invisible(NULL)
}

.sidra_checkpoint_initialize <- function(state, identity, queries) {
  if (is.null(state$directory)) return(state)
  state$manifest <- list(
    schema_version = 1L, identity = identity, queries = queries,
    plan_identity = .sidra_collection_identity(queries, NULL, NULL),
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    entries = vector("list", length(queries))
  )
  .sidra_checkpoint_save(state$manifest,
                         file.path(state$directory, "sidrar-checkpoint.rds"))
  state
}

.sidra_checkpoint_read <- function(state, index) {
  if (is.null(state$directory)) return(NULL)
  entry <- state$manifest$entries[[index]]
  if (is.null(entry)) return(NULL)
  # Paths are derived locally, never taken from the RDS manifest.
  path <- file.path(state$directory, sprintf("batch-%06d.rds", index))
  if (!is.list(entry) || !identical(entry$index, as.integer(index)) ||
      !is.character(entry$md5) || length(entry$md5) != 1L ||
      is.na(entry$md5) || !file.exists(path) ||
      !identical(unname(tools::md5sum(path)), entry$md5)) {
    .sidra_checkpoint_abort("Checkpoint batch is missing or its checksum is invalid",
                           batch_index = index, path = path)
  }
  batch <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(batch) || !is.data.frame(batch$data) ||
      !is.character(batch$url) || length(batch$url) != 1L || is.na(batch$url) ||
      !is.list(batch$warnings) ||
      !all(vapply(batch$warnings, inherits, logical(1), "warning")) ||
      !inherits(batch$accessed_at, "POSIXct") || length(batch$accessed_at) != 1L ||
      !is.finite(as.numeric(batch$accessed_at))) {
    .sidra_checkpoint_abort("Checkpoint batch has an invalid structure",
                           batch_index = index, path = path)
  }
  batch
}

.sidra_checkpoint_write <- function(state, index, batch) {
  if (is.null(state$directory)) return(state)
  path <- file.path(state$directory, sprintf("batch-%06d.rds", index))
  # An uncommitted orphan can be replaced; no completed batch is overwritten.
  .sidra_checkpoint_save(batch, path, replace = TRUE)
  state$manifest$entries[[index]] <- list(
    index = as.integer(index), md5 = unname(tools::md5sum(path))
  )
  .sidra_checkpoint_save(state$manifest$entries[[index]],
                         file.path(state$directory, sprintf("batch-%06d.meta.rds", index)))
  state
}
