#' Discover SIDRA tables
#'
#' Returns the official IBGE aggregate catalog as a rectangular table while
#' retaining the research group to which each SIDRA table belongs.
#'
#' @param refresh A single logical value. When caching is enabled, bypass and
#'   replace an existing entry.
#' @param cache A single logical value. Cache normalized catalog or metadata
#'   responses on disk. The default is `FALSE`, preserving live behavior.
#' @param ttl A positive cache lifetime in seconds. The default is 30 minutes.
#' @param cache_dir Optional cache directory. By default, the platform-specific
#'   user cache directory for `sidrar` is used.
#'
#' @return A base [data.frame()] with columns `research_id`, `research_name`,
#'   `table_id`, and `table_name`. Identifier columns are character vectors.
#'   Research groups without tables are retained with missing table fields.
#' @details The catalog is obtained from the official version 3 aggregate API.
#'   Unknown fields added by the API are ignored. Missing fields are returned
#'   as `NA` without changing the documented column types.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [search_sidra()], [sidra_metadata()]
#' @examples
#' \dontrun{
#' catalog <- sidra_catalog()
#' subset(catalog, grepl("prices", table_name, ignore.case = TRUE))
#' }
#' @family SIDRA discovery functions
#' @keywords sidra IBGE
#' @export
sidra_catalog <- function(
  refresh = FALSE,
  cache = FALSE,
  ttl = 1800,
  cache_dir = NULL
) {
  .discovery_validate_refresh(refresh)
  .sidra_cached_value(
    key = "catalog-v3",
    cache = cache,
    refresh = refresh,
    ttl = ttl,
    cache_dir = cache_dir,
    fetch = function() {
      text <- .sidra_request(.sidra_catalog_url)
      catalog <- .sidra_parse_json(
        text,
        simplify = FALSE,
        context = "aggregate catalog"
      )
      groups <- .discovery_records(catalog)
      rows <- list()

      for (group in groups) {
        research_id <- .discovery_text(group$id)
        research_name <- .discovery_text(group$nome)
        aggregates <- .discovery_records(group$agregados)

        if (length(aggregates) == 0L) {
          rows[[length(rows) + 1L]] <- data.frame(
            research_id = research_id,
            research_name = research_name,
            table_id = NA_character_,
            table_name = NA_character_,
            stringsAsFactors = FALSE
          )
          next
        }

        for (aggregate in aggregates) {
          table_id <- .discovery_text(aggregate$id)
          if (!is.na(table_id) && grepl("^[0-9]+$", table_id)) {
            table_id <- .discovery_canonical_digits(table_id)
          }
          rows[[length(rows) + 1L]] <- data.frame(
            research_id = research_id,
            research_name = research_name,
            table_id = table_id,
            table_name = .discovery_text(aggregate$nome),
            stringsAsFactors = FALSE
          )
        }
      }

      result <- .discovery_bind(rows, .discovery_empty_catalog())
      .discovery_sort_catalog(result)
    }
  )
}

#' Retrieve structured SIDRA table metadata
#'
#' Normalizes the official version 3 metadata and period responses into a
#' stable collection of base data frames.
#'
#' @param table A single SIDRA table code.
#' @inheritParams sidra_catalog
#'
#' @return A named list with six data frames. `table` contains `table_id`,
#'   `table_name`, `research_name`, `subject`, `url`, `frequency`,
#'   `period_start`, and `period_end`. `periods` has the schema documented by
#'   [sidra_periods()]. `variables` contains `table_id`, `variable_id`,
#'   `variable_name`, `unit`, and the character list-column `summarized_by`.
#'   `classifications` contains `table_id`, `classification_id`,
#'   `classification_name`, `summarizable`, and the character list-column
#'   `summarization_exceptions`. `categories` contains `table_id`,
#'   `classification_id`, `category_id`, `category_name`, `unit`, `level`, and
#'   `category_order`. `geographies` contains `table_id`, `group`, and
#'   `level_id`. All API identifiers are character vectors; no raw JSON object
#'   is exposed.
#' @details `sidra_metadata()` requests both the table metadata endpoint and
#'   the table-specific period endpoint. This makes `periods` equivalent to
#'   [sidra_periods()] and keeps the table's complete period inventory separate
#'   from its frequency and first/last-period summary. Within each
#'   classification, `category_order` is the one-based order in the official
#'   response. Together with `level`, it preserves the API's hierarchical
#'   preorder; categories are deliberately not sorted by identifier.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [sidra_catalog()], [sidra_periods()], [sidra_locations()]
#' @examples
#' \dontrun{
#' metadata <- sidra_metadata(7060)
#' metadata$variables
#' metadata$geographies
#' }
#' @family SIDRA discovery functions
#' @keywords sidra IBGE
#' @export
sidra_metadata <- function(
  table,
  refresh = FALSE,
  cache = FALSE,
  ttl = 1800,
  cache_dir = NULL
) {
  table <- .discovery_validate_table(table)
  .discovery_validate_refresh(refresh)
  core <- .sidra_cached_value(
    key = .sidra_cache_key("metadata-v3", table),
    cache = cache,
    refresh = refresh,
    ttl = ttl,
    cache_dir = cache_dir,
    fetch = function() {
      url <- paste0(.sidra_catalog_url, "/", table, "/metadados")
      text <- .sidra_request(url)
      metadata <- .sidra_parse_json(
        text,
        simplify = FALSE,
        context = "table metadata"
      )
      if (!is.list(metadata)) {
        metadata <- list()
      }

      table_data <- .discovery_metadata_table(metadata, table)
      variable_data <- .discovery_metadata_variables(metadata, table)
      classification_data <- .discovery_metadata_classifications(
        metadata,
        table
      )
      geography_data <- .discovery_metadata_geographies(metadata, table)

      list(
        table = table_data,
        variables = variable_data$variables,
        classifications = classification_data$classifications,
        categories = classification_data$categories,
        geographies = geography_data
      )
    }
  )

  list(
    table = core$table,
    periods = sidra_periods(
      table,
      refresh = refresh,
      cache = cache,
      ttl = ttl,
      cache_dir = cache_dir
    ),
    variables = core$variables,
    classifications = core$classifications,
    categories = core$categories,
    geographies = core$geographies
  )
}

#' List the periods available for a SIDRA table
#'
#' @param table A single SIDRA table code.
#' @inheritParams sidra_catalog
#'
#' @return A base [data.frame()] with columns `table_id`, `period_id`,
#'   `period_name`, `alternative_names`, and `modified`. The
#'   `alternative_names` column is a base list-column of character vectors.
#' @details Data come from the official table-specific version 3 period
#'   endpoint. Period identifiers are kept as character strings so leading
#'   zeros and non-calendar codes cannot be lost.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [sidra_metadata()]
#' @examples
#' \dontrun{
#' sidra_periods(7060)
#' }
#' @family SIDRA discovery functions
#' @keywords sidra IBGE
#' @export
sidra_periods <- function(
  table,
  refresh = FALSE,
  cache = FALSE,
  ttl = 1800,
  cache_dir = NULL
) {
  table <- .discovery_validate_table(table)
  .discovery_validate_refresh(refresh)
  .sidra_cached_value(
    key = .sidra_cache_key("periods-v3", table),
    cache = cache,
    refresh = refresh,
    ttl = ttl,
    cache_dir = cache_dir,
    fetch = function() .discovery_fetch_periods(table)
  )
}

#' List the locations available for a SIDRA table
#'
#' @param table A single SIDRA table code.
#' @param level A single geographic level, with or without the `"N"` prefix;
#'   for example, `"N6"` or `6` for municipalities.
#' @inheritParams sidra_catalog
#'
#' @return A base [data.frame()] with columns `table_id`, `level_id`,
#'   `level_name`, `location_id`, and `location_name`. All identifiers are
#'   character vectors.
#' @details Locations come from the official table- and level-specific version
#'   3 endpoint. Location identifiers are SIDRA territorial identifiers; they
#'   must not be coerced to numeric values or assumed to be census tract codes.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [sidra_metadata()]
#' @examples
#' \dontrun{
#' sidra_locations(7060, "N6")
#' }
#' @family SIDRA discovery functions
#' @keywords sidra IBGE
#' @export
sidra_locations <- function(
  table,
  level,
  refresh = FALSE,
  cache = FALSE,
  ttl = 1800,
  cache_dir = NULL
) {
  table <- .discovery_validate_table(table)
  level <- .discovery_validate_level(level)
  .discovery_validate_refresh(refresh)
  .sidra_cached_value(
    key = .sidra_cache_key("locations-v3", table, level),
    cache = cache,
    refresh = refresh,
    ttl = ttl,
    cache_dir = cache_dir,
    fetch = function() {
      url <- paste0(
        .sidra_catalog_url, "/", table, "/localidades/", level
      )
      text <- .sidra_request(url)
      payload <- .sidra_parse_json(
        text,
        simplify = FALSE,
        context = "table locations"
      )
      locations <- .discovery_records(payload)
      rows <- lapply(locations, function(location) {
        location_level <- location$nivel
        if (!is.list(location_level)) {
          location_level <- list()
        }
        data.frame(
          table_id = table,
          level_id = level,
          level_name = .discovery_text(location_level$nome),
          location_id = .discovery_text(location$id),
          location_name = .discovery_text(location$nome),
          stringsAsFactors = FALSE
        )
      })

      result <- .discovery_bind(rows, .discovery_empty_locations())
      .discovery_sort(
        result,
        c("level_id", "location_id", "location_name")
      )
    }
  )
}

.discovery_validate_refresh <- function(refresh) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE", call. = FALSE)
  }
  invisible(refresh)
}

.discovery_validate_table <- function(table) {
  if (length(table) != 1L || is.na(table) || !is.atomic(table)) {
    stop("'table' must identify exactly one SIDRA table", call. = FALSE)
  }

  table <- trimws(as.character(table))
  if (!nzchar(table) || !grepl("^[0-9]+$", table)) {
    stop("'table' must be a numeric SIDRA table code", call. = FALSE)
  }
  .discovery_canonical_digits(table)
}

.discovery_validate_level <- function(level) {
  if (length(level) != 1L || is.na(level) || !is.atomic(level)) {
    stop("'level' must identify exactly one SIDRA geographic level", call. = FALSE)
  }

  level <- toupper(trimws(as.character(level)))
  if (!nzchar(level) || !grepl("^N?[0-9]+$", level)) {
    stop(
      "'level' must be a code such as 'N6' or 6",
      call. = FALSE
    )
  }
  number <- sub("^N", "", level)
  paste0("N", .discovery_canonical_digits(number))
}

.discovery_canonical_digits <- function(value) {
  sub("^0+(?=[0-9])", "", value, perl = TRUE)
}

.discovery_text <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L || is.list(x)) {
    return(default)
  }

  value <- x[[1L]]
  if (length(value) == 0L || is.na(value)) {
    return(default)
  }
  enc2utf8(as.character(value))
}

.discovery_integer <- function(x) {
  value <- .discovery_text(x)
  if (is.na(value)) {
    return(NA_integer_)
  }
  value <- suppressWarnings(as.integer(value))
  if (is.na(value)) NA_integer_ else value
}

.discovery_logical <- function(x) {
  if (is.null(x) || length(x) == 0L || is.list(x) || is.na(x[[1L]])) {
    return(NA)
  }
  as.logical(x[[1L]])
}

.discovery_records <- function(x) {
  if (is.null(x) || !is.list(x) || length(x) == 0L) {
    return(list())
  }
  if (!is.null(names(x))) {
    return(list(x))
  }
  Filter(is.list, x)
}

.discovery_bind <- function(rows, empty) {
  if (length(rows) == 0L) {
    return(empty)
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.discovery_sort <- function(data, columns) {
  if (nrow(data) < 2L) {
    rownames(data) <- NULL
    return(data)
  }

  keys <- lapply(columns, function(column) data[[column]])
  index <- do.call(
    order,
    c(keys, list(na.last = TRUE, method = "radix"))
  )
  data <- data[index, , drop = FALSE]
  rownames(data) <- NULL
  data
}

.discovery_sort_catalog <- function(data) {
  if (nrow(data) < 2L) {
    rownames(data) <- NULL
    return(data)
  }

  table_number <- suppressWarnings(as.numeric(data$table_id))
  index <- order(
    data$research_id,
    table_number,
    data$table_id,
    data$research_name,
    data$table_name,
    na.last = TRUE,
    method = "radix"
  )
  data <- data[index, , drop = FALSE]
  rownames(data) <- NULL
  data
}

.discovery_empty_catalog <- function() {
  data.frame(
    research_id = character(),
    research_name = character(),
    table_id = character(),
    table_name = character(),
    stringsAsFactors = FALSE
  )
}

.discovery_empty_periods <- function() {
  result <- data.frame(
    table_id = character(),
    period_id = character(),
    period_name = character(),
    modified = character(),
    stringsAsFactors = FALSE
  )
  result$alternative_names <- I(vector("list", 0L))
  result[c(
    "table_id", "period_id", "period_name", "alternative_names", "modified"
  )]
}

.discovery_empty_variables <- function() {
  result <- data.frame(
    table_id = character(),
    variable_id = character(),
    variable_name = character(),
    unit = character(),
    stringsAsFactors = FALSE
  )
  result$summarized_by <- I(vector("list", 0L))
  result
}

.discovery_empty_classifications <- function() {
  result <- data.frame(
    table_id = character(),
    classification_id = character(),
    classification_name = character(),
    summarizable = logical(),
    stringsAsFactors = FALSE
  )
  result$summarization_exceptions <- I(vector("list", 0L))
  result
}

.discovery_empty_categories <- function() {
  data.frame(
    table_id = character(),
    classification_id = character(),
    category_id = character(),
    category_name = character(),
    unit = character(),
    level = integer(),
    category_order = integer(),
    stringsAsFactors = FALSE
  )
}

.discovery_empty_geographies <- function() {
  data.frame(
    table_id = character(),
    group = character(),
    level_id = character(),
    stringsAsFactors = FALSE
  )
}

.discovery_empty_locations <- function() {
  data.frame(
    table_id = character(),
    level_id = character(),
    level_name = character(),
    location_id = character(),
    location_name = character(),
    stringsAsFactors = FALSE
  )
}

.discovery_metadata_table <- function(metadata, table) {
  periodicity <- metadata$periodicidade
  if (!is.list(periodicity)) {
    periodicity <- list()
  }
  metadata_id <- .discovery_text(metadata$id, default = table)
  if (!is.na(metadata_id) && grepl("^[0-9]+$", metadata_id)) {
    metadata_id <- .discovery_canonical_digits(metadata_id)
  }
  metadata_url <- metadata$URL
  if (is.null(metadata_url)) {
    metadata_url <- metadata$url
  }

  data.frame(
    table_id = metadata_id,
    table_name = .discovery_text(metadata$nome),
    research_name = .discovery_text(metadata$pesquisa),
    subject = .discovery_text(metadata$assunto),
    url = .discovery_text(metadata_url),
    frequency = .discovery_text(periodicity$frequencia),
    period_start = .discovery_text(periodicity$inicio),
    period_end = .discovery_text(periodicity$fim),
    stringsAsFactors = FALSE
  )
}

.discovery_metadata_variables <- function(metadata, table) {
  variables <- .discovery_records(metadata$variaveis)
  rows <- lapply(variables, function(variable) {
    summarized_by <- unlist(
      variable$sumarizacao,
      recursive = TRUE,
      use.names = FALSE
    )
    summarized_by <- enc2utf8(as.character(summarized_by))
    summarized_by <- unique(
      summarized_by[
        !is.na(summarized_by) & nzchar(trimws(summarized_by))
      ]
    )
    row <- data.frame(
      table_id = table,
      variable_id = .discovery_text(variable$id),
      variable_name = .discovery_text(variable$nome),
      unit = .discovery_text(variable$unidade),
      stringsAsFactors = FALSE
    )
    row$summarized_by <- I(list(summarized_by))
    row
  })
  result <- .discovery_bind(rows, .discovery_empty_variables())
  list(variables = .discovery_sort(result, c("variable_id", "variable_name")))
}

.discovery_metadata_classifications <- function(metadata, table) {
  classifications <- .discovery_records(metadata$classificacoes)
  classification_rows <- list()
  category_rows <- list()

  for (classification in classifications) {
    classification_id <- .discovery_text(classification$id)
    summarization <- classification$sumarizacao
    if (!is.list(summarization)) {
      summarization <- list(status = summarization)
    }
    exceptions <- unlist(
      summarization$excecao,
      recursive = TRUE,
      use.names = FALSE
    )
    exceptions <- as.character(exceptions)
    exceptions <- unique(
      exceptions[!is.na(exceptions) & nzchar(trimws(exceptions))]
    )
    classification_row <- data.frame(
      table_id = table,
      classification_id = classification_id,
      classification_name = .discovery_text(classification$nome),
      summarizable = .discovery_logical(summarization$status),
      stringsAsFactors = FALSE
    )
    classification_row$summarization_exceptions <- I(list(exceptions))
    classification_rows[[length(classification_rows) + 1L]] <-
      classification_row

    categories <- .discovery_records(classification$categorias)
    for (category_index in seq_along(categories)) {
      category <- categories[[category_index]]
      category_rows[[length(category_rows) + 1L]] <- data.frame(
        table_id = table,
        classification_id = classification_id,
        category_id = .discovery_text(category$id),
        category_name = .discovery_text(category$nome),
        unit = .discovery_text(category$unidade),
        level = .discovery_integer(category$nivel),
        category_order = as.integer(category_index),
        stringsAsFactors = FALSE
      )
    }
  }

  classification_data <- .discovery_bind(
    classification_rows,
    .discovery_empty_classifications()
  )
  category_data <- .discovery_bind(
    category_rows,
    .discovery_empty_categories()
  )

  list(
    classifications = .discovery_sort(
      classification_data,
      c("classification_id", "classification_name")
    ),
    categories = category_data
  )
}

.discovery_metadata_geographies <- function(metadata, table) {
  geographies <- metadata$nivelTerritorial
  if (is.null(geographies) || !is.list(geographies) ||
        length(geographies) == 0L) {
    return(.discovery_empty_geographies())
  }

  group_names <- names(geographies)
  if (is.null(group_names)) {
    group_names <- rep(NA_character_, length(geographies))
  }
  rows <- list()
  for (index in seq_along(geographies)) {
    levels <- unlist(geographies[[index]], recursive = TRUE, use.names = FALSE)
    if (length(levels) == 0L) {
      next
    }
    levels <- as.character(levels)
    levels <- levels[!is.na(levels) & nzchar(trimws(levels))]
    if (length(levels) == 0L) {
      next
    }
    levels <- toupper(trimws(levels))
    canonical_levels <- grepl("^N?[0-9]+$", levels)
    levels[canonical_levels] <- vapply(
      levels[canonical_levels],
      .discovery_validate_level,
      character(1)
    )

    rows[[length(rows) + 1L]] <- data.frame(
      table_id = rep(table, length(levels)),
      group = rep(group_names[[index]], length(levels)),
      level_id = levels,
      stringsAsFactors = FALSE
    )
  }

  result <- .discovery_bind(rows, .discovery_empty_geographies())
  result <- unique(result)
  .discovery_sort(result, c("group", "level_id"))
}

.discovery_fetch_periods <- function(table) {
  url <- paste0(.sidra_catalog_url, "/", table, "/periodos")
  text <- .sidra_request(url)
  payload <- .sidra_parse_json(
    text,
    simplify = FALSE,
    context = "table periods"
  )
  periods <- .discovery_records(payload)
  rows <- lapply(periods, function(period) {
    literals <- period$literals
    if (is.null(literals)) {
      literals <- period$nome
    }
    literals <- unlist(literals, recursive = TRUE, use.names = FALSE)
    literals <- enc2utf8(as.character(literals))
    literals <- unique(literals[!is.na(literals) & nzchar(trimws(literals))])
    period_name <- if (length(literals) == 0L) NA_character_ else literals[[1L]]
    alternative_names <- if (length(literals) < 2L) {
      character()
    } else {
      literals[-1L]
    }

    row <- data.frame(
      table_id = table,
      period_id = .discovery_text(period$id),
      period_name = period_name,
      modified = .discovery_text(period$modificacao),
      stringsAsFactors = FALSE
    )
    row$alternative_names <- I(list(alternative_names))
    row[c(
      "table_id", "period_id", "period_name", "alternative_names", "modified"
    )]
  })

  result <- .discovery_bind(rows, .discovery_empty_periods())
  .discovery_sort(result, c("period_id", "period_name"))
}
