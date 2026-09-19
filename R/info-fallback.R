.info_valid_id <- function(x) {
  if ((!is.character(x) && !is.numeric(x)) || is.object(x) ||
        length(x) != 1L || is.na(x)) return(FALSE)
  if (is.character(x)) return(grepl("^[0-9]+$", x))
  is.numeric(x) && !is.complex(x) && is.finite(x) &&
    x >= 0 && x <= 2^53 - 1 && x == floor(x)
}

.info_id_text <- function(x) {
  if (is.character(x)) return(x)
  format(x, scientific = FALSE, trim = TRUE, digits = 22L)
}

.info_valid_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

.info_metadata_error <- function(detail, url) {
  .sidrar_abort(
    paste0("IBGE's alternative API returned invalid ", detail),
    "sidrar_parse_error", url = url
  )
}

.info_metadata_object <- function(x, required, detail, url) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
        !all(required %in% names(x))) {
    .info_metadata_error(detail, url)
  }
  invisible(x)
}

.info_metadata_records <- function(x, detail, url, names_required = TRUE) {
  if (!is.list(x) || (!is.null(names(x)) && length(x) > 0L)) {
    .info_metadata_error(detail, url)
  }
  required <- if (names_required) c("id", "nome") else "id"
  ids <- vapply(x, function(record) {
    .info_metadata_object(record, required, detail, url)
    id <- record[["id", exact = TRUE]]
    if (!.info_valid_id(id) ||
          (names_required && !.info_valid_text(record[["nome", exact = TRUE]]))) {
      .info_metadata_error(detail, url)
    }
    .info_id_text(id)
  }, character(1))
  if (anyDuplicated(.discovery_canonical_digits(ids))) {
    .info_metadata_error(paste0(detail, " (duplicate identifiers)"), url)
  }
  ids
}

.info_metadata_geographies <- function(x, url) {
  if (!is.list(x) || (length(x) > 0L &&
        (is.null(names(x)) || anyDuplicated(names(x))))) {
    .info_metadata_error("territorial levels", url)
  }
  levels <- unlist(lapply(x, function(group) {
    if (length(group) == 0L) return(character())
    if (is.list(group)) {
      if (!is.null(names(group)) || any(!vapply(group, function(level) {
        is.character(level) && length(level) == 1L && !is.na(level)
      }, logical(1)))) {
        .info_metadata_error("territorial level records", url)
      }
      group <- unlist(group, use.names = FALSE)
    }
    if (!is.character(group) || anyNA(group) ||
          any(!grepl("^N?[0-9]+$", toupper(group)))) {
      .info_metadata_error("territorial level identifiers", url)
    }
    paste0("n", .discovery_canonical_digits(sub("^N", "", toupper(group))))
  }), use.names = FALSE)
  levels <- sort(unique(as.character(levels)))
  dictionary <- .geo_dictionary()
  aliases <- dictionary$description[match(levels, dictionary$code)]
  aliases[is.na(aliases)] <- levels[is.na(aliases)]
  data.frame(
    cod = aliases,
    desc = paste0(
      "N\u00edvel territorial ", toupper(levels),
      " (quantidade de unidades indispon\u00edvel)"
    )[seq_along(levels)],
    stringsAsFactors = FALSE
  )
}

.info_metadata_to_legacy <- function(metadata, periods, table, urls) {
  url <- unname(urls[["metadata"]])
  .info_metadata_object(
    metadata,
    c("id", "nome", "variaveis", "classificacoes", "nivelTerritorial"),
    "table metadata", url
  )
  if (!.info_valid_id(metadata$id) || !.info_valid_text(metadata$nome) ||
        !identical(
          .discovery_canonical_digits(.info_id_text(metadata$id)), table
        )) {
    .info_metadata_error("table identity", url)
  }
  variables <- metadata$variaveis
  variable_ids <- .info_metadata_records(variables, "variable records", url)
  missing_units <- character()
  variable_names <- vapply(seq_along(variables), function(index) {
    variable <- variables[[index]]
    unit <- variable[["unidade", exact = TRUE]]
    if (is.null(unit) || identical(unit, "")) {
      missing_units <<- c(missing_units, variable_ids[[index]])
      return(variable$nome)
    }
    if (!.info_valid_text(unit)) {
      .info_metadata_error("variable units", url)
    }
    paste0(variable$nome, " (", unit, ")")
  }, character(1))

  classifications <- metadata$classificacoes
  class_ids <- .info_metadata_records(
    classifications, "classification records", url
  )
  categories <- lapply(classifications, function(classification) {
    if (!"categorias" %in% names(classification)) {
      .info_metadata_error("classification categories", url)
    }
    records <- classification$categorias
    ids <- .info_metadata_records(records, "category records", url)
    data.frame(
      cod = ids,
      desc = vapply(records, function(record) record$nome, character(1)),
      stringsAsFactors = FALSE
    )
  })
  if (length(categories) == 0L) {
    categories <- NULL
  } else {
    names(categories) <- vapply(seq_along(categories), function(index) {
      paste0(
        "c", class_ids[[index]], " = ", classifications[[index]]$nome,
        " (", nrow(categories[[index]]), ")"
      )
    }, character(1))
  }

  period_ids <- .info_metadata_records(
    periods, "period records", unname(urls[["periods"]]),
    names_required = FALSE
  )
  result <- list(
    table = paste0("Tabela ", table, ": ", metadata$nome),
    period = paste(period_ids, collapse = ", "),
    variable = data.frame(
      cod = variable_ids, desc = variable_names, stringsAsFactors = FALSE
    ),
    classific_category = categories,
    geo = .info_metadata_geographies(metadata$nivelTerritorial, url)
  )
  attr(result, "sidrar_metadata") <- list(
    source = "aggregate-v3",
    urls = urls,
    unavailable = c(
      "descriptor_geographic_names", "active_geographic_unit_counts",
      "variable_period_availability_exceptions",
      if (length(missing_units) > 0L) "variable_units"
    ),
    variables_without_units = missing_units
  )
  result
}

.info_sidra_fallback <- function(table, primary) {
  if (!isTRUE(getOption("sidrar.fallback", TRUE))) stop(primary)
  table <- .discovery_canonical_digits(as.character(table))
  urls <- paste0(.sidra_catalog_url, "/", table, c("/metadados", "/periodos"))
  names(urls) <- c("metadata", "periods")
  message(
    "SIDRA's table descriptor returned a Cloudflare challenge; using IBGE's ",
    "official aggregate metadata. Active geographic unit counts and ",
    "variable-specific availability exceptions are unavailable."
  )
  tryCatch({
    metadata <- .sidra_parse_json(
      .sidra_request(urls[["metadata"]]), simplify = FALSE,
      context = "alternative table metadata"
    )
    periods <- .sidra_parse_json(
      .sidra_request(urls[["periods"]]), simplify = FALSE,
      context = "alternative table periods"
    )
    .info_metadata_to_legacy(metadata, periods, table, urls)
  }, error = function(e) {
    e$primary_error <- primary
    e$message <- paste0(
      "IBGE's alternative metadata failed after a SIDRA Cloudflare challenge: ",
      conditionMessage(e)
    )
    stop(e)
  })
}
