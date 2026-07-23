#' List the parameters of a SIDRA table
#'
#' Uses the official JSON table descriptor to return variables, periods,
#' classifications, categories, and geographic levels available in a table.
#'
#' @param x A numeric SIDRA table code.
#' @param wb Logical. When `TRUE`, open the official HTML descriptor in the
#'   default browser. When `FALSE`, return structured metadata.
#'
#' @return When `wb = FALSE`, a list with components `table`, `period`,
#'   `variable`, `classific_category`, and `geo`. When `wb = TRUE`, the
#'   descriptor URL is returned invisibly after the browser is opened.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso [get_sidra()]
#' @examples
#' \dontrun{
#' info_sidra(7060)
#' info_sidra(7060, wb = TRUE)
#' }
#' @keywords sidra IBGE
#' @export
info_sidra <- function(x, wb = FALSE) {
  table <- .validate_table(x)
  if (!is.logical(wb) || length(wb) != 1L || is.na(wb)) {
    stop("'wb' argument must be TRUE or FALSE", call. = FALSE)
  }

  if (wb) {
    url <- paste0(.sidra_descriptor_html_base, table)
    .open_sidra_descriptor(url)
    return(invisible(url))
  }

  .descriptor_to_legacy_info(.fetch_descriptor(table))
}

.descriptor_to_legacy_info <- function(descriptor) {
  if (!is.list(descriptor) || is.null(descriptor$Id) ||
        is.null(descriptor$Nome)) {
    .sidrar_abort(
      "SIDRA API returned an invalid table descriptor",
      "sidrar_parse_error"
    )
  }

  table <- list(
    table = paste0(
      "Tabela ", .scalar_text(descriptor$Id), ": ",
      .scalar_text(descriptor$Nome)
    )
  )

  period_description <- .scalar_text(descriptor$PeriodoDisponibilidade)
  if (!nzchar(period_description)) {
    periods <- descriptor$Periodos
    period_codes <- if (is.null(periods) || length(periods) == 0L) {
      character()
    } else {
      vapply(periods, function(x) .scalar_text(x$Codigo), character(1))
    }
    period_description <- paste(period_codes, collapse = ", ")
  }
  period <- list(period = period_description)

  variables <- descriptor$Variaveis
  variable_data <- if (is.null(variables) || length(variables) == 0L) {
    data.frame(
      cod = character(),
      desc = character(),
      stringsAsFactors = FALSE
    )
  } else {
    rows <- lapply(variables, function(variable) {
      description <- .scalar_text(variable$Nome)
      unit <- .scalar_text(variable$UnidadeMedida)
      exception <- .scalar_text(variable$PeriodoDisponibilidadeExcecao)

      if (nzchar(unit)) {
        description <- paste0(description, " (", unit, ")")
      }
      if (nzchar(exception)) {
        description <- paste0(description, " [", exception, "]")
      }

      data.frame(
        cod = .scalar_text(variable$Id),
        desc = description,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
  variable <- list(variable = variable_data)

  classifications <- descriptor$Classificacoes
  if (is.null(classifications) || length(classifications) == 0L) {
    classification_data <- NULL
  } else {
    classification_data <- lapply(classifications, function(classification) {
      categories <- classification$Categorias
      if (is.null(categories) || length(categories) == 0L) {
        data.frame(
          cod = character(),
          desc = character(),
          stringsAsFactors = FALSE
        )
      } else {
        rows <- lapply(categories, function(category) {
          data.frame(
            cod = .scalar_text(category$Id),
            desc = .scalar_text(category$Nome),
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, rows)
      }
    })

    names(classification_data) <- vapply(
      classifications,
      function(classification) {
        paste0(
          "c", .scalar_text(classification$Id),
          " = ", .scalar_text(classification$Nome),
          " (", length(classification$Categorias), ")"
        )
      },
      character(1)
    )
  }
  classific_category <- list(classific_category = classification_data)

  levels <- descriptor$NiveisTerritoriais
  if (is.null(levels) || length(levels) == 0L) {
    geo_data <- data.frame(
      cod = character(),
      desc = character(),
      stringsAsFactors = FALSE
    )
  } else {
    dictionary <- .geo_dictionary()
    rows <- lapply(levels, function(level) {
      code <- paste0("n", .scalar_text(level$Id))
      index <- match(code, dictionary$code)
      alias <- if (is.na(index)) code else dictionary$description[[index]]

      data.frame(
        code = code,
        cod = alias,
        desc = paste0(
          .scalar_text(level$Nome),
          " (", .scalar_text(level$QuantidadeUnidadesAtivas, "0"), ")"
        ),
        stringsAsFactors = FALSE
      )
    })
    geo_data <- do.call(rbind, rows)
    geo_data <- geo_data[order(geo_data$code), c("cod", "desc"), drop = FALSE]
    rownames(geo_data) <- NULL
  }
  geo <- list(geo = geo_data)

  c(table, period, variable, classific_category, geo)
}
