#' Listing all the parameters of a SIDRA's table
#'
#' It returns the parameters and their descriptions of a SIDRA's table.
#'
#' @param x A table from SIDRA's API.
#' @param wb Logical. Should the metadata be open in the web browser?
#'    Default to FALSE.
#' @return A list with the all table's parameters.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{get_sidra}}
#' @examples
#' \dontrun{
#' info_sidra(1419)
#' }
#'
#' @keywords sidra IBGE
#' @export


info_sidra <- function(x, wb = FALSE) {

  if (!is.logical(wb)) {

    stop("'wb' argument must be TRUE or FALSE")

  } else if (wb == FALSE || wb == F) {

    a <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))

    # Tabela
    tab1 = a %>%
      rvest::html_nodes("#lblNumeroTabela") %>%
      rvest::html_text()

    tab2 = a %>%
      rvest::html_nodes("#lblNomeTabela") %>%
      rvest::html_text()

    table <- list("table" = paste0("Tabela ", tab1, ": ", tab2))


    # Período
    p1 = a %>%
      rvest::html_nodes("#lblPeriodoDisponibilidade") %>%
      rvest::html_text()

    period <- list("period" = p1)


    # Variáveis
    v1 <- a %>% rvest::html_nodes("#lblVariaveis") %>%
      rvest::html_text()

    v2 <- a %>% rvest::html_table(fill = TRUE, trim = TRUE)
    v2 <- v2[[2]]

    v3 <- data.frame(cod = apply(v2, 1, stringr::str_extract,"[[:digit:]]+"),
                     desc = apply(v2, 1, stringr::str_replace_all, "([[:digit:]])", ""))
    v3$cod <- stringr::str_trim(v3$cod)
    v3$desc <- stringr::str_trim(v3$desc)
    v3$desc <- stringr::str_replace(v3$desc, " - casas decimais:  padr\uE3o = , m\uE1ximo =", "")

    variables <- list("variable" = v3)

    # Classificações e categorias
    c1 <- rvest::html_nodes(a, "table") %>%
      rvest::html_table(fill = TRUE, trim = TRUE) %>%
      unlist() %>%
      stringr::str_extract("\\C[0-9]+") %>%
      stringr::str_subset("\\C[0-9]+") %>%
      base::tolower()

    if (length(c1) >= 1) {

      lc1 <- length(c1)

      c2 <- a %>% rvest::html_nodes(".tituloLinha:nth-child(4)") %>% rvest::html_text()

      c3 <- a %>% rvest::html_nodes(".tituloLinha:nth-child(5)") %>% rvest::html_text()

      c4 <- paste(c1, "=", c2, c3)

      c5 <- list()

      for (i in 0:(lc1-1)) {

        c5[[i+1]] <- a %>% rvest::html_nodes(paste0("#lstClassificacoes_lblQuantidadeCategorias_", i, "+ ", "#tabPrincipal span")) %>%
          rvest::html_text() %>%  stringr::str_replace("\\[[^]]*]", "NA")
        c5[[i+1]] <- c5[[i+1]][c5[[i+1]] != "NA"]
        c5[[i+1]] <-  data.frame(cod =  c5[[i+1]][seq(1, length(c5[[i+1]]), 2)],
                                 desc = c5[[i+1]][seq(2, length(c5[[i+1]]), 2)])

      }

      names(c5) <- c4

      classific_category <- list("classific_category" = c5)

    } else {

      classific_category <- list("classific_category" = NULL)

    }



    # Níveis Territoriais
    trad.geo <- data.frame(cod = as.character(c("n1","n2","n3","n8","n9","n7","n13","n14","n15","n23","n6","n10",
                                                "n11","n102")),
                           cod2 = as.character(c("Brazil","Region","State","MesoRegion","MicroRegion",
                                                 "MetroRegion","MetroRegionDiv","IRD","UrbAglo","PopArrang",
                                                 "City", "District","subdistrict","Neighborhood")),
                           level = c(1:14),
                           order = c(1:5, 10:14, 6:9))


    n1 <- rvest::html_nodes(a, "table") %>%
      rvest::html_table(fill = TRUE, trim = TRUE) %>%
      unlist() %>%
      stringr::str_extract("N[0-9]+") %>%
      stringr::str_subset("N[0-9]+") %>%
      tolower() %>%
      as.data.frame()

    n2 <- a %>% rvest::html_nodes("p+ #tabPrincipal span:nth-child(4)") %>% rvest::html_text()
    n3 <- a %>% rvest::html_nodes("p+ #tabPrincipal span:nth-child(5)") %>% rvest::html_text()
    n4 <- data.frame(desc = paste(n2, n3))

    n5 <- cbind(n1, n4)

    ngeo <-  merge(trad.geo, n5, by.x = "cod", by.y = ".")
    ngeo <- ngeo[c("cod2","desc")]
    names(ngeo) <- c("cod","desc")

    ngeo <- list(geo = ngeo)

    info <- c(table, period, variables, classific_category, ngeo)

    return(info)


  } else if (wb == TRUE || wb == T) {

    p <- readline(prompt = "Can the web browser be open? (y/n): ")

    if (p == "y" | p == "Y") {

      shell.exec(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))

    } else {

      stop(paste("Sorry, I need your permission to show the parameters of the table", x))

    }

  }

}
