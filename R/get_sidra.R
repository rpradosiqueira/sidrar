#' Get SIDRA's table
#'
#' This function allows the user to connect with IBGE's (Instituto Brasileiro de
#' Geografia e Estatistica) SIDRA API in a flexible way. \acronym{SIDRA} is the
#' acronym to "Sistema IBGE de Recuperação Automática" and it is the system where
#' IBGE makes aggregate data from their researches available.
#'
#' @usage get_sidra(x, variable = "allxp", period = "last", geo = "Brazil",
#'   geo.filter = NULL, classific = "all", category = "all", header = TRUE,
#'   format = 4, digits = "default", api = NULL)
#' @param x A table from IBGE's SIDRA API.
#' @param variable An integer vector of the variables' codes to be returned.
#'   Defaults to all variables with exception of "Total".
#' @param period A character vector describing the period of data. Defaults to
#'   the last available.
#' @param geo A character vector describing the geographic levels of the data.
#'   Defauts to "Brazil".
#' @param geo.filter A (named) list object with the specific item of the
#'   geographic level or all itens of a determined higher geografic level. It should
#'   be used when geo argument is provided, otherwise all geographic units of
#'   'geo' argument are considered.
#' @param classific A character vector with the table's classification(s). Defaults to
#'   all.
#' @param category "all" or a list object with the categories of the classifications
#'   of \code{classific(s)} argument. Defaults to "all".
#' @param header Logical. should the data frame be returned with the description
#'   names in header?
#' @param format An integer ranging between 1 and 4. Default to 4. See more in details.
#' @param digits An integer, "default" or "max". Default to "default" that returns the
#'   defaults digits to each variable.
#' @param api A character with the api's parameters. Defaults to NULL.
#' @details
#'   \code{period} can be a integer vector with names "first" and/or "last",
#'   or "all" or a simply character vector with date format %Y%m-%Y%m.
#'
#'   The \code{geo} argument can be one of "Brazil", "Region", "State",
#'   "MesoRegion", "MicroRegion", "MetroRegion", "MetroRegionDiv", "IRD",
#'   "UrbAglo", "City", "District","subdistrict","Neighborhood","PopArrang".
#'   'geo.filter' lists can/must be named with the same characters.
#'
#'   When NULL, the arguments \code{classific} and \code{category} return all options
#'   available.
#'   
#'   When argument \code{api} is not NULL, all others arguments informed are desconsidered
#'
#'   The \code{format} argument can be set to:
#'   \itemize{
#'      \item 1: Return only the descriptors' codes
#'      \item 2: Return only the descriptor's names
#'      \item 3: Return the codes and names of the geographic level and descriptors' names
#'      \item 4: Return the codes and names of the descriptors (Default)
#'      }
#' @return The function returns a data frame printed by default functions
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{info_sidra}}
#' @examples
#' \dontrun{
#' ## Requesting table 1419 (Consumer Price Index - IPCA) from the API
#' ipca <- get_sidra(1419,
#'                   variable = 69,
#'                   period = c("201212","201401-201412"),
#'                   geo = "City",
#'                   geo.filter = list("State" = 50))
#'
#' ## Urban population count from Census data (2010) for States and cities of Southest region.
#' get_sidra(1378,
#'           variable = 93,
#'           geo = c("State","City"),
#'           geo.filter = list("Region" = 3, "Region" = 3),
#'           classific = c("c1"),
#'           category = list(1))
#'  
#' ## Number of informants by state in the Inventory Research (last data available) 
#' get_sidra(api = "/t/254/n1/all/n3/all/v/151/p/last%201/c162/118423/c163/0")
#' 
#' }
#'
#' @keywords sidra IBGE
#' @export

get_sidra <- function(x,
                      variable = "allxp",
                      period = "last",
                      geo = "Brazil",
                      geo.filter = NULL,
                      classific = "all",
                      category = "all",
                      header = TRUE,
                      format = 4,
                      digits = "default",
                      api = NULL) {
  
  if (is.null(api)) {
    
    if (length(x) != 1) {
      stop("Only one table is allowed")
    }
    
    
    # Variaveis
    variable = paste(variable, collapse = ",")
    
    
    # Niveis territoriais
    trad.geo <- data.frame(cod = as.character(c("n1","n2","n3","n8","n9","n7","n13","n14","n15","n23","n6","n10",
                                                "n11","n102")),
                           description = as.character(c("Brazil","Region","State","MesoRegion","MicroRegion",
                                                        "MetroRegion","MetroRegionDiv","IRD","UrbAglo","PopArrang",
                                                        "City", "District","subdistrict","Neighborhood")),
                           level = c(1:14))
    
    if (sum(!(geo %in% trad.geo$description)) > 0) {
      
      a0 <- subset(geo, !(geo %in% trad.geo$description))
      
      stop(paste("Some element in 'geo' argument is misspecified:", paste0(a0, collapse = " & ")))
      
    }
    
    
    
    # geo e geo.filter
    if (is.null(geo) || geo == "Brazil") {
      
      path_geo <- "n1/1"
      
      if (!is.null(geo.filter)) {
        message("No filter is necessary in 'geo.filter' argument once 'geo' is set to 'Brazil' (default)")
      }
      
    } else if (length(geo.filter) > length(geo)) {
      
      if (is.null(geo) || geo == "Brazil") {
        
        message("No filter is necessary in 'geo.filter' argument once 'geo' is set to 'Brazil' (default)")
        
      } else {
        
        stop("The geo.filter argument must have the same or less length than 'geo'")
        
      }
      
    } else if (length(geo.filter) <= length(geo)) {
      
      for (i in 1:length(geo)) {
        
        if (is.null(names(geo.filter[i])) || names(geo.filter[i]) == "") {
          
          if (is.null(geo.filter[i])) {
            
            geo.filter[i] <- "all"
            names(geo.filter)[i] <- geo[i]
            
          } else {
            
            names(geo.filter)[i] <- geo[i]
            
          }
          
        }
        
      }
      
      a1 <- 1:length(geo)
      a2 <- 1:length(geo.filter)
      a3 <- subset(a1, !(a1 %in% a2))
      
      if (any(a3)) {
        
        for (j in a3) {
          
          geo.filter[[j]]<- "all"
          names(geo.filter)[[j]] <- geo[j]
          
        }
        
      }
      
      g1 <- data.frame(geo_desc = geo)
      g1 <- suppressWarnings(merge(g1, trad.geo, by.x = "geo_desc", by.y = "description"))
      
      g2 <- data.frame(geo_desc = names(geo.filter))
      g2 <- suppressWarnings(merge(g2, trad.geo, by.x = "geo_desc", by.y = "description"))
      names(g2) <- paste0(names(g1), "2")
      
      g3 <- cbind(g1, g2)
      g3$relation <- ifelse(g3$level == g3$level2, 0, ifelse(g3$level < g3$level2, 1, 0))
      
      if (sum(g3$relation) != 0) {stop("Some element in 'geo.filter' is misspecified")}
      
      g3$relation2 = ifelse(g3$level == g3$level2, 0, 1)
      
      path_geo_temp <- list()
      
      for (h in 1:length(geo)) {
        
        if (g3$relation2[h] == 0) {
          
          path_geo_temp[[h]] <- paste0(g3$cod2[h], "/", paste(geo.filter[[h]], collapse = ","))
          
        } else {
          
          path_geo_temp[[h]] <- paste0(g3$cod[h], "/in%20", g3$cod2[h], "%20", paste(geo.filter[[h]], collapse = ","))
          
        }
        
      }
      
      path_geo <- paste(unlist(path_geo_temp), collapse = "/")
      
    }
    
    
    
    # Classificaoes e categorias (ou secoes)
    if (is.null(classific) || classific == "all") {
      
      if (!is.null(category)) {
        message("Considering all categories once 'classific' was set to 'all' (default)")
      }
      
      category <- NULL
      
      path_classific <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))
      path_classific <- rvest::html_nodes(path_classific, "table")
      path_classific <- rvest::html_table(path_classific, fill = TRUE, trim = TRUE)
      path_classific <- unlist(path_classific)
      path_classific <- stringr::str_extract(path_classific, "\\C[0-9]+")
      
      if (sum(!is.na(path_classific)) == 0) {
        
        path_classific <- ""
        
      } else {
        
        path_classific <- stringr::str_subset(path_classific, "\\C[0-9]+")
        path_classific <- base::tolower(path_classific)
        path_classific <- paste0("/", paste0(path_classific, "/all", collapse = "/"))
        
      }
      
    } else if (!is.null(classific)) {
      
      if (is.null(category) || (is.character(category) & category == "all")) {

        path_classific <- paste0("/", paste0(classific, "/all", collapse = "/"))
        
      } else if (!is.list(category)) {
        
        stop("If not 'all', 'category' must be an object of type 'list'")
        
      } else if (length(category) > length(classific)) {
        
        stop("The length of 'category' must be equal or less than 'classific' argument")
        
      } else if (length(category) == length(classific)) {
        
        path_classific <- ""
        
        for (i in 1:length(category)) {
          
          path_classific <- paste0(path_classific, "/", paste0(classific[i], "/", paste0(category[[i]], collapse = ",")))

        }
        
      } else if (length(category) < length(classific)) {
        
        for (i in 1:length(category)) {
          
          path_classific <- paste0(classific[i], "/", paste0(category[[i]], collapse = ","))
          
        }
        
        path_classific <- paste0("/", paste0(path_classific, "/", paste0(classific[-c(1:length(category))], "/all", collapse = "/")))
        
      }
      
    }
    
    
    # period
    if (!is.character(period) & is.null(names(period))) {
      
      stop("The 'period' argument must be an object of type character")
      
    } else if (!is.null(names(period))) {
      
      if (length(period) != 1) {
        
        stop("only one element is possible when named vector ('last' or 'first') is present")
        
      } else if(!(names(period) == "last" | names(period) == "first")){
        
        stop("The element's 'name' attribute must be 'last' or 'first'")
        
      } else {
        
        period <- paste0(names(period), "%20", period)
        
      }
      
      
    } else {
      
      period <- paste0(period, collapse = ",")
      
    }
    
    
    # header
    if ( header == TRUE | header == T) {
      
      path_header = "y"
      
    } else if (header == FALSE | header == F) {
      
      path_header = "n"
      
    } else {
      
      stop("Only TRUE or FALSE")
      
    }
    
    
    # Cod and descriptions
    if (format == 4 || is.null(format)) {
      
      format <- "/f/a"
      
    } else if (format == 3) {
      
      format <- "/f/u"
      
    } else if (format == 2) {
      
      format <- "/f/n"
      
    } else if (format == 1) {
      
      format <- "/f/c"
      
    } else {
      
      warning("The format argument is misspecified. Considering defaut specification.")
      
    }
    
    
    # digits
    if (digits == "default" || is.null(digits)) {
      
      digits <- "/d/s"
      
    } else if (digits == "max") {
      
      digits <- "/d/m"
      
    } else if (digits >= 0 & digits <= 9) {
      
      digits <- paste0("/d/", digits)
      
    } else {
      
      warning("The digits argument is misspecified. Considering defaut specification.")
      
    }
    
    path <- RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values",
                                 "/t/", x, "/",
                                 path_geo,
                                 "/p/", period,
                                 "/v/", variable,
                                 path_classific,
                                 format, "/h/",
                                 path_header,
                                 digits),
                          ssl.verifyhost=FALSE,
                          ssl.verifypeer=FALSE)
    
    
  } else {
    
    if (!is.character(api)) stop("The 'api' argument must be a character vector")
    if (length(api) != 1) stop("The 'api' argument must have the length equals to 1")
    
    message("All others arguments are desconsidered when 'api' is informed")
    
    path <- RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values",
                                 api),
                          ssl.verifyhost=FALSE,
                          ssl.verifypeer=FALSE)
    
    path_header <- "y"
    
  }
  

  test1 <- try(rjson::fromJSON(path), silent=TRUE)
  
  
  if (strsplit(path, " ")[[1]][2] == "P") {
    
    stop("The 'period' argument is misspecified.")
    
  } else if (strsplit(path, " ")[[1]][1] == "Tabela" &
             strsplit(path, " ")[[1]][3] == "Tabela"){
    
    ntable <- strsplit(path, " ")[[1]][2]
    ntable <- substr(ntable, 1, nchar(ntable)-1)
    
    stop("This table does not exists.")
    
  } else if (strsplit(path, " ")[[1]][2] == "V") {
    
    stop(sprintf("The table %s does not contain the %s variable", x, variable))
    
  } else if (grepl("Server Error", path)) {
    
    stop("Server error: Some argument is misspecified or (probabily) The query will result in a table with more than 20k values. 
         In this case, you may address to the SIDRA's site and request the data manually to be delivered by an email account.")
  
  } else if ('try-error' %in% class(test1)) {
    
    stop(path)
    
  } else {
    
    path <- rjson::fromJSON(path)
    path <- as.data.frame(do.call("rbind", path))
    
    path <- as.data.frame(lapply(path, unlist), stringsAsFactors = FALSE)

    if (path_header == "y"){
      
      colnames(path) <- unlist(path[1, ])
      path <- path[-1, ]
      
    }
    
    id <- which(colnames(path) == "V" | colnames(path) == "Valor")
    
    path[ ,id] = suppressWarnings(ifelse(unlist(path[ ,id]) != "..", as.numeric(unlist(path[ ,id])), NA))
    
  }
  
  return(path)
  
}