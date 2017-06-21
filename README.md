
<!-- README.md is generated from README.Rmd. Please edit that file -->
sidrar
======

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/sidrar)](https://CRAN.R-project.org/package=sidrar) [![CRAC\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/sidrar)](https://CRAN.R-project.org/package=sidrar)


The goal of *sidrar* is to provide direct access to the data of IBGE's (Brazilian Institute of Geography and Statistics) SIDRA API within the R environment in an easy and flexible way. SIDRA is the acronym to "Sistema IBGE de Recuperação Automática" and it is the system where IBGE makes aggregate data from their researches available.

Installation
------------

Install the release version from CRAN:

``` r
install.packages("sidrar")
```

or the development version from github

``` r
# install.packages("devtools")
devtools::install_github("rpradosiqueira/sidrar")
```

Functions
---------

For the time being, the "sidrar" package contains only three functions:

``` r
get_sidra          It recovers data from the given table
                   according to the parameters
                   
info_sidra         It allows you to check what parameters
                   are available for a table
                   
search_sidra       It searches which tables have a particular 
                   word in their names
```

Example
-------

Let's assume that we want the IPCA (Índice de Preços ao Consumidor Amplo) for the city of Campo Grande/MS. However, we want to recover only the overall percentage rate in the last 12 months. To do this simply execute:

``` r
library(sidrar)

get_sidra(x = 1419,
          variable = 63,
          period = c(last = "12"),
          geo = "City",
          geo.filter = 5002704,
          classific = "c315",
          category = list(7169),
          header = FALSE,
          format = 3)
```

To more examples, see the vignette ["Introduction to sidrar"](https://CRAN.R-project.org/package=sidrar/vignettes/Introduction_to_sidrar.html).
