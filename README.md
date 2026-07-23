
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sidrar

[![CRAN
status](https://www.r-pkg.org/badges/version/sidrar)](https://CRAN.R-project.org/package=sidrar)
[![R-CMD-check](https://github.com/rpradosiqueira/sidrar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rpradosiqueira/sidrar/actions/workflows/R-CMD-check.yaml)

`sidrar` provides direct access from R to aggregate data and metadata
published by the Brazilian Institute of Geography and Statistics (IBGE).
SIDRA stands for *Sistema IBGE de Recuperação Automática*.

## Installation

Install the released version from CRAN:

``` r
install.packages("sidrar")
```

Install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("rpradosiqueira/sidrar")
```

## Main functions

The package has three public functions:

- `search_sidra()` searches the current official aggregate catalog.
- `info_sidra()` lists the parameters available for a table.
- `get_sidra()` retrieves the selected observations.

Table codes are returned as the names of the `search_sidra()` result:

``` r
search_sidra("IPCA")
info_sidra(7060)
```

## Retrieve data

This example requests the monthly IPCA for the general index in Campo
Grande, Mato Grosso do Sul, over the 12 most recent periods:

``` r
library(sidrar)

ipca <- get_sidra(
  x = 7060,
  variable = 63,
  period = c(last = 12),
  geo = "City",
  geo.filter = list(City = 5002704),
  classific = "c315",
  category = list(7169)
)
```

You may also pass either a relative API path or a complete official
HTTPS URL. A request containing `/h/n` is returned without consuming its
first observation as a header:

``` r
ipca_brazil <- get_sidra(
  api = paste0(
    "https://apisidra.ibge.gov.br/values/",
    "t/7060/n1/all/v/63/p/last/c315/7169"
  )
)
```

## Preserve special values

By default, `Valor` is numeric for compatibility with earlier releases.
SIDRA also uses symbols such as `"-"`, `"X"`, `".."`, and `"..."`. Use
`value_type = "character"` to preserve them in `Valor`, or
`value_type = "both"` to append `Valor_raw` while retaining numeric
`Valor`:

``` r
data <- get_sidra(
  api = "/t/1849/n3/all/v/811/p/2018/c12762/all",
  value_type = "both"
)
```

For more examples, see the [“Introduction to
sidrar”](https://CRAN.R-project.org/package=sidrar/vignettes/Introduction_to_sidrar.html)
vignette and the [official SIDRA API
documentation](https://apisidra.ibge.gov.br/home/ajuda).
