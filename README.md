
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

The original three entry points remain unchanged:

- `search_sidra()` searches the current official aggregate catalog.
- `info_sidra()` lists the parameters available for a table.
- `get_sidra()` retrieves the selected observations.

Table codes are returned as the names of the `search_sidra()` result:

``` r
search_sidra("IPCA")
info_sidra(7060)
```

An additive workflow is available for programmatic discovery, planning,
and larger requests:

- `sidra_catalog()`, `sidra_metadata()`, `sidra_periods()`, and
  `sidra_locations()` return structured discovery data.
- `sidra_query()` constructs a request without downloading values, and
  `sidra_plan()` reports the cardinalities that can be determined
  offline.
- `sidra_split()` creates explicit disjoint batches and
  `sidra_collect()` retrieves them sequentially with schema validation.

## Discover and plan

The structured discovery functions keep identifiers as character strings
and return stable base R objects:

``` r
catalog <- sidra_catalog()
metadata <- sidra_metadata(7060)
periods <- sidra_periods(7060)
brazil <- sidra_locations(7060, "N1")
```

Build and inspect a query before downloading values:

``` r
query <- sidra_query(
  x = 7060,
  variable = 63,
  period = sprintf("2024%02d", 1:12),
  geo = "City",
  geo.filter = list(City = 5002704),
  classific = "c315",
  category = list(7169)
)

query$url
sidra_plan(query)
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

## Network behavior and cache

The legacy functions continue to query current IBGE services on every
call. The structured discovery functions have an opt-in disk cache;
value responses are never cached automatically:

``` r
metadata <- sidra_metadata(7060, cache = TRUE)
sidra_cache_info()
sidra_cache_clear()
```

Cache entries expire after 30 minutes by default. Use `refresh = TRUE`
to bypass and replace an entry. Save collected values explicitly when a
reproducible snapshot is required. Requests use a timeout and limited
retries for transient failures; customize them with:

``` r
options(
  sidrar.timeout = 120,
  sidrar.retries = 4
)
```

Regular package tests are offline. Live API smoke tests run separately
on a small set of queries to detect availability and schema changes.

HTTP 429 and 503 responses honor `Retry-After` in seconds or HTTP-date
form, even when retry logging is quiet. The maximum accepted
server-requested delay defaults to 60 seconds and is configurable for
all package requests:

``` r
options(sidrar.retry_after_max = 120) # Accept server delays up to two minutes
options(sidrar.retry_after_max = NULL) # Restore the default of 60 seconds
```

Use one finite positive number of seconds; invalid settings fall back to
60. If the server requests more than this limit,
`sidrar_retry_after_error` carries the original HTTP details,
`retry_after`, and `retry_after_max`, instead of retrying early. Wait
until the permitted time before trying again. This option does not
change `sidrar.timeout` or `sidrar.retries`.

### Cloudflare access challenges

Some SIDRA values requests have returned a Cloudflare browser challenge
(`HTTP 403`, `Just a moment...`) since reports dated September 15, 2026.
This is an access restriction upstream, rather than a malformed query.

The package recognizes this response and, for compatible queries, uses
the official IBGE aggregate API v3 with `view=flat`. Existing calls to
`get_sidra()` and `sidra_collect()` can use this fallback without
changing their arguments. Version 0.6.0 extends the fallback introduced
in 0.5.1 to multiple geographic levels (including containing-level
filters), explicit periods and ranges, `all`, `first`, `first N`,
`last`, and `last N`. Explicit variable/category codes or `all`/`allxp`
are supported as appropriate, with the default descriptor format. Header
handling and `value_type` remain unchanged. A message identifies when
the alternative endpoint is used.

For example, the complete PNAD quarterly series for Brazil, regions, and
states can be requested without changing the original API path:

``` r
pnad <- get_sidra(
  api = "/t/6468/n1/all/n2/all/n3/all/v/4099/p/all/d/v4099%201",
  value_type = "both"
)
```

Dimension columns follow the order in the original URL, including
variable before period. Observation order remains that returned by the
alternative service; sort explicitly when an analysis depends on row
order. When automatic classification discovery (`classific = "all"`)
encounters a descriptor challenge, it can also use official aggregate
metadata.

`info_sidra()` now uses aggregate metadata and the period inventory
after a descriptor challenge as well. Its five legacy components are
unchanged. Unavailable descriptor-specific geographic names, active-unit
counts, and variable availability exceptions are explicitly disclosed
rather than guessed. Inspect `attr(info_sidra(7060), "sidrar_metadata")`
for the alternative source URLs and limitations. `wb = TRUE` still opens
the original descriptor page.

Explicit precision (`/d/1` or a single variable-specific `/d/v4099 1`,
with the space URL-encoded) is accepted only when returned numeric
values already have the requested decimal places. Otherwise a
`sidrar_fallback_precision_error` is raised; the alternative service
does not expose all stored digits. Values are neither rounded again nor
padded to imply unavailable precision. Default precision preserves the
received values.

Territorial views, extinct-unit options, category sums, non-default
descriptor formats, maximum precision (`digits = "max"`), and other
unsupported URL options are not translated automatically. The original
`sidrar_challenge_error` then explains the limitation in
`fallback_reason`. For URLs supplied through `api`, period, variable,
and at least one territorial level must be specified explicitly. You can
disable fallback with `options(sidrar.fallback = FALSE)`.

The alternative response is always checked for complete dimension
fields, textual identifiers, duplicate observation keys, and codes
outside explicit filters. Unexpected codes or duplicate keys are errors;
rows are never silently filtered or deduplicated. Missing explicitly
requested members instead emit `sidrar_incomplete_warning`, with details
in its `missing` field. This may mean unavailable data rather than
truncation: sparse tables need not contain every possible combination,
and missing cells are not filled with zero.

These local checks do not establish full coverage of `all`, exact
membership of `first`/`last`, or containing-level geography filters.
Those comparisons require current catalog or territorial metadata. The
checks add no metadata requests of their own.

If both official endpoints are unavailable, the package cannot restore
access itself. Report the failing URL, access time, and `cf_ray` from
the condition to IBGE. Increasing retries does not solve browser
challenges. With `sidra_collect(..., provenance = TRUE)`, `urls` records
the actual endpoints and `requested_urls` records the original queries
when fallback occurs.

### Requests above the SIDRA value limit

When SIDRA rejects a request because it exceeds the service’s
per-request value limit, `get_sidra()` raises a `sidrar_limit_error`.
Its message reports the requested count, the current limit, and the
minimum number of calls. The condition also inherits from
`sidrar_http_error`, so existing error handlers continue to work.

`sidra_split()` partitions one explicit dimension without splitting
category sums or changing the geographic level. A geographic filter is
splittable only when the query requests one non-Brazil level; otherwise
unchanged territorial levels could overlap between calls. Period
selectors `all`, `first`, `last`, and ranges are expanded using the
official period inventory; gaps are not filled with invented periods.
Relative or complete API URLs can also be split. `sidra_collect()` runs
the resulting queries sequentially and refuses to combine incompatible
schemas:

``` r
query <- sidra_query(
  x = table_code,
  variable = variable_code,
  period = period_code,
  geo = "City",
  classific = classification_code,
  category = list(category_codes)
)

batches <- sidra_split(query, by = "category", size = 8)
data <- sidra_collect(batches, provenance = TRUE)
sidra_provenance(data)
```

### URL batching and resumable collection (0.6.0)

For automatic period batching, supply `batch_size` to `sidra_collect()`.
Optionally keep successful batches in a dedicated checkpoint directory:

``` r
url <- "/t/6468/n1/all/n2/all/n3/all/v/4099/p/all/h/n"

pnad <- sidra_collect(
  url,
  value_type = "both",
  batch_size = 8,             # At most eight periods per request
  checkpoint = "sidrar-pnad",
  provenance = TRUE
)

# If interrupted, run the same call again: verified completed batches are reused.
sidra_provenance(pnad)$batch_accessed_at
sidra_provenance(pnad)$resumed

# Or inspect a split before downloading any values:
batches <- sidra_split(url, by = "period", size = 8)
batches$resolution$selection
```

The appropriate group size depends on every selected dimension; even one
period can exceed the service’s value limit. In that case, split another
explicit dimension (variable, category, or a safe geographic filter) as
well. `get_sidra()` remains a single-request interface. Neither batching
nor saving values to disk is enabled implicitly in ordinary calls.

Checkpointed queries freeze relative periods even without `batch_size`.
Resuming validates query settings, package version, checksums, and batch
schemas. It does not refresh old results: use a new directory for a new
snapshot, particularly when upstream data may have been revised.
Provenance keeps each batch’s original download time. Checkpoints are
trusted local RDS files. Warnings are saved and signaled again on
resume, including incomplete-coverage diagnostics. Checkpoints remain
separate from the metadata cache; `sidra_cache_clear()` does not remove
them. The final result is still combined in memory.

Concurrent use of a checkpoint is rejected. After a hard process
interruption, confirm no collector is running before manually removing a
leftover `.sidrar-lock` directory. Existing or corrupt checkpoints are
never silently deleted. URL splitting rejects ambiguous/unsupported
selectors; geographic containment splitting remains available through
structured queries only.

### Geographic identifiers

Keep geographic identifiers as character strings. SIDRA neighborhood
codes (`geo = "Neighborhood"`) identify its neighborhood territorial
level; they are not census tract identifiers and must not be joined
directly to census tract geometries without an official correspondence.

Structured queries also support official territorial views and extinct
territorial units:

``` r
sidra_query(1612, geo_view = 44, classific = character())
sidra_query(
  1612,
  geo = "State",
  geo.filter = list(c(20, 34)),
  include_extinct = TRUE,
  classific = character()
)
```

For more examples, see the [“Introduction to
sidrar”](https://CRAN.R-project.org/package=sidrar/vignettes/Introduction_to_sidrar.html)
vignette and the [official SIDRA API
documentation](https://apisidra.ibge.gov.br/home/ajuda).
