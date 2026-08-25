# sidrar 0.5.0

* Added an actionable `sidrar_limit_error` when the SIDRA API rejects a
  request for exceeding its per-request value limit. The condition remains a
  `sidrar_http_error` and records the requested count, limit, minimum number
  of batches, response body, and request URL (#28).
* Added structured discovery through `sidra_catalog()`, `sidra_metadata()`,
  `sidra_periods()`, and `sidra_locations()` using official IBGE JSON
  endpoints. Identifiers remain character vectors and returned schemas are
  stable when upstream responses are empty or gain fields.
* Added `sidra_query()` and `sidra_plan()` to inspect URLs, explicit dimension
  cardinalities, and optional limit risk before retrieving values. No service
  limit is hard-coded.
* Added explicit, sequential batching with `sidra_split()` and
  `sidra_collect()`. Batches must be disjoint, retain their original order,
  and have identical names and column types before rows are combined.
* Added opt-in collection provenance through `sidra_provenance()` and opt-in
  disk caching for normalized catalog and metadata responses. Value responses
  are never cached by the package.
* Added territorial-view (`G`) queries, optional extinct units (`/u/y`),
  case-insensitive geographic aliases, and supported `nNN` level codes.
* All HTTP errors now carry status, response body, and URL fields. Transport
  failures can additionally identify timeout, TLS, DNS, connection, and
  transient failures while remaining `sidrar_http_error` conditions.
* Improved local input validation, reset row names after processing API
  headers, rejected structural URL delimiters and duplicate classifications,
  and clarified that `value_type` still applies when `api` is used.
* Clarified that SIDRA neighborhood identifiers belong to its territorial
  level and are not census tract identifiers (#8).

# sidrar 0.3.0

* Replaced fragile HTML scraping with official JSON endpoints for table
  descriptors and the aggregate catalog.
* Restored `search_sidra()` after changes to the SIDRA website. Searches are
  now case- and accent-insensitive, support multiple non-adjacent terms, and
  return table codes as names (#24).
* Reworked `info_sidra()` while preserving its historical list components:
  `table`, `period`, `variable`, `classific_category`, and `geo`.
* Fixed `get_sidra(api = ...)` for complete official HTTPS URLs and preserved
  percent-encoded paths. Thanks to @viniciusoike for reporting the problem and
  proposing a solution in #25 and #26.
* Fixed direct API response parsing so changed or short server responses
  produce informative package errors instead of failing inside error handling
  (#16).
* Requests containing `/h/n` now preserve the first observation instead of
  treating it as a header.
* Fixed vector geographic filters and the construction of queries containing
  multiple classifications and partially specified category lists.
* Added `value_type` to preserve SIDRA special symbols in a character column
  or alongside the historical numeric value column (#18).
* Added HTTPS status checks, informative API errors, UTF-8 decoding, an
  identifying user agent, configurable timeouts, and retries for transient
  failures.
* Added support for intermediary and immediate geographic regions.
* Reduced runtime dependencies to `httr`, `jsonlite`, and `utils`.
* Added unit tests, opt-in live API tests, and continuous integration across
  current R platforms.

# sidrar 0.2.9

* Address SSL error

# sidrar 0.2.8

* Minor adjusts to address CRAN warnings

# sidrar 0.2.7

* Adjusts in the Vignette to CRAN

# sidrar 0.2.6

* Fixed bug in get_sidra (Issue #10)
* Minor change in the Vignette

# sidrar 0.2.5

* Fixed bugs in get_sidra (Issue #5, #6)
* "tidyr" and "dplyr" package dependency removed

# sidrar 0.2.4

* Fixed bug in search_sidra (Issue #2)
* Better error messages
* Vignette updated

# sidrar 0.2.1

* Fixed list-column in resultant data.frame in get_sidra (Issue #1)

# sidrar 0.2.0

* New argument in get_sidra ("api")

# sidrar 0.1.1

* Better messages depending of the input arguments

# sidrar 0.1.0

* Initial version



