# sidrar 0.6.0

* Split relative or complete SIDRA values URLs with `sidra_split()`, preserving
  parameter order. Period selections `all`, `first`, `last`, and ranges can be
  frozen to the official period inventory before downloading values.
* Added opt-in period batching with `sidra_collect(batch_size = ...)` and
  resumable local checkpoints with `checkpoint = ...`. Completed batches have
  checksums and original access times; mismatched settings, schema drift,
  corrupted files, and concurrent writers fail explicitly. No implicit disk
  cache or change to `get_sidra()` is introduced.
* `info_sidra()` can use official aggregate metadata and periods when the
  original descriptor returns a browser challenge. Its five legacy components
  are preserved; fields unavailable from the alternative source are disclosed
  instead of fabricated. Both-source failures retain the original error.
* Expanded offline compatibility tests across monthly, quarterly, annual, and
  multiple-classification synthetic fixtures, including special symbols,
  UTF-8, leading-zero identifiers, and comparison by keys rather than row order.

* Validate the complete alternative-response schema on every fallback path,
  including canonical queries at default precision. Reject out-of-selection
  codes and duplicate observation keys without altering identifiers or values.
  Missing explicitly requested members emit `sidrar_incomplete_warning`;
  sparse tables are not forced into a complete Cartesian product.
* Honor `Retry-After` on HTTP 429 and 503 independently of quiet logging,
  including HTTP dates, while preserving the configured total attempt budget.
  The maximum accepted server-requested delay is configurable through
  `options(sidrar.retry_after_max = 120)`, with a default of 60 seconds.
  Longer delays return an informative error with `retry_after_max` instead of
  retrying prematurely or waiting indefinitely.
* Broadened the official aggregate API fallback to multiple territorial levels,
  complete and first-period selections, period ranges, and SIDRA URLs with
  dimensions in different orders. Dimension columns follow the original URL;
  observation order remains that returned by the alternative service.
* Explicit decimal precision, including a single variable-specific `/d/vID N`
  selection, is accepted when numeric values already have the requested number
  of decimal places. Incompatible precision raises
  `sidrar_fallback_precision_error` rather than silently rounding values or
  inventing unavailable digits. Maximum precision remains unsupported by the
  alternative route.
* Automatic classification discovery can use official aggregate metadata when
  SIDRA's table descriptor returns a Cloudflare challenge.
* Recognized additional Cloudflare challenge pages containing a challenge title
  and the official challenge host, without requiring legacy HTML markers.

# sidrar 0.5.1

* Recognized Cloudflare browser challenges as `sidrar_challenge_error`,
  preserving HTTP details and the Ray ID without printing challenge HTML (#29).
* Added a fallback for compatible values queries to IBGE's official aggregate
  API v3 when the SIDRA endpoint returns a browser challenge. The fallback
  preserves header handling, identifiers, and special values, and can be
  disabled with `options(sidrar.fallback = FALSE)`. Unsupported query options
  retain an explicit error instead of changing the requested selection.
* Collection provenance now records the actual source URLs and, when a
  fallback was used, the original `requested_urls`.
* Removed an unintended live descriptor request from the offline splitting
  tests, fixing CRAN check failures when SIDRA access is blocked.

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



