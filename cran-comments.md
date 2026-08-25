## Release summary

This release adds structured discovery, query planning, explicit sequential
batching, opt-in metadata caching, and result provenance. It also adds an
actionable `sidrar_limit_error` for requests rejected by SIDRA's per-request
value limit and support for territorial-view queries. The existing
`get_sidra()`, `search_sidra()`, and `info_sidra()` interfaces, defaults, and
return contracts remain compatible.

## Test environments

* Windows 11 x64 (build 26200), R 4.6.0 (2026-04-24 ucrt)

## R CMD check results

`R CMD check --as-cran` was run on the source tarball with CRAN incoming
remote checks enabled, including the PDF and HTML manuals.

0 errors | 0 warnings | 0 notes

Six opt-in integration tests against the live SIDRA API also passed. They are
skipped during ordinary package checks.

## Downstream compatibility

The current reverse imports were checked against the final source tarball
with `_R_CHECK_FORCE_SUGGESTS_=true`:

* `datazoom.amazonia` 1.2.0: OK
* `PNADCperiods` 0.1.2: OK
* `SidraFacil` 1.0.2: OK

Their uses of the legacy `get_sidra()` and `info_sidra()` interfaces were also
audited. Compatibility tests in `sidrar` cover the existing argument order,
base `data.frame` output, Portuguese column names and order, numeric `Valor`,
relative percent-encoded API paths, and the historical `info_sidra()` list
structure.
