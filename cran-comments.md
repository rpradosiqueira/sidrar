## Release summary

This update modernizes the package after changes to IBGE's SIDRA services.
It replaces HTML scraping with official JSON endpoints, restores table
search, fixes complete API URLs and header-free responses, and adds tests
while preserving the existing `get_sidra()` defaults and return schema.

## Test environments

* Windows 11 x64 (build 26200), R 4.6.0 (ucrt)
* win-builder, Windows Server 2022 x64, R-devel
  (2026-07-22 r90289 ucrt)

## R CMD check results

`R CMD check --as-cran --no-manual` was run on the source tarball with CRAN
incoming remote checks enabled.

The same source tarball was checked on win-builder with R-devel, including
the PDF and HTML manuals.

0 errors | 0 warnings | 0 notes

## Downstream compatibility

The current reverse imports (`datazoom.amazonia`, `PNADCperiods`, and
`SidraFacil`) were audited for their use of the public API. Compatibility
tests cover the existing argument order, base `data.frame` output, Portuguese
column names and order, numeric `Valor`, relative percent-encoded API paths,
and the historical `info_sidra()` list structure.
