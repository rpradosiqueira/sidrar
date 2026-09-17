## Release summary

This is a corrective update to sidrar 0.5.0 in response to upstream SIDRA
Cloudflare access challenges reported since September 15, 2026 (GitHub #29).
Compatible values queries can use IBGE's official aggregate API v3 after a
recognized browser challenge. Unsupported selections fail explicitly rather
than being silently changed. No dependencies, public arguments, exports, or
default return contracts have been changed.

This update also fixes the current CRAN check failures in test-collect.R,
including the M1mac additional issue: a splitting test unintentionally called
the live table descriptor. The test now mocks that descriptor and remains
fully offline.

## Test environments

* Windows 11 x64 (build 26200), R 4.6.0 (2026-04-24 ucrt)
* macbuilder, Apple M1, macOS Tahoe 26.6, R 4.6.1 Patched
  (2026-07-27 r90311)
* win-builder, Windows Server 2022 x64 (build 20348), R Under development
  (unstable) (2026-09-16 r90549 ucrt)

## R CMD check results

`R CMD check --as-cran` was run locally on the source tarball with CRAN
incoming remote checks and PDF/HTML manuals enabled. The exact same source
tarball was checked on macbuilder (including its PDF manual):
https://mac.R-project.org/macbuilder/results/1789671693-2ea5a02f7b9a0712/

0 errors | 0 warnings | 0 notes

The same tarball passed win-builder R-devel, including CRAN incoming checks
and PDF/HTML manuals: 0 errors, 0 warnings, 0 notes.
https://win-builder.r-project.org/FhE9jMW9Key9/

The local package test suite passed 691 expectations, with no failures or
warnings and seven opt-in live tests skipped. A separate bounded live check
of the installed 0.5.1 reproduced the query from issue #29 using only api =
url and returned all four expected observations through the official fallback.

## Downstream compatibility

The current reverse imports were checked against the final source tarball
in an isolated library with `R CMD check --no-manual` and
`_R_CHECK_FORCE_SUGGESTS_=true`:

* `datazoom.amazonia` 1.2.0: OK
* `PNADCperiods` 0.1.2: OK
* `SidraFacil` 1.0.2: OK

PNADCperiods' internal testthat reporter counted 19 warnings and 15 skips,
with 1602 passes and no failures. A complete baseline check with sidrar 0.5.0
returned the same counts and Status OK; no increase was observed.

Their uses of the legacy `get_sidra()` and `info_sidra()` interfaces were also
audited. Compatibility tests in `sidrar` cover the existing argument order,
base `data.frame` output, Portuguese column names and order, numeric `Valor`,
relative percent-encoded API paths, and the historical `info_sidra()` list
structure.
