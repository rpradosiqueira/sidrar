## Release summary

sidrar 0.6.0 extends the official aggregate API fallback introduced in 0.5.1,
covering the multi-geography and complete-period queries reported by users
after the SIDRA browser-challenge changes. It adds strict response validation,
configurable Retry-After limits, URL-based batching, resumable opt-in local
checkpoints, and an official metadata fallback for info_sidra(). Unsupported
selections still fail explicitly instead of being silently changed.

There are no new dependencies or exports. The legacy get_sidra(), info_sidra(),
and search_sidra() signatures and default return contracts are unchanged.
Optional arguments were appended to sidra_collect(); existing calls are
unaffected. No files are written by default. All examples and regular tests
are offline; checkpoint tests use temporary directories and clean up.

## Test environments and results

* Windows 11 x64 (build 26200), R 4.6.0 (2026-04-24 ucrt).

R CMD build and R CMD check --no-manual: 0 errors, 0 warnings, 0 notes.

R CMD check --as-cran on the exact same source tarball, with incoming remote
checks and PDF/HTML manuals enabled: 0 errors, 0 warnings, 1 NOTE:

    Days since last update: 1

This check was run on September 19, 2026; sidrar 0.5.1 was published on
September 18. The maintainer chose to wait for the previous version's CRAN
check matrix to finish updating before submitting 0.6.0. These comments will
be refreshed before the actual upload; this file is not a submission receipt.

The final test suite passed 2420 expectations, with no failures or test
warnings and nine opt-in live tests skipped. Bounded live queries succeeded
for IPCA, PNAD, Census, and agricultural data. Raw values from the primary and
alternative official APIs matched by identifiers for a four-observation
query with five classifications in two different classification orders.
The checked tarball also passed a two-period checkpoint/resume smoke test.

The exact tarball was uploaded to win-builder R-devel on September 19;
the email result is pending. No R-devel result is claimed here yet.

## Existing CRAN checks

At the September 19 preflight, the check matrix still mixed sidrar 0.5.0
and 0.5.1. The outstanding r-patched-linux test error was for 0.5.0 and the
unintended descriptor request already corrected in 0.5.1. The displayed
0.5.1 checks were OK. That offline regression test remains covered in 0.6.0.

## Downstream compatibility

The current reverse imports were checked against the final 0.6.0 tarball
in an isolated library with R CMD check --no-manual and all required
dependencies available:

* datazoom.amazonia 1.2.0: OK
* PNADCperiods 0.1.2: OK
* SidraFacil 1.0.2: OK

PNADCperiods' internal test reporter counted 19 warnings and 15 skips,
with 1602 passes and no failures. These counts are identical to the recorded
0.5.1 baseline. All three checks finished with 0 errors, warnings, or notes;
no new downstream regression was observed.
