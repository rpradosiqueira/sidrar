## Resubmission
This is a resubmission. In this version I have:

* Adjusted the DESCRIPTION file in according to 
the comments of a CRAN maintaner.

## Resubmission
This is a resubmission. In this version I have:

* Adjusted the 'info_sidra' function relating to the
"no visible binding for global variable '.'" note.

## New R CMD check results

0 errors | 0 warnings | 0 note 

## Test environments
* local OS X install, R 3.3.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note 

checking R code for possible problems ... NOTE
info_sidra: no visible binding for global variable '.'

This is because of the use of "%>%" pipe operator with the 
"mutate"" function from dplyr package.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
