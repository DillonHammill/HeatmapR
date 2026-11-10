## Release summary

HeatmapR 2.0.0 is a major release. The package now contains C++ code
(through cpp11) for clustering and drawing, so it needs compilation. Parallel
loops use `std::thread`, never more threads than the user allows, and at most
two while `_R_CHECK_LIMIT_CORES_` is set; the tests use at most two threads
unless `NOT_CRAN` is `"true"`. Examples and tests draw on `pdf(NULL)` or the
current device and don't open new devices in non-interactive sessions.

## Test environments

* local: Ubuntu 24.04, R 4.3.3
* GitHub Actions: macOS, Windows and Ubuntu (R release and devel), Ubuntu
  (R oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* installed size is 8.4Mb (libs 7.1Mb with debugging symbols, 0.7Mb
  without): the package now contains compiled code.

The local check also reported notes and warnings from the sandbox it ran in
(no internet access for the incoming and URL checks, no `inconsolata.sty`
for the PDF manual, no `en_US.UTF-8` locale, the system R's
`-mno-omit-leaf-frame-pointer` flag); none come from the package.

## Reverse dependencies

To check before submission with
`tools::package_dependencies("HeatmapR", reverse = TRUE, which = "all")`
(and `revdepcheck::revdep_check()` if there are any) - not run yet, as the
local check had no access to CRAN.
