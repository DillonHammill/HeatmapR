## Release summary

HeatmapR 2.0.0 is a major release. The package now contains C++ code
(through cpp11) for clustering and drawing, so it needs compilation. Parallel
loops use `std::thread`, never more threads than the user allows, and at most
two while `_R_CHECK_LIMIT_CORES_` is set; the tests use at most two threads
unless `NOT_CRAN` is `"true"`. Examples and tests draw on `pdf(NULL)` or the
current device and don't open new devices in non-interactive sessions.

The licence changes from GPL-2 to MIT with this release. The author holds
the copyright to all of the package's code. HeatmapR 1.1.0 had no compiled
code; its only code derived from another package, modified functions of
the GPL-licensed `ggdendro` package, has been removed. Compiled code
written during the development of 2.0.0 that followed GPL-licensed code
(R's `hclust()` and `kmeans()` and the `diptest` package) has been
rewritten from the published algorithms.

## Test environments

* local: Ubuntu 24.04, R 4.3.3
* GitHub Actions: macOS, Windows and Ubuntu (R release and devel), Ubuntu
  (R oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* installed size is 14.5Mb (libs 12.1Mb with debugging symbols, 1.2Mb
  without): the package now contains compiled code.

The local check also reported notes and warnings from the sandbox it ran in
(no internet access for the incoming and URL checks, no `inconsolata.sty`
for the PDF manual, no `en_US.UTF-8` locale, the system R's
`-mno-omit-leaf-frame-pointer` flag); none come from the package.

## Reverse dependencies

There are no reverse dependencies: the CRAN page of HeatmapR 1.1.0 lists
no packages that depend on, import, suggest or link to it.
