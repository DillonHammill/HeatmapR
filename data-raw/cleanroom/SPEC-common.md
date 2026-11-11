# Clean-room rewrite of parts of HeatmapR: rules for every component

HeatmapR is an R package (C++ through cpp11) that is moving from the GPL-2 to the MIT licence. A few functions in its compiled code followed GPL-licensed code (from R itself and from the `diptest` package). They have been deleted from your copy and replaced by stubs that throw `"clean-room stub: <name>"`. Your job is to write new implementations of the stubs in your component **from the specification, the published papers and R's documented behaviour only**, so that the new code is HeatmapR's own and can be released under MIT.

## What you must not look at

The point of the exercise is provenance, so these rules are strict:

- Do not read, download or search for the source code of R or its packages: no R source tarballs, no GitHub mirrors of R (`wch/r-source` or similar), no `stats`, `grDevices`, `diptest`, `cluster` or `fastcluster` source files. That includes R-level source that calls compiled code (e.g. printing `stats::kmeans`), and the C, C++ or Fortran behind it.
- Do not read published code listings of the algorithms. In particular, do not read the Fortran listings printed with Applied Statistics algorithms (AS 136, AS 217, etc.), StatLib's `apstat` collection, or translations of them.
- Do not read anything outside your workspace directory, except R's installed libraries (to run them) and the documents you find on the web that are allowed below. In particular, do not read `/home/claude/heatmapr-sc`, `/home/claude/scratch`, `/home/claude/lib_*` or any other copy of HeatmapR. Your copy of HeatmapR has no git history; don't try to recover one.
- Don't use code you remember from those sources. Write the code from the algorithm's description.

## What you may use

- This specification and the papers it cites. You may fetch the papers themselves (arXiv, journal pages, open PDFs) and read their prose, equations and pseudocode.
- R's help pages (e.g. `?hclust`, `?dist`, `?cor`, `?kmeans`, `?quantile`, `?approx`, `?rgb`). They describe behaviour, not code.
- R functions and installed packages as **black boxes**: run them and compare their outputs with yours (e.g. `stats::hclust()`, `stats::kmeans()`, `stats::dist()`, `cor()`, `quantile()`, `mean()`, `sd()`, `approx()`, `grDevices::rgb()`, `col2rgb()`, `diptest::dip()`). This is how you check your results.
- The rest of your HeatmapR copy: its other C++ and R code, its tests, its comments.
- General numerical-analysis knowledge (e.g. compensated or two-pass sums, how floating-point rounding and fused multiply-adds behave).

## How to work

- Your workspace is `/home/claude/cleanroom/<component>/`. The package is in `HeatmapR/` there. Install it into your own library, which is `lib/` in your workspace:

  ```
  cd /home/claude/cleanroom/<component>/HeatmapR
  R CMD INSTALL --no-test-load -l ../lib .
  ```

- Run tests like this. A single test file is selected with `filter`, a regular expression on the file name after `test-`:

  ```
  NOT_CRAN=true Rscript -e 'library(testthat); library(HeatmapR, lib.loc = "../lib"); test_dir("tests/testthat", filter = "kmeans-summarise", load_package = "none", reporter = "summary", env = new.env(parent = asNamespace("HeatmapR")))'
  ```

- Expected failures:
  - Tests that use `local_mocked_bindings` error in this harness. Ignore those.
  - The other components are stubs in your copy too. A test that fails with `clean-room stub: <name>` for a function outside your component is expected; ignore it.
  - Run the tests your spec lists. Add new tests for your component in a new file `tests/testthat/test-cleanroom-<component>.R`, comparing with the black-box oracles. Keep the tests that exist. If an existing test in your area demands something this rewrite can no longer promise (e.g. bit-identical results where the spec allows rounding differences), change it as little as possible and explain why in your report.
- Change only the files your spec names, plus your new test file and the tests the spec says you may adjust. Don't refactor other code.
- Follow the package's style:
  - C++17 for cpp11.
  - Comments in British English that say what the code does and why.
  - All-capitals comments for steps inside functions, e.g. `// THE CLOSEST PAIR`.
  - No new dependencies.
  - The code must compile without warnings with GCC and Clang, and be portable to Windows (MinGW), macOS (Apple Clang, ARM) and Linux. No OpenMP: use `parallel.h` (`par::parallel_for`) for threads, if at all.
- Above each new function, write a short provenance note naming what it was written from, e.g. `// Written from Mullner (2011), section 3.1, and stats::hclust()'s documented results`.
- Performance matters: these functions run on single-cell data (10^4 to 10^6 rows). Each spec gives a target.

## What to hand back

When you finish, reply with a short report covering:

1. The files you changed.
2. What you wrote each function from: papers, sections, help pages.
3. The design, in a few sentences.
4. Test results: which tests you ran, which passed, and any failures you think are expected, with the reason.
5. How your results compare with the oracles (how many cases, exact or within what tolerance).
6. Timings against the targets.
7. Anything in the spec you couldn't meet or had to interpret.
8. A confirmation that you followed the rules above.
