# Clean-room rewrite for the MIT licence (November 2026)

HeatmapR moved from the GPL-2 to the MIT licence in 2.0.0. Parts of its
compiled code had followed GPL-licensed code:

- centroid and median linkage and the Lance-Williams updates followed R's
  `hclust.f`;
- Hartigan-Wong k-means followed R's `kmns.f`;
- the dip followed the `diptest` package;
- distances and correlations with missing values, Canberra and binary
  distances, Kendall's tau, quantiles, means, standard deviations and colour
  ramps followed R's own arithmetic.

These functions were deleted and rewritten in a clean room.

## Process

1. **Specifications.** One person studied the old code and wrote the
   specifications in this folder: `SPEC-common.md` plus one per component.
   They describe interfaces and observable behaviour only, with the papers
   and R help pages to work from, and no code.
2. **Sanitised copies.** Each component was implemented in its own copy of
   the package with:
   - the GPL-derived functions removed and replaced by stubs;
   - no git history;
   - the GPL sources deleted from the machine.
3. **Implementers.** A separate implementer that had never seen the old code
   or the GPL sources wrote each component: four fresh AI agents (Claude),
   one per component. The rules (`SPEC-common.md`) forbade:
   - reading R's or any package's source, including printing R functions
     that call compiled code;
   - reading published code listings (AS 136, AS 217) or translations of
     them;
   - reading anything outside the implementer's workspace.

   R's functions were used only as black boxes, to compare results.
4. **Integration.** The new files replaced the old ones in the package, and
   the full test suite and `R CMD check` were run.

## What each component was written from

| Component | Files | Sources |
|---|---|---|
| Linkage | `src/linkage.cpp`: `lance_williams()`, `generic_impl()`, `hclust_form()` | Müllner (2011), section 3.1, read as a rendering of the arXiv paper; Lance and Williams (1967) coefficients; `?hclust`; `stats::hclust()` as a black box |
| k-means | `src/kmeans.cpp`: `kmeans_hw_cpp()` | The description of the algorithm in Hartigan and Wong (1979), Method steps 1-7 and Fault diagnostics; `?kmeans`; `stats::kmeans()` as a black box for the details the text leaves open |
| Dip | `src/dip.cpp`: `dip_sorted()` | The definition of the dip and the outline of its computation in Hartigan and Hartigan (1985), worked out from first principles; `diptest::dip()` as a black box for the scale and ties |
| Numerics | `src/metric.cpp`, `src/scale.cpp`, `src/sparse.cpp`, `src/colour.cpp` | R's help pages (`?dist`, `?cor`, `?quantile`, `?mean`, `?sd`, `?approx`, `?colorRamp`, `?rgb`), Kendall (1945), Hyndman and Fan (1996), and black-box comparisons with R |

Notes on the sources:

- **k-means:** the network refused the paper's PDF, so the implementer first
  worked from the paper's description as it knew it. The author then supplied
  the paper, and the implementer was given only its text pages, with the code
  listings blanked out. It checked every step against them and found three
  points where the code follows `stats::kmeans()` rather than the literal
  text:
  - Step 4b also searches the point's IC2;
  - Step 6 doesn't switch on an exact tie;
  - with k = 2, the fit stops after the first quick-transfer stage.

  The comments in `src/kmeans.cpp` cite the steps and mark each detail
  settled against `stats::kmeans()`.
- **Dip:** the paper's page couldn't be fetched, so the computation was
  worked out from the definition.

## Results

- **Linkage:** gives `stats::hclust()`'s merges and order exactly on
  continuous data, and heights to a relative 1e-12 (in practice bit for bit).
  On tied data, the trees are valid and take tied pairs in the order of the
  clusters' first leaves.
- **k-means:** identical to `stats::kmeans()` (clusters, centres, iterations
  and status) on about 6,000 problems.
- **Dip:** equals `diptest::dip()` to a relative 1e-12. `data-raw/dip-check.R`
  compares them.
- **Numerics:** bit for bit with R in nearly every case; see
  `tests/testthat/test-numerics-r.R`.
