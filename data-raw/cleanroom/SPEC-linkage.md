# SPEC-linkage: agglomerative clustering in `src/linkage.cpp`

Read `SPEC-common.md` first. Your component is three stubs in `src/linkage.cpp`: `lance_williams()` (section 2), `generic_impl()` (section 3) and `hclust_form()` (section 4). Section 1 describes the code around them, which you keep. Other functions in the file (the nearest-neighbour chain `nn_chain_impl()`, Prim's algorithm for single linkage, the thread team, `Tri`, `tree_list()` and the R entry points) are HeatmapR's own and stay as they are. `nn_chain_impl()` calls your `lance_williams()`, so it must work for the methods the chain uses.

## 1. Context

**Method codes**, as `stats::hclust()` numbers them:

| Code | Method |
|---|---|
| 1 | `ward.D` |
| 2 | `single` |
| 3 | `complete` |
| 4 | `average` (UPGMA) |
| 5 | `mcquitty` (WPGMA) |
| 6 | `median` (WPGMC) |
| 7 | `centroid` (UPGMC) |
| 8 | `ward.D2` |

For `ward.D2`, the algorithm runs on the **squared** input distances and reports the **square root** of each merge distance as its height. `nn_chain_impl()` does this by calling `d.square()` at its start and taking `std::sqrt()` of each height. `generic_impl()` must do the same. All other methods run on the distances as given; `hclust()` doesn't square them for centroid or median linkage either.

**`Tri`** (defined in the file) holds the distances between `N` active clusters as the lower triangle of a `dist` object:

- `get(i, j)` and `set(i, j, v)` read and write the distance between slots `i` and `j` (in either order).
- `square()` squares every distance.
- `compact(act)` drops every slot not in the increasing vector `act` and renumbers the remaining ones 0, 1, ... in the same order.

You may use `compact()` or not, as you like.

**`Tree`** (in `linkage.h`) has these fields:

- `n`: the number of leaves.
- `merge_a`, `merge_b` and `height`: one entry per merge.
- `order`: the leaf order, filled by `hclust_form()`.

Each `merge_a`/`merge_b` entry is either `-(leaf + 1)` for a leaf, with leaves numbered from 0, so leaf 0 is -1, or the 1-based number of an earlier merge.

**`init`** is either `nullptr` (every leaf has size 1) or the initial size of each leaf, as `hclust()`'s `members` argument gives them (positive doubles). Sizes add up when clusters merge. They enter the Lance-Williams formulas for methods 1, 4, 7 and 8.

**`threads`** is the number of threads you may use. Using just one is fine. If you use more, results must not depend on the number of threads.

Distances reaching `generic_impl()` are finite. Missing distances are rejected before it is called. `n >= 2` is guaranteed: `cluster_tri()` handles `n < 2`.

## 2. `lance_williams(method, dik, djk, d12, mi, mj, mk)`

This returns the distance from a cluster `k` to the cluster formed by merging `i` and `j`. The inputs are:

- `dik = d(i, k)`, `djk = d(j, k)` and `d12 = d(i, j)`;
- the sizes `mi`, `mj` and `mk`.

Use the Lance and Williams (1967) recurrence, with each method's coefficients as in Müllner (2011), table 1:

| Method | Distance to the merged cluster |
|---|---|
| single (2) | `min(dik, djk)` |
| complete (3) | `max(dik, djk)` |
| average (4) | `(mi*dik + mj*djk) / (mi + mj)` |
| mcquitty (5) | `(dik + djk) / 2` |
| median (6) | `(dik + djk) / 2 - d12 / 4` |
| centroid (7) | `(mi*dik + mj*djk) / (mi + mj) - mi*mj*d12 / (mi + mj)^2` |
| ward.D (1) and ward.D2 (8), on the values in the triangle (squared for 8) | `((mi + mk)*dik + (mj + mk)*djk - mk*d12) / (mi + mj + mk)` |

You don't need to reproduce `hclust()`'s heights bit for bit; agreement to a relative 1e-12 is the requirement. Write the arithmetic as you see fit. The existing `HEATMAPR_PROD(a, b)` macro (in `metric.h`) rounds a product on its own instead of letting the compiler fuse it into a multiply-add. Use it or not; results must not change with the processor.

## 3. `generic_impl(Tri& d, int n, int method, const double* init, int threads)`

Hierarchical clustering for **every** method code 1-8. It is used for:

- centroid and median linkage, which aren't "reducible": a merged cluster can be closer to another cluster than the merge height, so heights can decrease (inversions);
- every method when the environment variable `HEATMAPR_LINKAGE=list` is set, which the tests use to compare it with the other algorithms.

At every step, it must merge the **closest pair** of active clusters exactly, updating the distances to the merged cluster with `lance_williams()`. Return the merges in the **order they are made**, with their heights. Heights can decrease for methods 6 and 7; don't sort them.

**Ties.** Several pairs can be at the same smallest distance. Order the active clusters by their smallest leaf index; that is the order of their slots if the merged cluster takes the lower of the two slots. Among the tied pairs `(A, B)`, with `A` before `B` in that order, merge the one with the earliest `A`, then the earliest `B`. This rule is believed to match `stats::hclust()` in most tied cases, but matching `hclust()` on ties is not required.

**Algorithm.** Written from Müllner (2011), *Modern hierarchical, agglomerative clustering algorithms*, arXiv:1109.2378, section 3.1, the "generic" algorithm. It keeps, for each cluster, a candidate nearest neighbour among the clusters after it, in a priority queue with lazily corrected entries. This finds the exact closest pair at every step for every linkage, including inversions. You may fetch and read that paper; its pseudocode is fine to follow. Any other algorithm with the same exactness and similar speed is acceptable.

**Results required:**

- For random continuous data (distinct distances), the tree after `hclust_form()` must equal `stats::hclust(d, method)`'s exactly:
  - `merge` and `order` must be identical;
  - `height` must agree to a relative 1e-12;
  - this holds for every method and with `members`.
- For tied data (e.g. integer coordinates), the tree must be a valid tree with `hclust()`'s multiset of heights (to 1e-12). Report how often it is identical to `hclust()`'s.
- `nn_chain_impl()` (methods 1, 3, 4, 5, 8) must still give `hclust()`'s trees with your `lance_williams()`. The tests check this.

**Speed.** The current timings on this machine are:

- `heat_map_clust(x, method = "centroid", max = Inf)` for `x <- matrix(rnorm(5000 * 20), 5000)` takes about 1.9 s in total, most of it the distances;
- the same call with `method = "average"` (nearest-neighbour chain) takes 1.7 s.

The target is centroid and median linkage within 1.5 times the average-linkage time. 10,000 leaves must work (the triangle is 400 MB).

## 4. `hclust_form(Tree& tree, bool ordered)`

This puts a tree into the form `stats::hclust()` returns. These are the observable conventions of `hclust()`'s output, which you can confirm by running `hclust()`:

1. **Merge order.**
   - If `ordered` is true, the merges stay in the order they were made.
   - If `ordered` is false, which is the chain's output, re-sort the merges into increasing height, keeping the original order between equal heights (a stable sort). Every merge must still come after the merges it joins. If rounding has left a child higher than its parent, sort on each merge's height raised to the largest key among its children.
   - Renumber the positive (merge) references to match the new order.
2. **Each merge row:**
   - two leaves: the smaller leaf number first, e.g. `-2, -5`;
   - a leaf and a merge: the leaf first;
   - two merges: the earlier merge first.
3. **`order`** is the 1-based leaves in the order of a depth-first walk from the last merge, the first entry of each row before the second. For example, for `hclust()`'s merge matrix `m`:

   ```r
   ord <- function(z) if (z < 0) -z else c(ord(m[z, 1]), ord(m[z, 2]))
   identical(as.integer(ord(nrow(m))), h$order)   # TRUE for hclust()'s own trees
   ```

   For trees with `n = 1` (no merges), `order` is `1`.
4. Heights stay attached to their merges.

Avoid recursion depth problems: trees can have 10^6 leaves and be a single chain.

## Tests to run

- `compiled`
- `heat_map_clust`
- `dist-order`
- `large`
- `threads`
- `branch-support`
- `groups`
- `spectral-autocut`
- `compiled-distances`: its clustering tests, from `"hclust()'s algorithm, compiled, gives hclust()'s trees exactly"` on. Its first tests use the numerics component.

Some tests in these files use distances or colours from the numerics component (Canberra, binary, Kendall, missing values, colours). Those fail with `clean-room stub` errors and are expected to.

You may change `expect_identical()` on the heights of centroid and median linkage in `test-compiled.R` to `expect_equal(..., tolerance = 1e-12)`. Keep `merge` and `order` identical.

`test-compiled-distances.R`'s test `"hclust()'s algorithm, compiled, gives hclust()'s trees exactly"` runs the generic algorithm (`HEATMAPR_LINKAGE=list`) on tied integer and binary distances and demands identical merges and heights. Under this spec, change it as follows:

- Keep exact merges and order for the continuous data.
- Compare heights there to 1e-12.
- For the tied data, require a valid tree with `hclust()`'s sorted heights (to 1e-12), and `hclust()`'s merges where your tie rule gives them.
- Report how many tied cases are identical.

Add `tests/testthat/test-cleanroom-linkage.R`. It should compare with `stats::hclust()` for every method on at least 200 random problems (n from 2 to 300, several dimensions, with and without `members`), with and without `HEATMAPR_LINKAGE=list`. It should also cover tied data, inversions (centroid and median), and a chain-shaped tree of 10^5 leaves through `hclust_form()`.

Entry points from R that exercise this code include:

- `.hclust_dist(d, method, members = )` and `.hclust_rows(...)` (internal);
- `heat_map_clust()`.

Look at `R/heat_map_clust.R` and the R entry points at the end of `src/linkage.cpp` to see how they are called.

## References

- Lance, G. N. and Williams, W. T. (1967). A general theory of classificatory sorting strategies: 1. Hierarchical systems. *The Computer Journal*, 9(4), 373-380.
- Müllner, D. (2011). Modern hierarchical, agglomerative clustering algorithms. arXiv:1109.2378.
- Murtagh, F. (1983). A survey of recent advances in hierarchical clustering algorithms. *The Computer Journal*, 26(4), 354-359.
