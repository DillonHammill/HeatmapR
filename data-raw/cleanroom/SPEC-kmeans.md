# SPEC-kmeans: Hartigan-Wong k-means in `src/kmeans.cpp`

Read `SPEC-common.md` first. Your component is the stub `kmeans_hw_cpp()` in `src/kmeans.cpp`. The rest of the file stays as it is: `first_pc_cpp()`, `group_means_cpp()` and `distinct_rows_cpp()`. You may add helper functions, structs and SIMD kernels in an anonymous namespace above it.

## 1. Interface

```cpp
[[cpp11::register]]
cpp11::list kmeans_hw_cpp(cpp11::doubles_matrix<> x, cpp11::doubles_matrix<> centres,
                          int iter_max, int threads);
```

- **`x`** is `m x p`, column-major as R stores it, finite doubles. The rows are the points.
- **`centres`** is `k x p`: the starting centres, with distinct rows (the caller checks this).
- **`iter_max`** is the largest number of iterations (the caller passes 100).
- **`threads`** may be used for the parts that are naturally parallel, such as the first assignment of every point to its two closest centres. Results must not depend on it.

Return a named list:

| Name | Value |
|---|---|
| `cluster` | integer vector of length `m`: the 1-based cluster of each point; all `NA` when `ifault == 3` |
| `centers` | `k x p` double matrix: the mean of each final cluster's points |
| `iter` | integer: the number of iterations, as `stats::kmeans()` reports them: the passes of the optimal-transfer stage run until convergence, or `iter_max + 1` when it didn't converge. With k = 2, `kmeans()` reports 1 for a converged fit. With two clusters, a point's other cluster is always its second closest, so the quick-transfer stage alone reaches the optimum after the first pass. Check these by running `kmeans()`. |
| `ifault` | integer status code, below |

| `ifault` | Meaning |
|---|---|
| 0 | converged: a full optimal-transfer pass moved no point |
| 1 | a cluster became empty, or a starting centre was nobody's closest centre |
| 2 | did not converge in `iter_max` iterations; clusters and centres are still returned |
| 3 | `k < 2` or `k >= m`; `cluster` is all `NA` |
| 4 | optional: the quick-transfer stage was stopped by a safety limit on its steps; clusters and centres are still returned. Only use this if you add such a limit. |

When `ifault` is 0, 2 or 4, `centers` must be the exact means of the final clusters: each column summed over the cluster's points in row order, divided by the count.

The R caller, `.try_kmeans()` in `R/heat_map_clust.R`, treats `ifault` 1 and 3 as failure and uses the clusters otherwise.

## 2. The algorithm

Hartigan and Wong (1979), *Algorithm AS 136: A k-means clustering algorithm*, Applied Statistics 28(1), 100-108. Implement it from the **description** in the paper's text, sections 1-3. You may also use Hartigan (1975), *Clustering Algorithms* (Wiley), chapter 4. Do not read the Fortran listing printed with the paper, or any translation of it; see `SPEC-common.md`.

In outline, as the paper describes it:

1. **Start.**
   - Assign each point to its closest centre (`IC1`) and record its second closest (`IC2`).
   - Compute the centres as the means of the assigned points.
   - Every cluster starts in the "live set".
2. **Optimal-transfer stage.**
   - For each point `i` in turn, in its cluster `L1` of size `n1`, find the cluster `L2` that minimises `n2 d(i, L2)^2 / (n2 + 1)` among the clusters that could take it, using the live-set rules of the paper.
   - Move the point if that value is smaller than `n1 d(i, L1)^2 / (n1 - 1)`.
   - A cluster of one point never loses its point.
   - Update `IC1`/`IC2`, the sizes and both centres at once after each move.
   - A cluster stays live while it has been updated within the last pass.
3. **Quick-transfer stage.** For each point, consider only swapping it between `IC1` and `IC2`. Repeat until a full pass makes no transfer.
4. Alternate stages 2 and 3 until a full optimal-transfer pass moves no point, or `iter_max` is reached.

The paper specifies the order in which points are visited and the live-set bookkeeping; follow it. Two implementations that do so give the same clusters. Use squared Euclidean distances throughout.

## 3. Results required

`stats::kmeans(x, centres, iter.max = 100)` (algorithm `"Hartigan-Wong"`, the default) is your black-box oracle.

- **Well separated clusters** (e.g. Gaussian clusters 6 sd apart): `cluster` identical to `stats::kmeans()`'s and `centers` equal to a relative 1e-12, in every case.
- **General data.** Use the case generator in `tests/testthat/test-kmeans-summarise.R`, `kmeans_cases()`: Gaussian, integer, large-scale and mixture data, n up to 2000, p up to 30, k up to 40.
  - Report the fraction of cases where `cluster` is identical to `stats::kmeans()`'s. Aim for (nearly) all.
  - Where it differs, the result must still be a Hartigan local optimum (no single point can be moved to another cluster to lower the total within-cluster sum of squares). Its total within-cluster sum of squares must not be worse than `kmeans()`'s by more than a relative 1e-9, except in rare cases that you report.
- **Failures.** When `stats::kmeans()` errors (e.g. an empty cluster), return `ifault` 1 (or 3 for an invalid `k`).
- **Processor.** Results must not depend on the processor's instruction set or on `threads`.

## 4. Speed

On this machine, `stats::kmeans()` on 100,000 x 20 points with k = 10 takes about 0.3 s. The target is no slower than `stats::kmeans()` on one thread for problems of that size, and faster with several threads.

## 5. Tests

Run these test files:

- `kmeans-summarise`
- `review-fixes`
- `single-cell`
- `describe`
- `deferred-fixes`
- `colours-redraw`

Many tests that call `heat_map(..., tree_kmeans_y = )` or `.kmeans_groups()` also go through hierarchical clustering or colours, which are stubs in your copy; failures with `clean-room stub` errors from other components are expected.

The tests that matter for you are those that call `kmeans_hw_cpp()` or `.try_kmeans()` directly, especially:

- `"compiled k-means gives stats::kmeans()'s clusters exactly"`;
- `"the SIMD distances of compiled k-means are those of its scalar code"`, which checks that results don't change with `kernel_settings_cpp(..., "scalar")`.

**Changing tests.** If `"compiled k-means gives stats::kmeans()'s clusters exactly"` can't pass in every case under the "results required" rules above:

- change it to check the rules: identical clusters where the clusters are well separated, a Hartigan local optimum and no worse total within-cluster sum of squares otherwise;
- keep the check on `iter` only where the clusters are identical;
- explain the change in your report.

Add `tests/testthat/test-cleanroom-kmeans.R` with your own comparisons against `stats::kmeans()`, including:

- integer data with ties;
- k = 2;
- a cluster of one point;
- `iter_max` reached;
- an empty cluster;
- `k >= m`.

## References

- Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: a k-means clustering algorithm. *Applied Statistics*, 28(1), 100-108. Read the text only, not the code listing.
- Hartigan, J. A. (1975). *Clustering Algorithms*. Wiley.
