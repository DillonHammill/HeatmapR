# SPEC-dip: the dip statistic in `src/dip.cpp`

Read `SPEC-common.md` first. Your component is the stub `dip::dip_sorted()` in `src/dip.cpp`. Everything else in the file is HeatmapR's own and stays as it is: the preparation of values (detection threshold, spreading of ties, dropping piled-up extremes), `group_dip()` and the R entry point `dip_cpp()`. Its callers are in that file and in `src/sparse.cpp` (`sparse_group_dips_cpp()` and `dense_group_dips_cpp()`).

## 1. Interface

```cpp
namespace dip {
double dip_sorted(const double* xs, std::size_t n);
}
```

- `xs[0..n)` is sorted in increasing order, finite and `n >= 1`.
- Ties (equal values) can occur. Callers usually spread them, but `dip_cpp(x, -Inf, FALSE, ...)` passes raw values.
- If `n == 0`, return `NaN`.

## 2. What it returns

`dip_sorted()` returns the **dip** of Hartigan and Hartigan (1985): the distance from the empirical distribution function `F_n` of the values to the closest unimodal distribution function,

```
D(F_n) = inf over unimodal G of sup_x |F_n(x) - G(x)|
```

A distribution function is unimodal if it is convex up to a point (its mode) and concave after it.

Report it on the scale that `diptest::dip()` uses, which is the scale of the tests:

- every sample of `n` values has a dip of at least `1 / (2n)`;
- `n == 1`, or all values equal, gives exactly `1 / (2n)`;
- for `n >= 2`, no dip exceeds `1/4` (`n == 1` gives `1/2`).

For example, `diptest::dip()` gives:

| Input | Dip |
|---|---|
| `5` | 0.5 |
| `c(2, 2, 2, 2)` | 0.125 |
| `c(1, 2)` | 0.25 |
| `1:3` | 1/6 |
| `c(1, 1, 2, 2)` | 0.25 |

Check the convention against `diptest::dip()`, which is installed, as a black box.

**Results required.**

- `dip_sorted(sort(x))` must equal `diptest::dip(x)` to a relative 1e-12 for every input:
  - random samples (normal, uniform, exponential, mixtures of two or three normals at various separations);
  - n from 1 to 10^5;
  - samples with ties (integer data, repeated values);
  - degenerate cases (`n` = 1, 2, 3; all equal; two distinct values).
- Test on at least several thousand random inputs.

## 3. How to compute it

Write it from Hartigan and Hartigan (1985), *The dip test of unimodality*, The Annals of Statistics 13(1), 70-84 (open on Project Euclid, doi:10.1214/aos/1176346577). The paper describes how the dip is computed: from the greatest convex minorant and least concave majorant of the empirical distribution function on a modal interval that is narrowed step by step. You may also work from the definition and your own analysis.

Do **not** read P. M. Hartigan's Algorithm AS 217 (Applied Statistics 34(3), 1985). It is a code listing. Do not read the `diptest` package's code or any translation of either (see `SPEC-common.md`).

Convex minorants and concave majorants of sorted points can be built with a monotone-stack (convex hull) pass in linear time. The whole computation should be `O(n)` or `O(n log n)` for sorted input. Use enough precision (e.g. `long double` for the differences between the two functions) to meet the 1e-12 requirement.

## 4. Speed

The current code takes about 11 ms per call for n = 50,000 (two normals, sorted). The target is at most 20 ms; faster is welcome. Bimodal flags call this once per feature and group, thousands of times.

## 5. Tests

Run these test files:

- `bimodal`, which has reference dips hard-coded from `diptest::dip()` and a snapshot;
- `markers-dot`;
- `single-cell`.

Some tests in them use colours, scaling or clustering from other components, which are stubs in your copy; failures with `clean-room stub` errors from other components are expected.

The null table of the dip (`.DIP_NULL` in `R/heat_map_dip.R`, from `data-raw/dip-null.R`) was simulated with the old implementation. If your dips equal `diptest::dip()` to 1e-12, the table stays valid. Confirm this by rerunning a few thousand of its simulations, e.g. n = 20, 50, 200, for the 0.99 quantile.

Add `tests/testthat/test-cleanroom-dip.R` comparing with `diptest::dip()`:

- Use `skip_if_not_installed("diptest")`; `diptest` is not a dependency of HeatmapR.
- Also hard-code a dozen reference values from `diptest::dip()`, so that the test still means something where `diptest` isn't installed.

## References

- Hartigan, J. A. and Hartigan, P. M. (1985). The dip test of unimodality. *The Annals of Statistics*, 13(1), 70-84.
