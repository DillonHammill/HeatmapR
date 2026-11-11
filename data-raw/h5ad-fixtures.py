# Small AnnData files for the examples and tests of heat_map_read_h5ad(),
# written by the anndata package (python3 data-raw/h5ad-fixtures.py, from the
# package root): inst/extdata/example.h5ad and tests/testthat/fixtures.
# The values are drawn with a fixed seed; the tests compare what HeatmapR
# reads with these same values, saved beside them as CSV.
import numpy as np
import pandas as pd
import scipy.sparse as sp
import anndata as ad
import h5py

rng = np.random.default_rng(2026)
n_cells, n_genes = 60, 30
counts = rng.poisson(rng.gamma(0.6, 2.0, size=(1, n_genes)), size=(n_cells, n_genes))
counts[:20, :5] += rng.poisson(8, size=(20, 5))
logn = np.log1p(counts / np.maximum(counts.sum(1, keepdims=True), 1) * 1e4)
cells = [f"cell{i + 1}" for i in range(n_cells)]
genes = [f"GENE{j + 1}" for j in range(n_genes)]
cell_type = pd.Categorical(
    np.repeat(["T", "B", "NK"], 20), categories=["T", "B", "NK"]
)
sample = pd.Categorical(np.tile(["s1", "s2", "s3", "s4"], 15))
sample[3] = np.nan
obs = pd.DataFrame(
    {
        "cell_type": cell_type,
        "sample": sample,
        "n_counts": counts.sum(1).astype(float),
        "is_doublet": rng.random(n_cells) < 0.1,
        "barcode": [f"AC{i:04d}" for i in range(n_cells)],
        "n_genes": pd.array(rng.integers(5, 30, n_cells), dtype="Int64"),
    },
    index=cells,
)
obs.loc["cell2", "n_genes"] = pd.NA
var = pd.DataFrame(
    {
        "gene_ids": [f"ENSG{j:011d}" for j in range(n_genes)],
        "highly_variable": rng.random(n_genes) < 0.5,
    },
    index=genes,
)

# CSR X (log-normalised), counts as a layer, raw with the same genes
a = ad.AnnData(
    X=sp.csr_matrix(logn.astype(np.float32)),
    obs=obs,
    var=var,
    layers={"counts": sp.csr_matrix(counts.astype(np.int32))},
)
a.raw = a.copy()
a.write_h5ad("inst/extdata/example.h5ad")

# Dense X, a CSC layer and a CSR layer whose indices aren't sorted; strings as
# plain string arrays, as anndata 0.8 to 0.11 write them
shuffled = sp.csr_matrix(counts.astype(np.float64))
for r in range(n_cells):
    lo, hi = shuffled.indptr[r], shuffled.indptr[r + 1]
    order = rng.permutation(hi - lo) + lo
    shuffled.indices[lo:hi] = shuffled.indices[order]
    shuffled.data[lo:hi] = shuffled.data[order]
shuffled.has_sorted_indices = False
obs_plain = obs.copy()
obs_plain.index = pd.Index(cells, dtype=object)
obs_plain["barcode"] = obs_plain["barcode"].astype(object)
var_plain = var.copy()
var_plain.index = pd.Index(genes, dtype=object)
var_plain["gene_ids"] = var_plain["gene_ids"].astype(object)
b = ad.AnnData(
    X=logn,
    obs=obs_plain,
    var=var_plain,
    layers={"counts": sp.csc_matrix(counts.astype(np.float64)),
            "shuffled": shuffled},
)
b.write_h5ad("tests/testthat/fixtures/dense.h5ad")

# The layout of anndata before 0.8: categories stored in obs/__categories
# and the codes as integers, indices in an "_index" column
with h5py.File("tests/testthat/fixtures/legacy.h5ad", "w") as f:
    f.create_dataset("X", data=logn.astype(np.float32))
    for name, df, index in (("obs", obs, cells), ("var", var, genes)):
        g = f.create_group(name)
        g.attrs["_index"] = "_index"
        g.create_dataset("_index", data=np.array(index, dtype="S"))
        cats = g.create_group("__categories")
        order = []
        for col in df.columns:
            v = df[col]
            if isinstance(v.dtype, pd.CategoricalDtype):
                cats.create_dataset(col, data=np.array(v.cat.categories, dtype="S"))
                g.create_dataset(col, data=v.cat.codes.to_numpy().astype(np.int8))
                g[col].attrs["categories"] = cats[col].ref
            elif v.dtype == object:
                g.create_dataset(col, data=np.array(v, dtype="S"))
            elif isinstance(v.dtype, pd.Int64Dtype):
                continue
            else:
                g.create_dataset(col, data=v.to_numpy())
            order.append(col)
        g.attrs["column-order"] = np.array(order, dtype="S")

# The values to compare with
pd.DataFrame(logn, index=cells, columns=genes).to_csv("tests/testthat/fixtures/logn.csv")
pd.DataFrame(counts, index=cells, columns=genes).to_csv("tests/testthat/fixtures/counts.csv")
