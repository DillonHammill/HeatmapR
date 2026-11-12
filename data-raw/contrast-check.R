# The moderated t-tests of heat_map_contrast() (.moderated_t()) against
# limma's lmFit() and eBayes() as a black box, on 300 random two-group
# problems (2 to 6 samples per group, 5 to 200 features). Run from the package
# root with HeatmapR and limma installed. Last run (limma 3.6x): the largest
# relative differences were 6e-15 in t, 5e-13 in p and 4e-14 in log fold
# change. With a feature of zero variance the prior differs: limma moves zero
# variances just above zero and uses them, heat_map_contrast() leaves them out.
suppressPackageStartupMessages(library(limma))
mt <- HeatmapR:::.moderated_t
worst <- c(t = 0, p = 0, logfc = 0)
set.seed(1)
cases <- 0
for(rep in 1:300) {
  n1 <- sample(2:6, 1); n2 <- sample(2:6, 1); G <- sample(c(5, 20, 200), 1)
  sd <- sqrt(1 / rchisq(G, df = sample(c(2, 5, 50), 1)) * runif(1, 0.1, 3))
  if(runif(1) < 0.2) sd <- rep(1, G)
  y <- matrix(rnorm((n1 + n2) * G, sd = rep(sd, each = n1 + n2)), n1 + n2)
  y[(n1 + 1):(n1 + n2), 1:min(3, G)] <- y[(n1 + 1):(n1 + n2), 1:min(3, G)] + 2
  test <- rep(c(FALSE, TRUE), c(n1, n2))
  r <- mt(y, test)
  design <- cbind(1, as.numeric(test))
  fit <- eBayes(lmFit(t(y), design))
  worst["t"] <- max(worst["t"], max(abs(r$t - fit$t[, 2]) / pmax(1, abs(fit$t[, 2]))))
  worst["p"] <- max(worst["p"], max(abs(r$p - fit$p.value[, 2]) / pmax(1e-300, fit$p.value[, 2])))
  worst["logfc"] <- max(worst["logfc"], max(abs(r$logfc - fit$coefficients[, 2])))
  cases <- cases + 1
}
print(cases); print(signif(worst, 3))
# NA values and a zero variance
y <- matrix(rnorm(8 * 30), 8); y[1, 3] <- NA; y[, 4] <- 1
test <- rep(c(FALSE, TRUE), 4)
r <- mt(y, test); fit <- suppressWarnings(eBayes(lmFit(t(y), cbind(1, as.numeric(test)))))
print(cbind(ours = r$p[1:6], limma = fit$p.value[1:6, 2]))
