# One-way fixed-effects OLS with cluster-robust standard errors, plus plain OLS
# with HC1 robust errors. Companions to _helpers_twfe.R, for the recovered fines
# scripts (multas_RegsFE.R uses `feols(y ~ x | fe)`, fines_robustness_sutva.do
# uses `areg y x, a(fe) cluster(cl)` and `reg y x, robust`).
#
# Why hand-rolled: fixest/lfe are not in renv.lock (see _helpers_twfe.R for the
# toolchain story). One-way absorption is exact in a single demeaning pass, and
# multiple regressors are handled by ordinary matrix OLS on the demeaned data.
#
# Degrees of freedom: Stata's areg (and fixest) do NOT count the absorbed dummies
# against N-K when the absorbed factor is nested within the clusters (the usual
# a(MUNICIPIO) cluster(MUNICIPIO) case); they DO count them otherwise (e.g. a(UF)
# cluster(MUNICIPIO)). fe_ols() checks nesting and matches that convention, so its
# SEs are comparable to the legacy output rather than to _helpers_twfe.R's more
# conservative correction.

library(data.table)

# y: numeric; X: numeric matrix (or vector) of regressors, NO intercept -- the FE
# absorbs it; fe: absorbed factor; cluster: cluster ids. Returns per-regressor
# beta/se/t/p plus fit metadata.
fe_ols <- function(y, X, fe, cluster) {
  X <- as.matrix(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("x", seq_len(ncol(X)))
  keep <- stats::complete.cases(y, X) & !is.na(fe) & !is.na(cluster)
  y <- y[keep]; X <- X[keep, , drop = FALSE]
  fe <- fe[keep]; cluster <- cluster[keep]

  # demean via ave() so row order is preserved exactly
  dm <- function(v) as.numeric(v) - stats::ave(as.numeric(v), fe)

  yt <- dm(y)
  Xt <- apply(X, 2, dm)
  Xt <- matrix(Xt, ncol = ncol(X), dimnames = list(NULL, colnames(X)))

  XtX <- crossprod(Xt)
  beta <- as.numeric(solve(XtX, crossprod(Xt, yt)))
  e <- yt - Xt %*% beta

  # CR1 cluster-robust variance.
  cl <- as.character(cluster)
  S <- rowsum(Xt * as.numeric(e), cl)            # G x k score sums
  meat <- crossprod(S)
  N <- length(y); G <- nrow(S); k <- ncol(Xt)
  n_fe <- data.table::uniqueN(fe)
  nested <- all(rowSums(table(fe, cl) > 0) == 1L)
  K <- k + if (nested) 1L else n_fe
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))
  bread <- solve(XtX)
  V <- correction * bread %*% meat %*% bread
  se <- sqrt(diag(V))
  tstat <- beta / se

  list(
    coefs = data.table(
      term = colnames(Xt), beta = beta, se = se, t = tstat,
      p = 2 * stats::pt(abs(tstat), df = G - 1, lower.tail = FALSE)
    ),
    n_obs = N, n_fe = n_fe, n_clusters = G, fe_nested_in_cluster = nested
  )
}

# Plain OLS of y on X with an intercept, HC1 ("robust") standard errors --
# Stata's `reg y x, robust`.
ols_hc1 <- function(y, X) {
  X <- as.matrix(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("x", seq_len(ncol(X)))
  keep <- stats::complete.cases(y, X)
  y <- y[keep]; X <- cbind(`(Intercept)` = 1, X[keep, , drop = FALSE])

  XtX <- crossprod(X)
  beta <- as.numeric(solve(XtX, crossprod(X, y)))
  e <- as.numeric(y - X %*% beta)
  N <- nrow(X); k <- ncol(X)
  bread <- solve(XtX)
  meat <- crossprod(X * e)
  V <- (N / (N - k)) * bread %*% meat %*% bread
  se <- sqrt(diag(V))
  tstat <- beta / se
  data.table(
    term = colnames(X), beta = beta, se = se, t = tstat,
    p = 2 * stats::pt(abs(tstat), df = N - k, lower.tail = FALSE),
    n_obs = N
  )
}

# Self-test: recover known slopes under strong FE + verify against lm() on the
# dummy-expanded model.
feols_selftest <- function(verbose = TRUE) {
  set.seed(7)
  n_g <- 40L; n_per <- 50L
  fe <- rep(seq_len(n_g), each = n_per)
  cl <- fe                       # nested case
  x1 <- stats::rnorm(n_g * n_per)
  x2 <- stats::rnorm(n_g * n_per)
  a <- stats::rnorm(n_g, 0, 4)
  y <- 1.5 * x1 - 0.7 * x2 + a[fe] + stats::rnorm(n_g * n_per)

  fit <- fe_ols(y, cbind(x1 = x1, x2 = x2), fe, cl)
  ref <- stats::coef(stats::lm(y ~ x1 + x2 + factor(fe)))[c("x1", "x2")]
  ok <- max(abs(fit$coefs$beta - as.numeric(ref))) < 1e-8 &&
        abs(fit$coefs$beta[1] - 1.5) < 0.1

  r <- ols_hc1(y, cbind(x1 = x1, x2 = x2))
  ok <- ok && max(abs(r$beta - stats::coef(stats::lm(y ~ x1 + x2)))) < 1e-8

  if (verbose) {
    cat(sprintf("feols self-test: beta1 %.3f (true 1.5), matches lm() -> %s\n",
                fit$coefs$beta[1], if (ok) "PASS" else "FAIL"))
  }
  invisible(ok)
}
