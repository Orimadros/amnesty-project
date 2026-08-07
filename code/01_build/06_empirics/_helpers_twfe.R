# Two-way fixed-effects estimator with cluster-robust standard errors.
#
# Why hand-rolled: `lfe`/`fixest` are not in renv.lock, and `lfe` cannot be added
# via the CLAUDE.md package dance on this host -- it needs a Fortran toolchain
# (/opt/gfortran) that is absent, so `install.packages('lfe')` fails at link time.
# Rather than extend the reproducibility contract or install a system toolchain,
# this implements the estimator with base R + data.table, both already pinned.
#
# Model:  y_it = beta * D_it + alpha_i + gamma_t + eps_it
#
# Identification is by the Frisch-Waugh-Lovell theorem: partial the two fixed
# effects out of both y and D, then regress the residuals. The FEs are removed by
# alternating projections (demean by unit, demean by time, repeat), which converges
# to the exact two-way within transformation. For a balanced panel this converges
# immediately; the loop makes it correct for unbalanced panels too.
#
# Standard errors are cluster-robust (CR1) at the level supplied, which is what the
# paper uses (state). The finite-sample correction counts the fixed effects against
# the degrees of freedom.

library(data.table)

# Alternating-projections two-way demeaning. Returns the residualised vector.
demean_twoway <- function(x, unit, time, tol = 1e-10, max_iter = 200L) {
  d <- data.table(x = as.numeric(x), unit = unit, time = time)
  for (i in seq_len(max_iter)) {
    before <- d$x
    d[, x := x - mean(x), by = unit]
    d[, x := x - mean(x), by = time]
    if (max(abs(d$x - before)) < tol) break
  }
  if (i == max_iter) warning("demean_twoway hit max_iter without converging")
  d$x
}

# Two-way FE regression of y on a single regressor D, clustered by `cluster`.
twfe <- function(y, D, unit, time, cluster) {
  keep <- !is.na(y) & !is.na(D)
  y <- y[keep]; D <- D[keep]
  unit <- unit[keep]; time <- time[keep]; cluster <- cluster[keep]

  yt <- demean_twoway(y, unit, time)
  Dt <- demean_twoway(D, unit, time)

  denom <- sum(Dt * Dt)
  if (denom <= 0) stop("no residual variation in the regressor after demeaning")
  beta <- sum(Dt * yt) / denom

  e <- yt - beta * Dt

  # CR1 cluster-robust variance for a single regressor.
  s <- data.table(score = Dt * e, cl = cluster)[, .(g = sum(score)), by = cl]
  meat <- sum(s$g^2)

  N <- length(y)
  G <- nrow(s)
  K <- 1L + data.table::uniqueN(unit) + data.table::uniqueN(time) - 1L
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))

  se <- sqrt(correction * meat / denom^2)
  tstat <- beta / se

  list(
    beta = beta, se = se, t = tstat,
    p = 2 * stats::pt(abs(tstat), df = G - 1, lower.tail = FALSE),
    n_obs = N, n_units = data.table::uniqueN(unit), n_clusters = G
  )
}

# Multi-regressor variant: y on a MATRIX X of regressors under the same two-way
# FE + CR1 clustering. Needed for event studies (one dummy per relative year).
# Same conventions as twfe(); K counts the FEs against the dof as above.
twfe_k <- function(y, X, unit, time, cluster) {
  X <- as.matrix(X)
  if (is.null(colnames(X))) colnames(X) <- paste0("x", seq_len(ncol(X)))
  keep <- !is.na(y) & stats::complete.cases(X)
  y <- y[keep]; X <- X[keep, , drop = FALSE]
  unit <- unit[keep]; time <- time[keep]; cluster <- cluster[keep]

  yt <- demean_twoway(y, unit, time)
  Xt <- apply(X, 2, demean_twoway, unit = unit, time = time)
  Xt <- matrix(Xt, ncol = ncol(X), dimnames = list(NULL, colnames(X)))

  XtX <- crossprod(Xt)
  beta <- as.numeric(solve(XtX, crossprod(Xt, yt)))
  e <- as.numeric(yt - Xt %*% beta)

  cl <- as.character(cluster)
  S <- rowsum(Xt * e, cl)
  meat <- crossprod(S)

  N <- length(y)
  G <- nrow(S)
  K <- ncol(Xt) + data.table::uniqueN(unit) + data.table::uniqueN(time) - 1L
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
    n_obs = N, n_units = data.table::uniqueN(unit), n_clusters = G
  )
}

# Self-test: recover a known beta from synthetic data with strong unit and time
# effects. Run this before trusting the estimator on real data.
twfe_selftest <- function(verbose = TRUE) {
  set.seed(42)
  n_u <- 300L; n_t <- 10L
  g <- CJ(unit = seq_len(n_u), time = seq_len(n_t))
  au <- stats::rnorm(n_u, 0, 5)
  gt <- stats::rnorm(n_t, 0, 3)
  treated <- rep(c(TRUE, FALSE), length.out = n_u)
  g[, D := as.integer(treated[unit] & time >= 6L)]
  true_beta <- 2.5
  g[, y := true_beta * D + au[unit] + gt[time] + stats::rnorm(.N, 0, 1)]
  g[, cl := (unit %% 12L) + 1L]

  fit <- twfe(g$y, g$D, g$unit, g$time, g$cl)
  ok <- abs(fit$beta - true_beta) < 0.05
  # twfe_k must agree exactly with twfe on the single-regressor case
  fk <- twfe_k(g$y, cbind(D = g$D), g$unit, g$time, g$cl)
  ok <- ok && abs(fk$coefs$beta - fit$beta) < 1e-8 && abs(fk$coefs$se - fit$se) < 1e-8
  if (verbose) {
    cat(sprintf(
      "twfe self-test: true beta %.3f | estimated %.3f (se %.3f) -> %s\n",
      true_beta, fit$beta, fit$se, if (ok) "PASS" else "FAIL"
    ))
  }
  invisible(ok)
}
