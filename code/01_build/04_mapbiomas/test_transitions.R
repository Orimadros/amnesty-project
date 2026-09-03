# test_transitions.R
# ---------------------------------------------------------------------------
# Unit test for the deforestation/reforestation transition kernel.
#
# WHY THIS TEST IS SPECIAL: it exercises ONLY the C++ string logic in
# aux/deforestation_rules.cpp. It never loads sf, terra, or any GDAL/GEOS/PROJ
# code, so its result is identical on host R and inside the container. That
# makes it safe to run anywhere for a fast (~seconds) correctness check on the
# highest-risk piece of the MapBiomas pipeline.
#
# Each pixel is one row; each column is a year's land-cover label. We feed in
# hand-built rows whose correct output we worked out by hand from the rules,
# then assert the kernel reproduces them.
#
# Run (from project root):
#   Rscript code/01_build/04_mapbiomas/test_transitions.R
# Exits 0 if all cases pass, 1 otherwise.
# ---------------------------------------------------------------------------

suppressMessages(library(Rcpp))

# Locate the C++ helper relative to project root (robust to how it's invoked).
cpp_rel  <- file.path("code", "01_build", "04_mapbiomas", "aux", "deforestation_rules.cpp")
cpp_path <- if (file.exists(cpp_rel)) cpp_rel else file.path(getwd(), cpp_rel)
if (!file.exists(cpp_path)) {
  stop("Cannot find deforestation_rules.cpp — run this from the project root.")
}
sourceCpp(cpp_path)

# --- test harness ----------------------------------------------------------
run_case <- function(name, input_vec, expected_vec) {
  m   <- matrix(input_vec, nrow = 1)
  out <- as.character(apply_deforestation_rules_cpp(m)[1, ])
  ok  <- identical(out, expected_vec)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", name))
  if (!ok) {
    cat("     input:    ", paste(input_vec,    collapse = ", "), "\n")
    cat("     expected: ", paste(expected_vec, collapse = ", "), "\n")
    cat("     got:      ", paste(out,          collapse = ", "), "\n")
  }
  ok
}

# --- cases (hand-verified against the rules) -------------------------------
results <- c(
  # Once forest gives way to human use, the pixel is deforested and stays so.
  run_case(
    "deforestation persists after forest -> human",
    c("forest", "forest", "human", "human", "human"),
    c("forest", "forest", "deforested", "deforested", "deforested")
  ),

  # A sustained return of forest after deforestation is flagged as reforested.
  run_case(
    "reforestation after sustained forest return",
    c("deforested", "deforested", "forest", "forest", "forest"),
    c("deforested", "deforested", "forest", "forest", "reforested")
  ),

  # Land that is always forest must never change.
  run_case(
    "stable forest is unchanged",
    c("forest", "forest", "forest", "forest", "forest"),
    c("forest", "forest", "forest", "forest", "forest")
  )
)

cat(sprintf("\n%d/%d cases passed.\n", sum(results), length(results)))
if (!all(results)) quit(status = 1)
