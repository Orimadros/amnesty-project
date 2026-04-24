# ── plot_muni_base.R ──────────────────────────────────────────
# PURPOSE   Quick base-graphics map coloured by the `rand_val`
# INPUT     data/clean/muni_division_2015_rand.Rdata
# OUTPUT    A plot in the VS-Code “Plots” pane (or a graphics window)

library(sf)

load("data/clean/muni_division_2015_rand.Rdata")   # -> muni_division_2015_rand

plot(
  muni_division_2015_rand["rand_val"],
  main = "Smoke-test: random colours per municipality"
)
