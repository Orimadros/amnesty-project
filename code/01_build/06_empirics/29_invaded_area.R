# Stage 29 (recovered code): the invaded-property-area regression,
# empirics_amazon_final.do:152-163 (the "Anticipating Another Amnesty" analysis):
#
#   g after2 = when_occupied >= 2009
#   g did2 = after2*treated
#   reghdfe area_do_imovel treated after2 did2 if variable == 2 &
#     (inelegible | never eligible) & sa == . & defo_rate_value_max < 85 &
#     when_occupied != 9090, cluster(uf) a(uf i.when_occupied)
#
# Run on stage 19's rebuilt panel (their April-2025 vintage; EMP-style
# faithful assembly) + temas areas. when_occupied = first year with rate >= 10
# over the panel years (did.dta's rule at 2_empirics.R:2193, >= threshold),
# 9090 sentinel when never. `treated` here is the inelegible dummy; reghdfe
# drops after2 (collinear with the absorbed i.when_occupied), as in Stata --
# the do-file relies on that; here after2 is simply not included as a
# regressor (its absorption is exact).

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_twfe.R"))
if (!twfe_selftest()) stop("twfe self-test failed")

dd <- here("data", "legacy_dropbox")
rp <- as.data.table(readRDS(file.path(dd, "output_full", "rebuilt_did_panel.rds")))
rp <- rp[group %in% c("inelegible", "never eligible")]

occ <- rp[rate >= 10, .(first_occ = min(year)), by = COD_IMO]
rp <- merge(rp, occ, by = "COD_IMO", all.x = TRUE)
rp[, when_occupied := fifelse(is.na(first_occ), 9090L, as.integer(first_occ))]

tem_f <- here("data", "input", "sicar", "microdata", "temas_ambientais.csv")
tem <- fread(tem_f, select = c("registro_car", "area_do_imovel"))
tem <- unique(tem, by = "registro_car")
rp <- merge(rp, tem, by.x = "COD_IMO", by.y = "registro_car")

s <- rp[sa == FALSE & !is.na(rate_max_pre) & rate_max_pre < 85 &
        when_occupied != 9090 & !is.na(area_do_imovel)]
s[, `:=`(treated = as.integer(group == "inelegible"),
         after2 = as.integer(when_occupied >= 2009))]
s[, did2 := treated * after2]
s[, uf := substr(COD_IMO, 1, 2)]
s <- s[grepl("^[A-Z]{2}$", uf)]

# absorb uf + i.when_occupied (two-way); regressors treated + did2 (after2 is
# collinear with the when_occupied FE and dropped, as reghdfe does)
f <- twfe_k(s$area_do_imovel, cbind(treated = s$treated, did2 = s$did2),
            s$uf, s$when_occupied, s$uf)

cat("\n===== INVADED AREA (do-file :163) on the rebuilt April panel =====\n")
print(as.data.frame(f$coefs), digits = 4)
cat("n_obs ", f$n_obs, " | clusters ", f$n_clusters, "\n", sep = "")
cat("(no printed coefficient to anchor -- fig:10 shows distributions; this\n",
    "regression is the do-file's companion test that post-2009 invaders claim\n",
    "different property sizes)\n", sep = "")

fwrite(f$coefs, here("data", "intermediate", "empirics", "invaded_area_reg.csv"))
