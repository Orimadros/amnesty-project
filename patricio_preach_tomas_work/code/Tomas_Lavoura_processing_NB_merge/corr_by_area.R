###############################################################################
#  corr_by_area.R
#  --------------
#  • Depends on:  load_parcels.R  (run that first so eligible/legal/ineligible
#                 and helper funcs are in the workspace)
#  • Exposes one public function:
#        corr_by_area(df, state = NULL, region_id = NULL,
#                     region_name = NULL, min_pairs = 10)
#  • Optional utility:
#        corr_overall(df, ...)  # weighted average across all years
#  • Prints nothing by default – you call the functions and handle the result.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(fs)      
  library(readr)   
})

# ----- where the earlier join wrote its outputs -----------------------------
out_root <- "data/output/parcels_NB_Lavoura"

files <- list(
  eligible   = path(out_root, "eligible_parcels_all",   "eligible_parcel_nb_lavoura_wide.rds"),
  legal     = path(out_root, "legal_parcels_all",     "legal_parcel_nb_lavoura_wide.rds"),
  ineligible = path(out_root, "ineligible_parcels_all", "ineligible_parcel_nb_lavoura_wide.rds")
)

# ----- read each file and expose as variables -------------------------------
parcels <- imap(files, ~ {
  if (!file_exists(.x)) stop("File not found: ", .x, call. = FALSE)
  read_rds(.x)
})

list2env(parcels, .GlobalEnv)   # now have eligible / legal / ineligible

# ----- column-helper vectors -------------------------------------------------
nb_cols  <- function(df) grep("^preco_",           names(df), value = TRUE)
lav_cols <- function(df) grep("^price_\\d{4}_lav", names(df), value = TRUE)
id_cols  <- function(df) setdiff(names(df), c(nb_cols(df), lav_cols(df)))


# ---------------------------------------------------------------------------
#  corr_by_area() – Pearson correlation NB ↔ Lavoura by YEAR for a chosen area
# ---------------------------------------------------------------------------
corr_by_area <- function(df,
                         state = NULL,          # e.g. "RO" or c("RO","PA")
                         region_id = NULL,      # numeric ID(s)
                         region_name = NULL,    # string(s) like "Cacoal"
                         min_pairs = 10) {      # skip if < this many parcels

  # ---- filter to chosen area ----------------------------------------------
  if (!is.null(state))       df <- df %>% filter(state %in% state)
  if (!is.null(region_id))   df <- df %>% filter(region_id %in% region_id)
  if (!is.null(region_name)) df <- df %>% filter(region_name %in% region_name)

  # ---- grab NB & Lavoura column names -------------------------------------
  nb_all  <- nb_cols(df)                     # helper from loader
  lav_all <- lav_cols(df)

  yrs <- intersect(str_extract(nb_all,  "\\d{4}"),
                   str_extract(lav_all, "\\d{4}"))    # 2002-2017

  # ---- loop over years -----------------------------------------------------
  map_dfr(yrs, function(yr) {
    nb_cols_yr <- grep(paste0("_", yr, "$"), nb_all, value = TRUE)
    lav_col    <- paste0("price_", yr, "_lavoura")

    nb_mean <- rowMeans(df[, nb_cols_yr], na.rm = TRUE)  # parcel-level mean
    lav_vec <- df[[lav_col]]

    mask <- !is.na(nb_mean) & !is.na(lav_vec)
    tibble(
      year     = as.integer(yr),
      n_pairs  = sum(mask),
      cor      = if (sum(mask) >= min_pairs)
                   cor(nb_mean[mask], lav_vec[mask]) else NA_real_
    )
  })
}

# ---------------------------------------------------------------------------
#  corr_overall() – one pooled correlation number (weighted by n_pairs)
# ---------------------------------------------------------------------------
corr_overall <- function(df, ...) {
  tbl  <- corr_by_area(df, ...)
  mask <- !is.na(tbl$cor)
  if (any(mask)) weighted.mean(tbl$cor[mask], w = tbl$n_pairs[mask]) else NA
}

# ---------------------------------------------------------------------------
#  EXAMPLE CALLS (uncomment to test)
# ---------------------------------------------------------------------------
# 1. Correlation by year for the state of RO, eligible parcels only
# corr_by_area(eligible, state = "RO")

# 2. Correlation for region ID 124 (Cacoal) across all three datasets
# list(eligible = eligible, legal = legal, ineligible = ineligible) |>
#   purrr::imap_dfr(~ corr_by_area(.x, region_id = 124) %>% mutate(dataset = .y))

# 3. Single pooled correlation for RO + PA in the ineligible set
# corr_overall(ineligible, state = c("RO", "PA"))
