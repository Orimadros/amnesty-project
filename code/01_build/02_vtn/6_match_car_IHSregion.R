library(sf)
library(dplyr)
library(readr)
library(here)

sf_use_s2(TRUE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

load(here("data", "clean", "IHS_regions_divison.Rdata"))
regions_ihs <- regions_2015 %>%
  select(state, region_id, region_name) %>%
  st_make_valid()

car_layers <- list(
  eligible = here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
  ineligible = here("data", "intermediate", "car", "car_ineligible_cleaned.shp"),
  legal = here("data", "intermediate", "car", "already_treated.shp")
)

missing <- car_layers[!file.exists(unlist(car_layers))]
if (length(missing) > 0) {
  stop(
    paste(
      "Missing CAR layer(s) required by VTN step 6:",
      paste0(" - ", unname(missing), collapse = "\n"),
      "Run `make 01_car` first.",
      sep = "\n"
    )
  )
}

out_dir <- here("data", "intermediate", "car", "ihs_breakdown")
ensure_dir(out_dir)

region_lists <- list()

for (cat in names(car_layers)) {
  message("Processing CAR category: ", cat)
  car_layer <- st_read(car_layers[[cat]], quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(st_crs(regions_ihs))

  car_tbl <- st_join(car_layer, regions_ihs, left = TRUE)

  obj_name <- paste0(cat, "_car")
  assign(obj_name, car_tbl)
  save(
    list = obj_name,
    file = file.path(out_dir, paste0(obj_name, "_IHS.Rdata"))
  )

  region_tbl <- car_tbl %>%
    st_drop_geometry() %>%
    filter(!is.na(region_id)) %>%
    distinct(state, region_id, region_name) %>%
    arrange(state, region_id)

  write_csv(
    region_tbl,
    file.path(out_dir, paste0(cat, "_IHS.csv"))
  )

  region_lists[[cat]] <- region_tbl
}

all_car_regions <- bind_rows(region_lists) %>%
  distinct(state, region_id, region_name) %>%
  arrange(state, region_id)

save(
  all_car_regions,
  file = here("data", "intermediate", "car", "all_car_regions.Rdata")
)

write_csv(
  all_car_regions,
  file.path(out_dir, "all_car_regions.csv")
)

message("Done. Outputs written to: ", out_dir)
