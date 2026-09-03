# Small utility helpers used by the CAR build scripts.
#
# Faithful ports of the two utilities the legacy producer pulled in via
# `source("https://raw.githubusercontent.com/Thiago-Alckmin/.../helper_functions_simple.R")`.
# Vendored here so the pipeline has no runtime network dependency. Originals in
# `legacy_repo/dropbox_producers/create_muni_year_intersections/helper_functions_simple.R`.

suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
})

# print a banner-wrapped status message
message_with_lines <- function(message_text) {
  message("---------------------------------------------------")
  message(message_text)
  message("---------------------------------------------------")
}

# rename a set of columns of a data.table by position-matched vectors.
# Returns the original table (with a warning) if a target name already exists
# among the columns that are not being renamed. Faithful to the legacy behaviour.
rename_columns <- function(datatable, current_names, new_names) {
  all_names <- names(datatable)
  do_not_change <- all_names[!(all_names %in% current_names)]

  if (sum(new_names %in% do_not_change) == 1) {
    variables_that_already_exist <- new_names[new_names %in% do_not_change] %>%
      glue::glue_collapse(sep = ", ")
    warning(paste0(
      "At least one element of new_names already exists as a variable which will ",
      "not be renamed. The function returns the original data-table instead. ",
      "The variables in question are: ", variables_that_already_exist
    ))
    return(datatable)
  }

  data_do_not_change <- datatable %>% copy() %>% .[, ..do_not_change]
  data_change <- datatable %>% copy() %>% .[, ..current_names]
  names(data_change) <- new_names
  cbind(data_do_not_change, data_change)
}
