library(sf) # for `st_cast`

# Save internal data:
# Save Vietnam admin1 administrative boundarie map from 2004 to 2007, use in the
# function hist_gadm to be able to recreate the merge event of `Ha Tay` and
# `Ha Noi` in 2008.
vn_a1_0407 <- readRDS("data-raw/gadm_vn_0407.rds")
vn_a1_0407 <-  sf::st_cast(vn_a1_0407, "MULTIPOLYGON")

# Save data and empty environment ----------------------------------------------

devtools::use_data(vn_a1_0407,
                   internal = TRUE, overwrite = TRUE)

rm(list = ls())

