## code to prepare `DATASET` dataset goes here

# usethis::use_data(overwrite = FALSE)

# Datasets ----
## ├ OGTT data ----
library(tidyverse)
gtt <- readRDS(here::here("data-raw/OGTT_merged_2017-12-23.rds"))
ogtt_wide <- 
  gtt |> 
  mutate(id = row_number()) |> 
  filter(!is.na(insulin_auc),
         cohort != "PPH") |> 
  select(id, age, gender.f, race.f, height_cm, weight_kg, bmi, hdl_bl, ldl_bl, trigs_bl,
         starts_with("ogtt_glucose"), starts_with("ogtt_insulin")) 
ogtt_nested <- 
  gtt |> 
  mutate(id = row_number()) |> 
  filter(!is.na(insulin_auc),
         cohort != "PPH") |> 
  select(id, age, gender.f, race.f, height_cm, weight_kg, bmi, hdl_bl, ldl_bl, trigs_bl,
         ogtt_df) 


usethis::use_data(ogtt_wide)
usethis::use_data(ogtt_nested)

# Internal Data ----
# Internal data objects are all stored within a single .rda file (R/sysdata.rda) 
# Internal data objects can be made available for use with `devtools::load_all()` 

# original data files are stored in `data-raw` folder and are not exported in the package

## ├ PREVENT coefficient tables ----
cfs_base10yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_base10yr")
cfs_full10yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_full10yr")
cfs_base30yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_base30yr")
cfs_full30yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_full30yr")


usethis::use_data(cfs_base10yr, cfs_full10yr, 
                  cfs_base30yr, cfs_full30yr, 
                  internal = TRUE,
                  overwrite = TRUE)

## ├ SDI Zip code files ----
sdi_files <- list.files(here::here("data-raw/SDI_zcta/"), full.names = T)
readr::read_csv(sdi_files[1], n_max = 250) 


