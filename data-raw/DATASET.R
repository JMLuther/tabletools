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


# usethis::use_data(cfs_base10yr, cfs_full10yr, 
#                   cfs_base30yr, cfs_full30yr, 
#                   internal = TRUE,
#                   overwrite = TRUE)

## ├ SDI Zip code files ----

library(data.table)
df_sdi <-
  tibble(fp = list.files(here::here("data-raw/SDI_zcta/"), full.names = T),
         fn = list.files(here::here("data-raw/SDI_zcta/"), full.names = F)) |> 
  mutate(year = gsub(".*-[0-9]+-([0-9]+)-.*.csv", "\\1", fn)) |> 
  mutate(data = map(fp, ~fread(.x, select = c("ZCTA5_FIPS", "SDI_score" )) )) |> 
  unnest(data) |> 
  group_by(year) |> 
  mutate(sdi_decile = ntile(SDI_score, 10))  |> 
  select(year, ZCTA5_FIPS, SDI_score, sdi_decile)

## ├ SAVE internal objects ----
usethis::use_data(cfs_base10yr, cfs_full10yr, 
                  cfs_base30yr, cfs_full30yr, 
                  df_sdi,
                  internal = TRUE,
                  overwrite = TRUE)
