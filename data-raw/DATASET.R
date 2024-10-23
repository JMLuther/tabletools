## code to prepare `DATASET` dataset goes here

# usethis::use_data(overwrite = FALSE)

# Datasets ----
## ├ Iohexol Data ----
# formerly entered in the examples for calculate_mgfr_2c()
# moved to data Oct 2024
dat_schwartz <-
  data.frame(
    time = c(10, 20, 30, 60, 120, 180, 240, 300, 360),
    iohexol_ug_ml = c(656.1168132,477.1163595,371.3542728,
                      223.1121251,111.1086272,61.88251616,
                      37.43137242,21.79250307,12.75996292)  )


## ├ Pottel data ----
# from Supplemental document
dat10 <- data.frame(
  id = c(10, 10, 10, 10, 10, 10, 10, 10),
  time = c(30, 60, 90, 120, 150, 180, 240, 300),
  iohexol = c(239.9117,217.3945,178.215,159.4682,
           143.1718,130.3613,113.6223,98.6295))
dat17 <- data.frame(
  id = c(17, 17, 17, 17, 17, 17, 17, 17),
  time = c(30,60,90,120,150,180,240,300),
  iohexol = c(264.529,170.6695,143.0782,118.5563,
           102.927,89.5715,67.937,51.058))


## ├ Tondel data ----
# full example data provided by Tondel in Table 2: https://pubmed.ncbi.nlm.nih.gov/29134449/
dat_tondel <- data.frame(time=c(10,30,120,180,210,240,300),
                         iohexol=c(464,343,156,100,84,72,51))
usethis::use_data(dat_schwartz)
usethis::use_data(dat10)
usethis::use_data(dat17)
usethis::use_data(dat_tondel)




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

## ├ Relmapirazin Data ----
df_relmapirazin_dat <-
  readr::read_csv(here::here("data-raw/relmapirazin/mgfr_data_dorshow_2024.csv")) |> 
  filter(!is.na(subject_number)) |>
  nest(data = c(time, relmapirazin_ng_ml)) |> 
  select(-mgfr)
df_relmapirazin_ids <-
  readr::read_csv(here::here("data-raw/relmapirazin/mgfr_data_dorshow_2024_ids.csv")) |> 
  select(-panel)
df_relmapirazin <-
  left_join(df_relmapirazin_dat, df_relmapirazin_ids, by = c("subject_number" = "subject_number")) 

usethis::use_data(df_relmapirazin)

  
# Internal Data ----
# Internal data objects are all stored within a single .rda file (R/sysdata.rda) 
# Internal data objects can be made available for use with `devtools::load_all()` 

# original data files are stored in `data-raw` folder and are not exported in the package

## ├ Omnipaque Table ----
# US brand = 300 or 350
df_omnipaque <- 
  data.frame(
  omnipaque_v = c(350L, 300L, 240L, 180L, 140L),
  iohexol_mg_ml = c(755L, 647L, 518L, 388L, 302L),
  omnipaque_specgrav = c(1406L, 1349L, 1280L, 1209L, 1164L),
  iohexol_mg_5ml = c(3775L, 3235L, 2594L, 1940L, 1510L)
)
saveRDS(df_omnipaque, here::here("data-raw/df_omnipaque.rds"))


## ├ PREVENT coefficient tables ----
cfs_base10yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_base10yr")
cfs_full10yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_full10yr")
cfs_base30yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_base30yr")
cfs_full30yr <-  readxl::read_excel(here::here("data-raw/prevent_coefficients.xlsx"), sheet = "cfs_full30yr")

saveRDS(cfs_base10yr, here::here("data-raw/cfs_base10yr.rds"))
saveRDS(cfs_full10yr, here::here("data-raw/cfs_full10yr.rds"))
saveRDS(cfs_base30yr, here::here("data-raw/cfs_base30yr.rds"))
saveRDS(cfs_full30yr, here::here("data-raw/cfs_full30yr.rds"))

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

saveRDS(df_sdi, here::here("data-raw/df_sdi.rds"))


# LOAD AND SAVE/UPDATE INTERNAL DATA ----
df_omnipaque <- readRDS(here::here("data-raw/df_omnipaque.rds"))
cfs_base10yr <- readRDS(here::here("data-raw/cfs_base10yr.rds"))
cfs_full10yr <- readRDS(here::here("data-raw/cfs_full10yr.rds"))
cfs_base30yr <- readRDS(here::here("data-raw/cfs_base30yr.rds"))
cfs_full30yr <- readRDS(here::here("data-raw/cfs_full30yr.rds"))
df_sdi       <- readRDS(here::here("data-raw/df_sdi.rds"))


## ├ SAVE internal objects ----
usethis::use_data(df_omnipaque,
                  cfs_base10yr, cfs_full10yr, 
                  cfs_base30yr, cfs_full30yr, 
                  df_sdi, 
                  internal = TRUE,
                  overwrite = TRUE)
