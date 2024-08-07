## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

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
