## code to prepare `DATASET` dataset goes here

# usethis::use_data(overwrite = FALSE)

# Datasets ----

# Datasets that can be loaded by name from the package
# Original source files are stored in "data-raw" folder
# Data files are stored in "data" folder.
# .rda files are created from objects by the `usethis::use_data()` function

library(tidyverse)
library(data.table)

## ├ Iohexol Data ----
### ├ Schwartz ----
# formerly entered in the examples for calculate_mgfr_2c()
# moved to data Oct 2024
dat_schwartz <-
  data.frame(
    time = c(10, 20, 30, 60, 120, 180, 240, 300, 360),
    iohexol_ug_ml = c(
      656.1168132,
      477.1163595,
      371.3542728,
      223.1121251,
      111.1086272,
      61.88251616,
      37.43137242,
      21.79250307,
      12.75996292
    )
  )


### ├ Pottel data ----
# from Supplemental document
dat10 <- data.frame(
  id = c(10, 10, 10, 10, 10, 10, 10, 10),
  time = c(30, 60, 90, 120, 150, 180, 240, 300),
  iohexol = c(
    239.9117,
    217.3945,
    178.215,
    159.4682,
    143.1718,
    130.3613,
    113.6223,
    98.6295
  )
)
dat17 <- data.frame(
  id = c(17, 17, 17, 17, 17, 17, 17, 17),
  time = c(30, 60, 90, 120, 150, 180, 240, 300),
  iohexol = c(
    264.529,
    170.6695,
    143.0782,
    118.5563,
    102.927,
    89.5715,
    67.937,
    51.058
  )
)


### ├ Tondel data ----
# full example data provided by Tondel in Table 2: https://pubmed.ncbi.nlm.nih.gov/29134449/
dat_tondel <- data.frame(
  time = c(10, 30, 120, 180, 210, 240, 300),
  iohexol = c(464, 343, 156, 100, 84, 72, 51)
)

### ├ Ebert example data ----
# data from XLS sheet, Ebert KI 2024: https://pubmed.ncbi.nlm.nih.gov/39097002/
dat_ebert <-
  data.frame(
    time = c(160, 180, 200, 220, 232, 240),
    iohexol = c(70, 60, 47, 37, 30, 25)
  )

usethis::use_data(dat_schwartz)
usethis::use_data(dat10)
usethis::use_data(dat17)
usethis::use_data(dat_tondel)
usethis::use_data(dat_ebert)


## ├ OGTT data ----

gtt <- readRDS(here::here("data-raw/OGTT_merged_2017-12-23.rds"))
ogtt_wide <-
  gtt |>
  mutate(id = row_number()) |>
  filter(!is.na(insulin_auc), cohort != "PPH") |>
  select(
    id,
    age,
    gender.f,
    race.f,
    height_cm,
    weight_kg,
    bmi,
    hdl_bl,
    ldl_bl,
    trigs_bl,
    starts_with("ogtt_glucose"),
    starts_with("ogtt_insulin")
  )
ogtt_nested <-
  gtt |>
  mutate(id = row_number()) |>
  filter(!is.na(insulin_auc), cohort != "PPH") |>
  select(
    id,
    age,
    gender.f,
    race.f,
    height_cm,
    weight_kg,
    bmi,
    hdl_bl,
    ldl_bl,
    trigs_bl,
    ogtt_df
  )


usethis::use_data(ogtt_wide)
usethis::use_data(ogtt_nested)

## ├ Relmapirazin Data ----
df_relmapirazin_dat <-
  readr::read_csv(here::here(
    "data-raw/relmapirazin/mgfr_data_dorshow_2024.csv"
  )) |>
  filter(!is.na(subject_number)) |>
  nest(data = c(time, relmapirazin_ng_ml)) |>
  select(-mgfr)
df_relmapirazin_ids <-
  readr::read_csv(here::here(
    "data-raw/relmapirazin/mgfr_data_dorshow_2024_ids.csv"
  )) |>
  select(-panel)
df_relmapirazin <-
  left_join(
    df_relmapirazin_dat,
    df_relmapirazin_ids,
    by = c("subject_number" = "subject_number")
  )

usethis::use_data(df_relmapirazin)

## ├ HTN medication DDD example data ----
htn_med_dose <-
  data.frame(
    stringsAsFactors = FALSE,
    study_id = c(
      33L,
      66L,
      29L,
      10L,
      23L,
      6L,
      20L,
      39L,
      22L,
      3L,
      51L,
      2L,
      48L,
      9L,
      7L,
      9L,
      60L,
      52L,
      65L,
      56L,
      35L,
      50L,
      27L,
      57L,
      51L,
      10L,
      52L,
      43L,
      10L,
      24L,
      56L,
      58L,
      22L,
      3L,
      64L,
      25L,
      40L,
      65L,
      38L,
      20L,
      38L,
      6L,
      32L,
      40L,
      34L,
      14L,
      2L,
      24L,
      35L,
      14L
    ),
    medication = c(
      "ramipril",
      "spironolactone",
      "nifedipine",
      "metoprolol",
      "amlodipine",
      "enalapril",
      "losartan",
      "valsartan",
      "captopril",
      "clonidine",
      "fosinopril",
      "amlodipine",
      "spironolactone",
      "chlorthalidone",
      "diltiazem",
      "amlodipine",
      "nifedipine",
      "lisinopril",
      "metoprolol",
      "triamterene",
      "valsartan",
      "methyldopa",
      "amlodipine",
      "doxazosin",
      "amlodipine",
      "spironolactone",
      "nifedipine",
      "labetalol",
      "clonidine",
      "losartan",
      "hydrochlorothiazide",
      "verapamil",
      "spironolactone",
      "lisinopril",
      "olmesartan",
      "diltiazem",
      "prazosin",
      "verapamil",
      "spironolactone",
      "spironolactone",
      "clonidine",
      "diltiazem",
      "amlodipine",
      "valsartan",
      "labetalol",
      "amlodipine",
      "benazepril",
      "hydralazine",
      "hydrochlorothiazide",
      "atenolol"
    ),
    dose = c(
      20,
      50,
      120,
      100,
      5,
      40,
      50,
      320,
      150,
      0.6,
      40,
      5,
      75,
      50,
      180,
      10,
      90,
      40,
      200,
      37.5,
      160,
      750,
      10,
      2,
      5,
      50,
      60,
      400,
      0.05,
      100,
      25,
      120,
      100,
      80,
      40,
      240,
      4,
      240,
      100,
      200,
      0.4,
      240,
      10,
      640,
      400,
      10,
      40,
      200,
      25,
      50
    )
  )
usethis::use_data(htn_med_dose)


# Internal Data ----

# Internal data objects are all stored within a single .rda file (R/sysdata.rda)
# Internal data objects can be made available for use with `devtools::load_all()`

# original data files are stored in `data-raw` folder and are not exported in the package

## ├ DDD ----
# DDD data from WHO Collaborating Centre for Drug Statistics Methodology
# https://www.whocc.no/ddd/definition_and_general_considerations/
# DDD is the assumed average maintenance dose per day for a drug used for its main indication in adults
ddd <- readxl::read_excel(here::here("data-raw/DDD list.xlsx"))
names(ddd) <- tolower(names(ddd))
saveRDS(ddd, here::here("data-raw/ddd.rds"))

## ├ eGFR FAS reference table  ----

df_FAS_Q <-
  data.frame(
    stringsAsFactors = FALSE,
    age = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      15,
      16,
      17,
      18,
      19,
      20
    ),
    gender = c(
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "both",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Male",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female",
      "Female"
    ),
    height_cm = c(
      75.00,
      87.00,
      95.50,
      102.50,
      110.00,
      116.70,
      123.50,
      129.50,
      13.50,
      140.00,
      146.00,
      152.50,
      159.00,
      165.00,
      172.00,
      176.00,
      178.00,
      179.00,
      180.00,
      181.50,
      164.50,
      166.00,
      166.50,
      167.00,
      167.50,
      168
    ),
    Q_uM = c(
      23,
      26,
      27,
      30,
      34,
      36,
      39,
      41,
      43,
      45,
      47,
      50,
      52,
      54,
      64,
      69,
      72,
      75,
      78,
      80,
      57,
      59,
      61,
      61,
      62,
      62
    ),
    Q_mg_dL = c(
      0.26,
      0.29,
      0.31,
      0.34,
      0.38,
      0.41,
      0.44,
      0.46,
      0.49,
      0.51,
      0.53,
      0.57,
      0.59,
      0.61,
      0.72,
      0.78,
      0.81,
      0.85,
      0.88,
      0.9,
      0.64,
      0.67,
      0.69,
      0.69,
      0.7,
      0.7
    )
  )
saveRDS(df_FAS_Q, here::here("data-raw/df_FAS_Q.rds"))

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

## ├ MESA CHD risk coefficients ----
mesa10yrcoeff <-
  data.frame(
    stringsAsFactors = FALSE,
    risk = c("mesa_10yr", "mesa_cac_10yr"),
    age = c(0.0455, 0.0172),
    gender_m = c(0.7496, 0.4079),
    race_black = c(-0.2111, 0.0353),
    race_asian = c(-0.5055, -0.3475),
    race_hispanic = c(-0.19, -0.0222),
    diabetes = c(0.5168, 0.3892),
    current_smoker = c(0.4732, 0.3717),
    Tc = c(0.0053, 0.0043),
    HDL = c(-0.014, -0.0114),
    using_lipidloweringagent = c(0.2473, 0.1206),
    SBP = c(0.0085, 0.0066),
    using_antihypertensive_medication = c(0.3381, 0.2278),
    MI_fmhx = c(0.4522, 0.3239),
    cac = c(NA, 0.2743)
  )
saveRDS(mesa10yrcoeff, here::here("data-raw/mesa10yrcoeff.rds"))


## ├ PREVENT coefficient tables ----
cfs_base10yr <- readxl::read_excel(
  here::here("data-raw/prevent_coefficients.xlsx"),
  sheet = "cfs_base10yr"
)
cfs_full10yr <- readxl::read_excel(
  here::here("data-raw/prevent_coefficients.xlsx"),
  sheet = "cfs_full10yr"
)
cfs_base30yr <- readxl::read_excel(
  here::here("data-raw/prevent_coefficients.xlsx"),
  sheet = "cfs_base30yr"
)
cfs_full30yr <- readxl::read_excel(
  here::here("data-raw/prevent_coefficients.xlsx"),
  sheet = "cfs_full30yr"
)

saveRDS(cfs_base10yr, here::here("data-raw/cfs_base10yr.rds"))
saveRDS(cfs_full10yr, here::here("data-raw/cfs_full10yr.rds"))
saveRDS(cfs_base30yr, here::here("data-raw/cfs_base30yr.rds"))
saveRDS(cfs_full30yr, here::here("data-raw/cfs_full30yr.rds"))

## ├ ASCVD Risk Coefficient table ----
df_coeff_ascvd <-
  data.frame(
    stringsAsFactors = FALSE,
    sex = c(
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "female",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male",
      "male"
    ),
    race = c(
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "white",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black",
      "black"
    ),
    coeff_n = c(
      "age",
      "age2",
      "tchol",
      "age_tchol",
      "hdl",
      "age_hdl",
      "sbp_t",
      "age_sbp_t",
      "sbp_u",
      "age_sbp_u",
      "smoker_yn",
      "age_smoker",
      "diabetes_yn",
      "baseline",
      "mean_coeff",
      "age",
      "age2",
      "tchol",
      "age_tchol",
      "hdl",
      "age_hdl",
      "sbp_t",
      "age_sbp_t",
      "sbp_u",
      "age_sbp_u",
      "smoker_yn",
      "age_smoker",
      "diabetes_yn",
      "baseline",
      "mean_coeff",
      "age",
      "age2",
      "tchol",
      "age_tchol",
      "hdl",
      "age_hdl",
      "sbp_t",
      "age_sbp_t",
      "sbp_u",
      "age_sbp_u",
      "smoker_yn",
      "age_smoker",
      "diabetes_yn",
      "baseline",
      "mean_coeff",
      "age",
      "age2",
      "tchol",
      "age_tchol",
      "hdl",
      "age_hdl",
      "sbp_t",
      "age_sbp_t",
      "sbp_u",
      "age_sbp_u",
      "smoker_yn",
      "age_smoker",
      "diabetes_yn",
      "baseline",
      "mean_coeff"
    ),
    coeff_val = c(
      -29.799,
      4.884,
      13.54,
      -3.114,
      -13.578,
      3.149,
      2.019,
      0,
      1.957,
      0,
      7.574,
      -1.665,
      0.661,
      0.967,
      -29.18,
      12.344,
      0,
      11.853,
      -2.664,
      -7.99,
      1.769,
      1.797,
      0,
      1.764,
      0,
      7.837,
      -1.795,
      0.658,
      0.914,
      61.18,
      17.114,
      0,
      0.94,
      0,
      -18.92,
      4.475,
      29.291,
      -6.432,
      27.82,
      -6.087,
      0.691,
      0,
      0.874,
      0.953,
      86.61,
      2.469,
      0,
      0.302,
      0,
      -0.307,
      0,
      1.916,
      0,
      1.809,
      0,
      0.549,
      0,
      0.645,
      0.895,
      19.54
    )
  )
df_coeff_ascvd <-
  df_coeff_ascvd |>
  pivot_wider(names_from = "coeff_n", values_from = "coeff_val")
saveRDS(df_coeff_ascvd, here::here("data-raw/df_coeff_ascvd.rds"))


# usethis::use_data(cfs_base10yr, cfs_full10yr,
#                   cfs_base30yr, cfs_full30yr,
#                   internal = TRUE,
#                   overwrite = TRUE)

## ├ SDI Zip code files ----
# PREVENT used the 2014-2018 data, so this is used for consistency

# library(data.table)
df_sdi <-
  data.table::fread(here::here(
    "data-raw/SDI_zcta/rgcsdi-2015-2019-zcta.csv"
  )) |>
  select(ZCTA5_FIPS, SDI_score) |>
  mutate(sdi_decile = ntile(SDI_score, 10)) |>
  select(ZCTA5_FIPS, SDI_score, sdi_decile)

saveRDS(df_sdi, here::here("data-raw/df_sdi.rds"))


# LOAD AND SAVE/UPDATE INTERNAL DATA ----
# add new object to this and then udpate with each new data set
df_omnipaque <- readRDS(here::here("data-raw/df_omnipaque.rds"))
cfs_base10yr <- readRDS(here::here("data-raw/cfs_base10yr.rds"))
cfs_full10yr <- readRDS(here::here("data-raw/cfs_full10yr.rds"))
cfs_base30yr <- readRDS(here::here("data-raw/cfs_base30yr.rds"))
cfs_full30yr <- readRDS(here::here("data-raw/cfs_full30yr.rds"))
df_sdi <- readRDS(here::here("data-raw/df_sdi.rds"))
df_FAS_Q <- readRDS(here::here("data-raw/df_FAS_Q.rds"))
df_coeff_ascvd <- readRDS(here::here("data-raw/df_coeff_ascvd.rds"))
mesa10yrcoeff <- readRDS(here::here("data-raw/mesa10yrcoeff.rds"))
ddd <- readRDS(here::here("data-raw/ddd.rds"))


## ├ SAVE internal objects ----
usethis::use_data(
  ddd,
  df_omnipaque,
  cfs_base10yr,
  cfs_full10yr,
  cfs_base30yr,
  cfs_full30yr,
  mesa10yrcoeff,
  df_coeff_ascvd,
  df_sdi,
  df_FAS_Q,
  internal = TRUE,
  overwrite = TRUE
)
