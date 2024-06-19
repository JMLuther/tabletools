## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tabletools)


## -----------------------------------------------------------------------------
convert_glucose_to_mM(100, glucose_units =  "mg/dL")

## -----------------------------------------------------------------------------
convert_insulin_to_pM(10, insulin_units = "uU/mL")

## -----------------------------------------------------------------------------
convert_weight_to_kg(150, weight_units = "lbs")

## -----------------------------------------------------------------------------
convert_height_to_m(70, height_units = "in")

## -----------------------------------------------------------------------------
# handling data stored in a dataframe
ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
                    glucose=c(93, 129, 178, 164, 97),        # mg/dL
                    insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL

# example from Gutch et al 2015
ogtt2 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
                    glucose=c(100, 160, 160, 160, 140),      # mg/dL
                    insulin=c(5, 10, 10, 10, 5))             # uU/mL

## -----------------------------------------------------------------------------
calculate_avg_glucose(5.5)

## -----------------------------------------------------------------------------
calculate_homair(ogtt1$glucose[1], ogtt1$insulin[1])

## -----------------------------------------------------------------------------
calculate_quicki(ogtt1$glucose[1], ogtt1$insulin[1])

## -----------------------------------------------------------------------------
calculate_matsuda_index(ogtt1$time, ogtt1$glucose, ogtt1$insulin)

## -----------------------------------------------------------------------------
calculate_stumvoll_isi(ogtt1$time, ogtt1$glucose, ogtt1$insulin) #

## -----------------------------------------------------------------------------
calculate_bsa(weight = 70, height =  1.50, method = "Mosteller",
              weight_units = "kg", height_units = "m")

## -----------------------------------------------------------------------------
calculate_egfr_cg(age=70, sex="Male", weight=70, creatinine=1.0) # 95.2

## -----------------------------------------------------------------------------
calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8) # 95.2

