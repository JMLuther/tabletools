## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(tabletools)
library(tidyverse)

## -----------------------------------------------------------------------------
convert_glucose_to_mM(100, glucose_units =  "mg/dL")

## -----------------------------------------------------------------------------
convert_insulin_to_pM(10, insulin_units = "uU/mL")

## -----------------------------------------------------------------------------
convert_weight_to_kg(150, weight_units = "lbs")

## -----------------------------------------------------------------------------
convert_length_to_m(70, length_units = "in")

## -----------------------------------------------------------------------------
data(ogtt_wide)
ogtt_wide |> head()

## -----------------------------------------------------------------------------
ogtt_long <- 
  ogtt_wide |> 
  select(id, starts_with("ogtt_")) |> 
  pivot_longer(cols = -id,
               names_to = c(".value", "time"),
               names_pattern = "ogtt_(.*)_([0-9]+)",
               values_to = ".value",
               names_transform = list(time = ~as.integer(.x))) |> # could be more complicated if not integers
  group_by(id) # wise to group by id at this point

ogtt_long |> head(10)

## -----------------------------------------------------------------------------
ogtt_long |> 
  nest(.key = "ogtt_df") |> # already grouped by id
  head()

## -----------------------------------------------------------------------------
data("ogtt_nested")
ogtt_nested |> select(1:3, ogtt_df) |>  head()

## -----------------------------------------------------------------------------
ogtt_nested$ogtt_df[[1]]

## -----------------------------------------------------------------------------
data("ogtt_nested")

ogtt_long <- 
  ogtt_nested |> 
  select(id, ogtt_df) |> 
  unnest(cols = ogtt_df) |> 
  group_by(id)
head(ogtt_long,10)


## ----figheight=4, fig.width=6-------------------------------------------------

theme_set(theme_jml())
pl_glucose <- 
  ogtt_long |> 
  ggplot(aes(time, glucose)) +
  geom_line(aes(group=id), alpha=0.3)
pl_insulin<- ogtt_long |> 
  ggplot(aes(time, insulin)) +
  geom_line(aes(group=id), alpha=0.3)
cowplot::plot_grid(pl_glucose, pl_insulin, nrow=1)


## -----------------------------------------------------------------------------
pl_summary_glucose <- 
  ogtt_long |> 
  mutate(glucose_tolerance.f = factor_glucose_tolerance(glucose[time==120])) |> 
  group_by(time, glucose_tolerance.f) |> 
  my_summary(glucose)  |> 
  ggplot(aes(time, glucose_mean, group=glucose_tolerance.f, color=glucose_tolerance.f)) +
  geom_point() + geom_line() + 
  geom_pointrange(aes(ymin=glucose_mean-glucose_sem, 
                      ymax=glucose_mean+glucose_sem)) +
  labs(y="Glucose (mg/dL)", color= "Glucose Tolerance:")
  
pl_summary_insulin <-
  ogtt_long |> 
  mutate(glucose_tolerance.f = factor_glucose_tolerance(glucose[time==120])) |> 
  group_by(time, glucose_tolerance.f) |> 
  my_summary(insulin)  |> 
  ggplot(aes(time, insulin_mean, group=glucose_tolerance.f, color=glucose_tolerance.f)) +
  geom_point() + geom_line() + 
  geom_pointrange(aes(ymin=insulin_mean-insulin_sem, 
                      ymax=insulin_mean+insulin_sem)) +
  labs(y="Insulin (uU/mL)", color= "Glucose Tolerance:")
  

cowplot::plot_grid(pl_summary_glucose , 
                   pl_summary_insulin ,
                   rel_widths = c(1,1),
                   nrow = 1) 

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
calculate_isi_matsuda(ogtt1$time, ogtt1$glucose, ogtt1$insulin)

## -----------------------------------------------------------------------------
calculate_isi_ogis(ogtt1$time, ogtt1$glucose, ogtt1$insulin,  height=1.70, weight = 70)

## -----------------------------------------------------------------------------
calculate_isi_stumvoll(ogtt1$time, ogtt1$glucose, ogtt1$insulin) #

## -----------------------------------------------------------------------------
calculate_isi_cederholm(ogtt1$time, ogtt1$glucose, ogtt1$insulin, weight=70)

## -----------------------------------------------------------------------------
calculate_isi_gutt(ogtt1$time, ogtt1$glucose, ogtt1$insulin, bmi=30)

## -----------------------------------------------------------------------------
calculate_isi_caumo(ogtt1$time, ogtt1$glucose, ogtt1$insulin, weight = 100)

## -----------------------------------------------------------------------------
calculate_isi_bigtt(ogtt1$time, ogtt1$glucose, ogtt1$insulin, gender = "F", bmi=30)

## -----------------------------------------------------------------------------
calculate_isi_avignon(ogtt1$time, ogtt1$glucose, ogtt1$insulin, weight = 100)

## -----------------------------------------------------------------------------
calculate_isi_belfiore(ogtt1$time, ogtt1$glucose, ogtt1$insulin)

## ----message=FALSE, warning=FALSE---------------------------------------------
ogtt_nested |> 
  mutate(glucose_0 = map_dbl(ogtt_df, ~.x$glucose[.x$time==0]),
         glucose_fasting.f = factor_glucose_fasting(map_dbl(ogtt_df, ~.x$glucose[.x$time==0])),
         glucose_tolerance.f = factor_glucose_tolerance(map_dbl(ogtt_df, ~.x$glucose[.x$time==0])),
         
         isi_stumvoll = map_dbl(ogtt_df, ~calculate_isi_stumvoll(.x$time, .x$glucose, .x$insulin)),
         isi_stumvoll_dem = pmap_dbl(list(ogtt_df, bmi, age), 
                             ~calculate_isi_stumvoll_dem(..1$time, ..1$glucose, ..1$insulin, bmi=..2, age=..3)),
         isi_matsuda = map_dbl(ogtt_df, ~calculate_isi_matsuda(.x$time, .x$glucose, .x$insulin)),
         isi_caumo = map2_dbl(ogtt_df, weight_kg, ~calculate_isi_caumo(.x$time, .x$glucose, .x$insulin, weight=.y)),
         isi_cederholm = map2_dbl(ogtt_df, weight_kg, ~calculate_isi_cederholm(.x$time, .x$glucose, .x$insulin, weight=.y)),
         isi_gutt = map2_dbl(ogtt_df, bmi, ~calculate_isi_gutt(.x$time, .x$glucose, .x$insulin, bmi=.y)),
         isi_ogis = pmap_dbl(list(ogtt_df, height_cm/100, weight_kg), 
                             ~calculate_isi_ogis(..1$time, ..1$glucose, ..1$insulin, height=..2, weight=..3)),
         isi_bigtt =pmap_df(list(ogtt_df, gender.f, bmi), 
                             ~calculate_isi_bigtt(..1$time, ..1$glucose, ..1$insulin, gender=..2, bmi=..3)),
         beta = map_df(ogtt_df, ~calculate_stumvoll_beta(.x$time, .x$glucose, .x$insulin)),
         )


## -----------------------------------------------------------------------------
calculate_bsa(weight = 70, height =  1.50, method = "Mosteller",
              weight_units = "kg", height_units = "m")

## -----------------------------------------------------------------------------
calculate_creatclearance_cg(age=70, sex="Male", weight=70, creatinine=1.0) # 68

## -----------------------------------------------------------------------------
calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8) # 95.2

