#' Estimated GFR using Full Age Spectrum formula (FAS)
#'
#' Uses age and sex-based median values for estimation of GFR as described by \href{https://pubmed.ncbi.nlm.nih.gov/26932693/}{Pottel et al.}
#' 
#' @param age Age in years 
#' @param sex Sex ("Male", "Female")
#' @param creatinine Serum Creatinine, in mg/dL
#' @param creatinine_units defaults to mg/dL
#'
#' @return a single value or vector of values for egfr
#' @export calculate_egfr_fas
#'
#' @examples
#' data.frame(age=1:20,
#' gender="female") |> 
#'   mutate(egfr= calculate_egfr_fas(age, gender, Q))
#' 
#' # comparison vs 
#' calculate_egfr_fas(age=11, sex="Male", creatinine = 0.69)
#' calculate_egfr_fas(age=11.3, sex="feMale", creatinine = 0.63)
#' 
#' calculate_egfr_fas(age=53, sex="Male", creatinine = 1.1)
#' calculate_egfr_fas(age=53.5, sex="feMale", creatinine = 0.83)
#' 
#' calculate_egfr_fas(age=77.7, sex="Male", creatinine = 1.35)
#' calculate_egfr_fas(age=77.2, sex="feMale", creatinine = 0.98)

calculate_egfr_fas <- Vectorize(
  function(age, sex, creatinine, creatinine_units="mg/dL"){
    if (age>2 & age<40){
      107.3/(creatinine/get_Q(age, sex))
    } else{
      107.3/(creatinine/get_Q(age, sex))*0.988^(age-40)
    }
  })

get_Q <- Vectorize(
  function(age, sex){
 age_r <- ifelse(age<20, floor(age), 20)
 gender_f <- ifelse(age<15, "both", 
                    handle_sex(sex))
 # list(age_r, gender_f)
 df_FAS_Q[df_FAS_Q$age==age_r & df_FAS_Q$gender==gender_f, "Q_mg_dL"]
})

