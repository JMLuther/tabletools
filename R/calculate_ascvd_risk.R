#' Calculate 10-year ASCVD Risk (2014 original formula) 
#'
#' @param age Age in years
#' @param sex Sex assigned at birth (Male/Female)
#' @param race Race self-identified
#' @param total_chol Total Cholesterol, mg/dL
#' @param hdl_chol HDL Cholesterol, mg/dL
#' @param sbp Systolic Blood Pressure, mmHg
#' @param using_antihypertensive_medication Hypertension medication use (TRUE/FALSE) 
#' @param current_smoker Currently Smoking  (TRUE/FALSE)
#' @param diabetes Diabetes diganosis  (TRUE/FALSE)
#' @param chol_units Cholesterol units (default "mg/dL")
#'
#' @return A single value for 10-year Cardiovascular Risk. Multiple *100 to obtain as a percentage. 
#' @export calculate_ascvd_risk
#' @export
#'
#' @examples
#' calculate_ascvd_risk(sex = "female",
#'                      race = "white",
#'                      age= 55,
#'                      total_chol = 213,
#'                      hdl_chol = 50,
#'                      sbp=120,
#'                      using_antihypertensive_medication=FALSE,
#'                      current_smoker=FALSE,
#'                      diabetes=FALSE)
# 
#' n = 10
#' tibble(id = 1:n,
#'        sex = sample(c("female","male"), n, replace = T),
#'        race = sample(c("white","black"), n, replace = T),
#'        age= rnorm(n, 55),
#'        total_chol = rnorm(n, 213),
#'        hdl_chol = rnorm(n, 50),
#'        sbp=rnorm(n, 120),
#'        using_antihypertensive_medication=sample(c(TRUE, FALSE), n, replace = T),
#'        current_smoker=sample(c(TRUE, FALSE), n, replace = T),
#'        diabetes=sample(c(TRUE, FALSE), n, replace = T)) |> 
#'   mutate(ascvd_r = calculate_ascvd_risk(age = age, sex = sex, race=race, 
#'                                           total_chol = total_chol, hdl_chol=hdl_chol,
#'                                           sbp=sbp, using_antihypertensive_medication=using_antihypertensive_medication,
#'                                           current_smoker=current_smoker,
#'                                           diabetes=diabetes))

calculate_ascvd_risk_ <- function(age, sex, race, 
                                 total_chol, hdl_chol, 
                                 sbp,
                                 using_antihypertensive_medication=FALSE,
                                 current_smoker=FALSE,
                                 diabetes=FALSE,
                                 chol_units = "mg/dL") {
  
  if (!is.logical(c(using_antihypertensive_medication, current_smoker, diabetes))){
    stop("use TRUE/FALSE for arguments `using_antihypertensive_medication`, `current_smoker`, `diabetes`")
  }
  total_chol =   convert_cholesterol_to_mgdl(total_chol, cholesterol_units = chol_units)
  hdl_chol =   convert_cholesterol_to_mgdl(hdl_chol, cholesterol_units = chol_units)
  gender_query = tolower(handle_sex(sex)) # Male/Female
  race =   ifelse(tolower(race) %in% c("black", "aa", "african american", "afr. am."), "black",
                  ifelse(tolower(race) %in% c("white", "caucasian"), "white", NA))
  cf <-  df_coeff_ascvd[df_coeff_ascvd$race=={{race}} & df_coeff_ascvd$sex=={{gender_query}}, ]
  
  # construct the model
  coeff_sum =
    cf$age * log(age) +   # AGE 
    cf$age2* log(age)^2 + # AGE^2
    cf$tchol * log(total_chol) +  # Total Chol
    cf$age_tchol*log(total_chol)*log(age) + 
    cf$hdl * log(hdl_chol) + 
    cf$age_hdl * log(age) * log(hdl_chol) + 
    cf$sbp_t * log(sbp) * using_antihypertensive_medication + 
    cf$age_sbp_t * log(age) * log(sbp) * using_antihypertensive_medication + 
    cf$sbp_u * log(sbp) * (!using_antihypertensive_medication) + 
    cf$age_sbp_u * log(age) * log(sbp) * (!using_antihypertensive_medication) + 
    cf$smoker_yn*current_smoker + 
    cf$age_smoker * log(age) * current_smoker +
    cf$diabetes_yn * diabetes
  
  return((1-cf$baseline)*exp(coeff_sum - cf$mean_coeff)) # 10-year Risk
}
calculate_ascvd_risk <- Vectorize(calculate_ascvd_risk_)
