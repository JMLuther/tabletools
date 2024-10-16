#' @export
#' @importFrom tabletools calculate_map 
#' @rdname calculate_iga_risk_postbiopsy 
calculate_iga_risk_postbiopsy_norace <- function(egfr, sbp, dbp, proteinuria, age, 
                                                 mest_M, mest_E, mest_S, mest_T, 
                                                 time=60,
                                                 immunosuppression=FALSE, RASB=TRUE,
                                                 explanation=FALSE){
  mean_ap = tabletools::calculate_map(sbp, dbp)
  M1 = as.numeric(mest_M == 1) # 0/1
  E1 = as.numeric(mest_E == 1) # 0/1
  S1 = as.numeric(mest_S == 1) # 0/1
  T1 = as.numeric(mest_T == 1) # 0/1; if T=2, then 0
  T2 = as.numeric(mest_T == 2)     # 0/1; 1 if T=2
  
  #  LP = Linear predictor
  LP = -0.5862*(sqrt(egfr)-8.8) - 0.0012*(mean_ap-97) + 0.1225*(log(proteinuria) - 0.09) + 
    0.0055*((mean_ap*log(proteinuria)) - 8.73) + 
    0.2693*M1 + 0.2447*E1 - 0.0050*S1 + 0.3237*T1 + 0.4221*T2 +
    -0.1732*T1*log(proteinuria) - 0.3120*T2*log(proteinuria) +
    -0.0227*(age-38) + 
    0.2675*RASB - 0.0478*RASB* log(proteinuria) -0.3266*immunosuppression
  
  s1=max((time-2.73)/38.41871,0)^3+(46.94*max((time-240.86)/38.41871,0)^3-238.13*max((time-49.67)/38.41871,0)^3)/191.19 
  S0 =  ifelse(time<12, 
               0.9983872-0.0033270*((time+0.1)/10)^0.5 - 0.0039851*(((time+0.1)/10)^0.5)*log((time+0.1)/10), #for time<12 months
               1.00651-0.00105*time-0.00281*s1) # for time≥12 months
  
  pred_risk = 1 - S0^exp(LP)
  
  # return(data.frame(pred_risk, s1, S0, LP, mean_ap, M1, E1, S1, T1, T2))
  ifelse(!explanation, return(pred_risk),
         return(sprintf("Probability of an adverse kidney event (50%% eGFR decline or ESRD) at %1.0f months is %1.1f%%",
                        time, pred_risk*100))         )
  
}
