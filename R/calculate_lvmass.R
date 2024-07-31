#' Calculate LV Mass using Echocardiogram results   
#'
#' Uses methods according to \href{https://pubmed.ncbi.nlm.nih.gov/25559473/}{American Society of Echocardiography}
#'
#' @param lvedd LV Internal Diameter Diastole 
#' @param ivsd  LV Septal Thickness Diastole
#' @param lvpwd LV Posterior Thickness Diastole
#' @param bsa   Body surface area (\eqn{m^2})
#' @param length_units 
#'
#' @return dataframe of echocardiagraphic LV mass results
#' @export
#'
#' @examples
#' calculate_lvmass(lvedd = 50.5, ivsd = 10.7, lvpwd = 13.3, bsa=2.42, length_units = "mm")

calculate_lvmass <- function(lvedd, ivsd, lvpwd, bsa, length_units="cm"){
  
  # convert units to mm for calculation
  lvedd_mm = convert_length_to_mm(lvedd, length_units)
  ivsd_mm = convert_length_to_mm(ivsd, length_units)
  lvpwd_mm = convert_length_to_mm(lvpwd, length_units)

  lvm = (0.8 * (1.04*((lvedd_mm + ivsd_mm + lvpwd_mm)^3 - lvedd_mm^3)) + 0.6)/1000 # grams
  lvmi = lvm/bsa
  rwt = 2*lvpwd_mm/lvedd_mm
  res_df = data.frame(lvm, lvmi, rwt)
  return(res_df)
}

