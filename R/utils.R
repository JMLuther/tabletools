# Utility Functions ----
## ├ Handle Sex/Gender consistently ----
# Always return Female/Male as character
handle_sex <- #Vectorize(
  function(sex){
  sx = tolower(sex)
  sx = ifelse(sx %in% c("female", "women","woman", "fem", "fe", "f"), "Female",
              ifelse(sx %in% c("male", "man", "men", "ma", "m"), "Male", NA_character_))
  sx
}
# )
# test_case <- c("M", "ma", "man", "FE", "MA", "F", NA)
# handle_sex(test_case)


# Always return Female/Male as character
handle_race <- 
  # Vectorize(
  function(race){
  rx = tolower(race)
  rx = ifelse(rx %in% c("white", "w", "caucasian", "cauc.", "cauc"), "White",
              ifelse(rx %in% c("black", "b", "bl", "aa", "african american", 
                               "black or african american",
                               "afr. amer.", "a.a."), "Black",
                     ifelse(rx %in% c("asian", "chinese american", "chinese"), "Asian", 
                            # ifelse(rx %in% c("chinese american", "chinese"), "Asian", 
                                   ifelse(rx %in% c("hispanic"), "Hispanic", NA_character_))))
  rx
}
# )
test_case <- c(NA, "BL", "black", "White", "AA", "Asian")
handle_race(test_case)
# 

handle_weight_units <- #Vectorize(
  function(weight_units){
  wu = ifelse(tolower(weight_units) %in% c("lbs","pounds", "lb"), "lbs",
              ifelse(tolower(weight_units) %in% c("kg", "kilograms", "kilog", "KG"), "kg", NA_character_))
  wu
}
# )
# test_case <- c("POUNDS", "lbs", "kilog", "kg")
# handle_weight_units(test_case)

