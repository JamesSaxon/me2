source("om_me2.R")

responses <- names(new_data)

vardict <- data.frame(id = responses, labels = varlabels)

fneg = c("Non-interview", "Valid missing", "Invalid missing", "Don't know", "Refused")

nonint_nom_vars <- data.frame(Response = c("R0063500", "R0112000", "R0115600", "R0163900", "R0254700", 
                                           "R0269300", "R0286200", "R0373900", "R0407600", "R0448800", "R0549710"), 
                              Year     = c(1967, 1968, 1969, 1971, 1973, 1975, 1976, 1978, 1980, 1981, 1983),
                              stringsAsFactors = FALSE)


nonint_labels = c(fneg,
                  "UNABLE TO LOCATE RESPONDENT-REASON NOT SPECIFIED",
                  "UNABLE TO LOCATE-MOVER-INTERVIEW IMPOSSIBLE TO OBTAIN",
                  "UNABLE TO LOCATE-MOVER-UNABLE TO OBTAIN INTERVIEW AFTER REPEATED ATTEMPTS",
                  "UNABLE TO LOCATE-MOVER-NO GOOD ADDRESS GIVEN",
                  "UNABLE TO LOCATE-NONMOVER-UNABLE TO OBTAIN INTERVIEW AFTER REPEATED ATTEMPTS",
                  "TEMPORARILY ABSENT",
                  "INSTITUTIONALIZED",
                  "REFUSED",
                  "DECEASED",
                  "OTHER", 
                  "NONINTERVIEW 2 CONSECTUTIVE YEARS; DROPPED FROM SAMPLE",
                  "ARMED FORCES", 
                  "MOVED OUTSIDE OF U.S. (OTHER THAN ARMED FORCED)", 
                  "CONGRESSIONAL REFUSAL")

marstat_nom_vars <- data.frame(Response = c("R0002400", "R0063700", "R0116400", "R0164000", "R0268850", "R0285000", 
                                            "R0325210", "R0374000", "R0407700", "R0474200", "R0549800", "R0703300"),
                               Year     = c(1966, 1967, 1969, 1971, 1973, 1975, 1976, 1978, 1980, 1981, 1983, 1990),
                               stringsAsFactors = F)

marstat_labels = c(fneg, "Spouse Present", "Spouse Absent", "Widowed", "Divorced", "Separated", "Never Married")

health_limits <- data.frame(activities = c("Walking", "Stairs", "Standing", "Sitting", "Stooping", 
                                           "Lifting10lb", "LiftingHeavy", "Reaching", "Handling", "Seeing", "Hearing"),
                              resp1971 = c("R0213800", "R0213900", "R0214000", "R0214100", "R0214200", "R0214300", 
                                           "R0214400", "R0214500", "R0214600", "R0214700", "R0214800"),
                              resp1976 = c("R0320500", "R0320600", "R0320700", "R0320800", "R0320900", "R0321000", 
                                           "R0321100", "R0321200", "R0321300", "R0321400", "R0321500"),
                              resp1981 = c("R0488700", "R0489000", "R0489300", "R0489600", "R0489900", "R0490200",
                                           "R0490500", "R0490800", "R0491100", "R0491400", "R0491700"),
                              stringsAsFactors = FALSE)

health_conditions = c("Any", "Pain", "Tiring", "Weakness", "Aches", "Fainting", "Tension", "Breath")

total_income <- data.frame(Response = c("R0057520", "R0106320", "R0162320", "R0253920", "R0267620",
                                        "R0283720", "R0371120", "R0403820", "R0434920", "R0547720", "R0708620"),
                           Year     = c(1966, 1967, 1969, 1971, 1973, 1975, 1976, 1978, 1980, 1981, 1990),
                           stringsAsFactors = FALSE)

wages_salary <- data.frame(Response = c("R0080200", "R0134300", "R0223500", "R0329600", "R0522300", "R0589400", "R0686300"),
                           Year     = c(1967, 1969, 1971, 1976, 1981, 1983, 1990),
                           stringsAsFactors = FALSE)

factor_data <- function(data) {

  for (i in 1:nrow(nonint_nom_vars)) {
    data[paste0("fNonInt_", nonint_nom_vars[i,"Year"])] <- factor(data[[nonint_nom_vars[i,"Response"]]], levels=c(-5:13), labels=nonint_labels)
  }

  data["fNonInt_1990"] <- factor(data[["R0601401"]], levels=c(-5:-1, 1:19), 
                                 labels=c(fneg,
                                          "UNABLE TO LOCATE SP (NO GOOD ADDRESS)",
                                          "ABLE TO LOCATE SP, UNABLE TO CONTACT",
                                          "SP REFUSED",
                                          "SP MENTALLY OR PHYSICALLY INCAPABLE, NOT INSTITUTIONALIZED, NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "SP MENTALLY OR PHYSICALLY INCAPABLE, IS INSTITUTIONALIZED, NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "SP TEMPORARILY ABSENT, NO PROXY AVAILABLE OR REFUSED",
                                          "SP MOVED OUTSIDE THE U.S., NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "SP - OTHER",
                                          "UNABLE TO LOCATE WIDOW (NO GOOD ADDRESS)",
                                          "ABLE TO LOCATE WIDOW, UNABLE TO CONTACT",
                                          "WIDOW REFUSED",
                                          "WIDOW MENTALLY OR PHYSICALLY INCAPABLE, NOT INSTITUTIONALIZED, NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "WIDOW MENTALLY OR PHYSICALLY INCAPABLE, IS INSTITUTIONALIZED, NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "WIDOW TEMPORARILY ABSENT, NO PROXY AVAILABLE OR REFUSED",
                                          "WIDOW MOVED OUTSIDE THE U.S., NO PROXY AVAILABLE OR PROXY REFUSED",
                                          "NO LIVING WIDOW, NO PROXY AVAILABLE",
                                          "NO LIVING WIDOW, PROXY REFUSED",
                                          "WIDOW/PROXY - OTHER",
                                          "CONGRESSIONAL REFUSAL NOTE: CODING CATEGORIES ARE DIFFERENT FROM PREVIOUS YEARS"))

  data["fResp_1990"] <- data[["R0601590"]]
  data$fResp_1990[0 <= data$fResp_1990 & data$fResp_1990 < 10] <- 1
  data$fResp_1990[10 <= data$fResp_1990 & data$fResp_1990 <= 18] <- 2
  data$fResp_1990[data$fResp_1990 == 19] <- 3
  data[["fResp_1990"]] <- factor(data[["fResp_1990"]], levels=c(-5, 1:3),
                                 labels=c("Non-interview", # "Valid missing", "Invalid missing", "Don't know", "Refused",
                                          "Respondent", "Widow", "Widow Proxy"))

  for (i in 1:nrow(marstat_nom_vars)) {
    data[paste0("fMarStat_", marstat_nom_vars[i,"Year"])] <- factor(data[[marstat_nom_vars[i,"Response"]]],
                                                                    levels=c(-5:-1, 1:6), labels=marstat_labels)


    spousevar <- paste0("fSpousePresent_", marstat_nom_vars[i,"Year"])
    data[[spousevar]] <- 0
    data[data[[marstat_nom_vars[i,"Response"]]] == 1, spousevar] <- 1
    data[data[[marstat_nom_vars[i,"Response"]]] < 0,  spousevar] <- NA

    data[paste0("fSpousePresent_", marstat_nom_vars[i,"Year"])] <- factor(data[[spousevar]],
                                                                    levels=c(1, 0), labels=c("True", "False"))
  }


  data["fRace"] <- factor(data[["R0002300"]], levels=c(1.0,2.0,3.0), labels=c("White", "Black", "Other"))

  data["fSchooling"] <- 0
  data[data[["R0056400"]] >= 12, "fSchooling"] <- 1
  data[data[["R0056400"]] >= 16, "fSchooling"] <- 2
  data[data[["R0056400"]] <   0, "fSchooling"] <- NA
  data["fSchooling"] <- factor(data[["fSchooling"]], levels=c(0:2), labels=c("Less than HS", "High School Grad", ">= 4 Yrs College"))


  data["fConditionAny_1976"] <- factor(data$R0321800, levels=0:1, labels=c("NO", "YES"))
  data["fConditionAny_1981"] <- factor(data$R0492600, levels=0:1, labels=c("NO", "YES"))
  for (ci in seq_along(health_conditions)) {

    ## 1976
    resp76 <- sprintf("R03219%02d", ci)
    var76  <- sprintf("fCondition%s_%d", health_conditions[ci], 1976)

    data[var76] <- data[resp76]
    data[data[[var76]] == -4, var76] <- 0
    data[var76] <- factor(data[[var76]], levels=0:1, labels=c("NO", "YES"))

    ## 1981
    resp81 <- sprintf("R04927%02d", ci)
    var81  <- sprintf("fCondition%s_%d", health_conditions[ci], 1981)

    data[var81] <- data[resp81]
    data[data[[var81]] == -4, var81] <- 0
    data[var81] <- factor(data[[var81]], levels=0:1, labels=c("NO", "YES"))
  }

  data["fConditionPain_1990"]     <- factor(data$R0621700, levels=0:1, labels=c("NO", "YES"))
  data["fConditionTiring_1990"]   <- factor(data$R0621800, levels=0:1, labels=c("NO", "YES"))
  data["fConditionWeakness_1990"] <- factor(data$R0621900, levels=0:1, labels=c("NO", "YES"))
  data["fConditionAches_1990"]    <- factor(data$R0622000, levels=0:1, labels=c("NO", "YES"))
  data["fConditionFainting_1990"] <- factor(data$R0622100, levels=0:1, labels=c("NO", "YES"))
  data["fConditionTension_1990"]  <- factor(data$R0622200, levels=0:1, labels=c("NO", "YES"))
  data["fConditionBreath_1990"]   <- factor(data$R0622300, levels=0:1, labels=c("NO", "YES"))

  
 
  data["fOutdoorSolo_1990"] <- factor(data$R0623900, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES"))
  data["fHelpWalking_1990"] <- factor(data$R0625200, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES"))

  data["fHealthPreventsWork_1966"] <- factor(data$R0018300, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES")) 
  data["fHealthPreventsWork_1969"] <- factor(data$R0130200, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES")) 
  data["fHealthPreventsWork_1976"] <- factor(data$R0318000, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES")) 
  data["fHealthPreventsWork_1981"] <- factor(data$R0487500, levels=c(-5:-1, 0:1), labels=c(fneg, "NO", "YES")) 



  for (vi in 1:nrow(health_limits)) {

    activity <- health_limits[vi,"activities"]

    for (yr in c(1971, 1976, 1981)) {

      var  <- sprintf("fHealthLimits%s_%d", activity, yr)
      resp <- health_limits[vi, sprintf("resp%d", yr)]

      data[var] <- data[[resp]]
      data[data[[resp]] >= 0, var] <- 1 # Here, the UNIVERSE is the answer....
      data[var] <- factor(data[[var]], levels=c(-4, 1), labels=c("No Problem", "Difficulty"))

    }
  }



  for (vi in 1:nrow(total_income)) {
    var  <- paste0("fTotalInc_", total_income[vi, "Year"])
    resp <- total_income[vi, "Response"]

    data[var] <- NA
    data[    0 <= data[[resp]] & data[[resp]] <  5000, var] <- 0
    data[ 5000 <= data[[resp]] & data[[resp]] < 10000, var] <- 1
    data[10000 <= data[[resp]] & data[[resp]] < 15000, var] <- 2
    data[15000 <= data[[resp]] & data[[resp]] < 20000, var] <- 3
    data[20000 <= data[[resp]] & data[[resp]] < 30000, var] <- 4
    data[30000 <= data[[resp]] & data[[resp]] < 1e6,   var] <- 5
    data[[var]] <- factor(data[[var]], levels = c(0:5), labels = c("> $0k", "> $5k", "> $10k", "> $15k", "> $20k", "> $30k"))
  }



  for (vi in 1:nrow(wages_salary)) {
    var  <- paste0("fWages_", wages_salary[vi, "Year"])
    resp <- wages_salary[vi, "Response"]

    data[var] <- NA
    data[    0 <= data[[resp]] & data[[resp]] <  5000, var] <- 0
    data[ 5000 <= data[[resp]] & data[[resp]] < 10000, var] <- 1
    data[10000 <= data[[resp]] & data[[resp]] < 15000, var] <- 2
    data[15000 <= data[[resp]] & data[[resp]] < 20000, var] <- 3
    data[20000 <= data[[resp]] & data[[resp]] < 30000, var] <- 4
    data[30000 <= data[[resp]] & data[[resp]] < 1e6,   var] <- 5
    data[[var]] <- factor(data[[var]], levels = c(0:5), labels = c("> $0k", "> $5k", "> $10k", "> $15k", "> $20k", "> $30k"))
  }



  data["BMI_1973"] <- 703.27 * data$R0258700 / (data$R0258600 * data$R0258600)
  data[(data$R0258700 < 0) | (data$R0258600 < 0), "BMI_1973"] <- NA

  data["fBMI_1973"] <- NA
  data$fBMI_1973[0  <= data$BMI_1973 & data$BMI_1973 < 20] <- 1
  data$fBMI_1973[20 <= data$BMI_1973 & data$BMI_1973 < 25] <- 2
  data$fBMI_1973[25 <= data$BMI_1973 & data$BMI_1973 < 30] <- 3
  data$fBMI_1973[30 <= data$BMI_1973] <- 4
  data["fBMI_1973"] <- factor(data$fBMI_1973, levels=c(1:4), labels=c("< 20", "20-25", "25-30", "> 30"))

  data["fHomeLanguage_1971"] <- NA
  data$fHomeLanguage_1971 <- factor(data$R0228000, levels=c(0:1), labels = c("English", "Spanish")) # "Germanic", "Other Romance", "Slavic", "Other"))

  data["fSmoking_1990"] <- NA
  data$fSmoking_1990[data$R0627700 == 1] <- 1 # still smokes.
  data$fSmoking_1990[data$R0628100 == 0] <- 0 # never have I ever.
  data$fSmoking_1990[data$R0628100 == 1] <- 2 # ever yes.
  data$fSmoking_1990[data$R0719600 == 0] <- 3
  data$fSmoking_1990[data$R0719600 == 1] <- 4
  data$fSmoking_1990 <- factor(data$fSmoking_1990, levels=c(0:4),
                               labels = c("No (R)", "Yes (R)", "Quit (R)", "No (W)", "Yes (W)"))

  data["fHeavyAlcohol_1990"] <- NA
  data$fHeavyAlcohol_1990[data$R0628600 >= 0] <- 0
  data$fHeavyAlcohol_1990[(data$R0628700 %in% 1:2) & (data$R0628800 %in% 1:3)] <- 1

  data$fHeavyAlcohol_1990[data$R0720400 >= 0] <- 2
  data$fHeavyAlcohol_1990[(data$R0720500 %in% 1:2) & (data$R0720600 %in% 1:3)] <- 3

  data$fHeavyAlcohol_1990 <- factor(data$fHeavyAlcohol_1990, levels=c(0:3),
                               labels = c("No (R)", "Yes (R)", "No (W)", "Yes (W)"))

  return(data)
}


death_year <- function(data, death_age_var) {

  v <- data[["birthYear"]] + d[[death_age_var]]
  matched <- (data[[death_age_var]] > 0) & (data[[death_age_var]] < 90)
  v[!matched] <- NA

  v
}



ncol_nom <- length(new_data)

d <- factor_data(new_data)

colnames(d)[1:ncol_nom] <- varlabels

d["birthYear"] <- d["YR OF R BIRTH, 66"] + 1900
d[d["birthYear"] > 1966, "birthYear"] <- d[d["birthYear"] > 1966, "birthYear"] - 100

d["ndiDeathYear"]   <- death_year(d, "NDI MATCHING - AGE OF DEATH, 2011")
d["ssDeathYear"]    <- death_year(d, "SS MATCHING - AGE OF DEATH, 2011")
d["certDeathYear"]  <- death_year(d, "AGE RDETH CAL DETH CERT,90")
d["int90DeathYear"] <- death_year(d, "AGE RDETH CALC FR 90 INT AND DT BIRTH")
  

