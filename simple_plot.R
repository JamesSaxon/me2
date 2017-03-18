#!/usr/bin/env RScript 

library(ggplot2)

# For scale_y_log10() with density, see 
# http://stackoverflow.com/questions/29111741/geom-density-doesnt-fill-correctly-with-scale-y-log10#

# d$Race <- factor(d[["RACE, 66"]], levels=c(1.0,2.0,3.0), labels=c("White", "Black", "Other"))

d["NDI - SS"] <- d["NDI MATCHING - AGE OF DEATH, 2011"] - d["SS MATCHING - AGE OF DEATH, 2011"]
missing = ((d[["NDI MATCHING - AGE OF DEATH, 2011"]] < 0) | (d[["SS MATCHING - AGE OF DEATH, 2011"]] < 0))
d[missing,"NDI - SS"] = NA

## Just get the year of death distribution.
pndi_dy <- ggplot(d, aes(x=d[["ndiDeathYear"]])) + 
           geom_histogram(position="identity", binwidth = 5, alpha = 0.4) + xlim(c(1940, 2015)) + 
           labs(x = "Year of Death", y = "Observations (NDI Match)") + scale_y_log10() + theme_classic()

## First by NDI match
pndi <- ggplot(d, aes(x=d[["NDI MATCHING - AGE OF DEATH, 2011"]], fill=fRace, y = ..density..)) + 
        geom_histogram(position="identity", binwidth = 1, alpha = 0.4) + xlim(c(50, 95)) + 
        labs(x = "Age at Death [Years]", y = "Density", title = "NDI Matching", fill = "Race") + theme_classic()

## Now by SS matching.
pss <- ggplot(d, aes(x=d[["SS MATCHING - AGE OF DEATH, 2011"]], fill=fRace, y = ..density..)) + 
       geom_histogram(position="identity", binwidth = 1, alpha = 0.4) + xlim(c(50, 95)) +
       labs(x = "Age at Death [Years]", y = "Density", title = "SS Matching", fill = "Race") + theme_classic()

## Difference
pdiff <- ggplot(d, aes(x=d[["NDI - SS"]], fill=fRace)) + 
         geom_histogram(position="identity", binwidth = 5, alpha = 0.4) +
         xlim(c(-50, 50)) + ylim(c(0, 8)) + 
         labs(x = "NDI - SS Offset [Years]", y = "Observations", fill = "Race") + theme_classic()

## From Death Certificate
pcert <- ggplot(d, aes(x=d[["AGE RDETH CAL DETH CERT,90"]], fill=fRace, y = ..density..)) + 
         geom_histogram(position="identity", binwidth = 1, alpha = 0.4) + xlim(c(50, 95)) +
         labs(x = "Age at Death [Years]", y = "Density", title = "Death Certificate in 1990", fill = "Race") + theme_classic()

## 1990 Interview
pint90 <- ggplot(d, aes(x=d[["AGE RDETH CALC FR 90 INT AND DT BIRTH"]], fill=fRace, y = ..density..)) + 
          geom_histogram(position="identity", binwidth = 1, alpha = 0.4) + xlim(c(50, 95)) +
          labs(x = "Age at Death [Years]", y = "Density", title = "Interview in 1990", fill = "Race") + theme_classic()


life_exp_by_var <- function(data, title, year, var, save = T) {

  print(var)

  mask <- complete.cases(d[c("ndiDeathYear", var)])
# mask <- mask & (data[["ndiDeathYear"]] > year)
  subset <- data[mask, c("NDI MATCHING - AGE OF DEATH, 2011", var)]

  breaks <- c(seq(50, 85, 5), 89)

  p <- ggplot(subset, aes(x=subset[["NDI MATCHING - AGE OF DEATH, 2011"]], group=subset[[var]])) + # , color=subset[[var]], y = ..density..)) + 
       # geom_histogram(size = 1, position="identity", aes(fill=subset[[var]], y = ..density.., show.legend = F), breaks = breaks, show.legend = F, alpha = 0.2) + # xlim(c(50, 95)) +
       geom_density(size = 1, position="identity", aes(color=subset[[var]], y = ..density..), alpha = 0) + xlim(c(50, 95)) +
       labs(x = "Age at Death [Years]", y = "Density", color = sprintf("%s\n(%d)", title, year)) +
       theme_classic()

  if (save) { ggsave(paste0(var, ".pdf"), height = 2.5, width = 6, units = "in") }

  p

}

potential_predictors <- function(data) {

  life_exp_by_var(data, "BMI", 1976, "fBMI_1973")
  life_exp_by_var(data, "Race", 1966, "fRace")
  life_exp_by_var(data, "Home Language", 1971, "fHomeLanguage_1971")
  life_exp_by_var(data, "Schooling", 1966, "fSchooling")


  for (yr in c(1966, 1967, 1969, 1971, 1973, 1975, 1976, 1978, 1980, 1981, 1990)) {
    life_exp_by_var(data, "Total Income", yr, paste0("fTotalInc_", yr))
  }

  for (yr in c(1967, 1969, 1971, 1976, 1981, 1983, 1990)) {
    life_exp_by_var(data, "Wages &\nSalary", yr, paste0("fWages_", yr))
  }

  for (ri in 1:nrow(marstat_nom_vars)) {
    year <- marstat_nom_vars[ri, "Year"]
    life_exp_by_var(data, "Spouse\nPresent", year, paste0("fSpousePresent_", year))
  }

  life_exp_by_var(data, "Smoking", 1990, "fSmoking_1990")
  life_exp_by_var(data, "Heavy\nAlcohol", 1990, "fHeavyAlcohol_1990")  


  for (cond in c("Pain", "Tiring", "Weakness", "Aches", "Fainting", "Tension", "Breath")) {
    for (year in c(1976, 1981, 1990)) {
      life_exp_by_var(data, paste0("Condition:\n", cond), year, sprintf("fCondition%s_%d", cond, year))
    }
  }


  for (vi in 1:nrow(health_limits)) {

    activity <- health_limits[vi,"activities"]
    for (yr in c(1971, 1976, 1981)) {

      var  <- sprintf("fHealthLimits%s_%d", activity, yr)
      life_exp_by_var(data, paste0("Limited\n", activity), yr, sprintf("fHealthLimits%s_%d", activity, yr))
    }
  }


}



