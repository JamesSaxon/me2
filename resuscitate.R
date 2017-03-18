library(ggplot2)
library(plyr)

source("om_me2_mod.R")
source("simple_plot.R")

intyears <- c(1967, 1968, 1969, 1971, 1973, 1975, 1976, 1978, 1980, 1981, 1983)

zombie_interviews <- function(data) {

  cn <- colnames(data)
  fni <- cn[grep("fNonInt", cn)][1:11]
  
  for (a in seq_along(1:10)) {
  
    fniA <- fni[a]
    fniB <- fni[a+1]
  
    zombies <- (data[[fniA]] == "DECEASED") & (data[[fniB]] != "DECEASED")
    if (sum(zombies)) {
      print(sprintf("Zombies in %s to %s!!!", fniA, fniB))
      print(data[zombies,"IDENTIFICATION CODE, 66"])
    }
  }
}

certified_sawyers <- function(data) {

  ndiCount  <- numeric()
  ssCount   <- numeric()
  certCount <- numeric()
  intCount  <- numeric()

  cert9Count <- numeric()
  ndi_ssCount <- numeric()
  
  for (year in intyears) {
    
    var <- paste0("fNonInt_", year)
  
    dead_interview <- data[[var]] == "DECEASED"

    # Claim is that subjects are dead in interview year.
    # Are their matched death certificates dated more than two years later?
    # 
    ndiCount  <- c(ndiCount,  sum(dead_interview & data["ndiDeathYear"]   > year + 2, na.rm = T))
    ssCount   <- c(ssCount,   sum(dead_interview & data["ssDeathYear"]    > year + 2, na.rm = T))
    intCount  <- c(intCount,  sum(dead_interview & data["int90DeathYear"] > year + 2, na.rm = T))
    certCount <- c(certCount, sum(dead_interview & data["certDeathYear"]  > year + 2, na.rm = T))

    ndi_ssCount  <- c(ndi_ssCount, sum(dead_interview & data["ndiDeathYear"] > year + 2 & 
                                   abs(data["ndiDeathYear"] - data["ssDeathYear"]) < 2, na.rm = T))

    cert9Count   <- c(cert9Count,  sum(dead_interview & data["certDeathYear"] > year + 2 & 
                                   data["DETH CERT MATCH PROC,MTHDS USED MATCH,90"] == 1, na.rm = T))

  }

  sawyers <- data.frame(NDI = ndiCount, SS = ssCount, "NDI+SS" = ndi_ssCount,
                        Int = intCount, Cert = certCount, CertSSN = cert9Count,
                        row.names=intyears)
  
  sawyers 
}

ghost_table <- function(data) {
  
  ndiCount  <- numeric()
  ssCount   <- numeric()
  certCount <- numeric()
  intCount  <- numeric()
  
  for (year in intyears) {
    
    var <- paste0("fNonInt_", year)
  
    censored  <- data[[var]] == "Valid missing"
  
    ndiCount  <- c(ndiCount,  sum(censored & data["ndiDeathYear"]   < year, na.rm = T))
    ssCount   <- c(ssCount,   sum(censored & data["ssDeathYear"]    < year, na.rm = T))
    certCount <- c(certCount, sum(censored & data["certDeathYear"]  < year, na.rm = T))
    intCount  <- c(intCount,  sum(censored & data["int90DeathYear"] < year, na.rm = T))
  
  }
  
  ghosts <- data.frame(NDI = ndiCount, SS = ssCount, Cert = certCount, Int = intCount, row.names=intyears)
  
  ghosts

}


widow_table <- function(data) {

  widow <- rbind("NDI"   = table(data[data[["ndiDeathYear"]]   < 1988, "fResp_1990"]),
                 "SS"    = table(data[data[["ssDeathYear"]]    < 1988, "fResp_1990"]),
                 "Cert"  = table(data[data[["certDeathYear"]]  < 1988, "fResp_1990"]),
                 "Int90" = table(data[data[["int90DeathYear"]] < 1988, "fResp_1990"]))

  widow

}



