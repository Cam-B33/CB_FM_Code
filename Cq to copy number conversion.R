# Cq to copy number conversion

# Install and load packages
install.packages("dplyr")
library(dplyr)
	

qPCR_data_Quant <- read.csv("big_otter_CQ_values.csv")
qPCR_data_Mic <- read.csv("credit_river_CQ_values.csv")

#Test
print(qPCR_data_Quant)
print(qPCR_data_Mic)
as.numeric(qPCR_data_Mic$CQ)
#R.squared	Slope	Intercept
	

# scQuant function
scQuant <- function(CQ) {
  if (CQ == 0) {
    return(0)
  } else {
    slope <- -3.38098380731628
    intercept <- 39.7852662212544
    logCopyNumber <- (CQ - intercept) / slope
    copyNumber <- 10^logCopyNumber
    return(copyNumber)
  }
}

# scMIC function
sc_mic <- function(CQ) {
  if (CQ == 0) {
    return(0)
  } else {
    slope <- -3.38098380731628
    intercept <- 39.7852662212544
    logCopyNumber <- (CQ - intercept) / slope
    copyNumber <- 10^logCopyNumber
    return(copyNumber)
  }
}

qPCR_data_Mic <- qPCR_data_Mic %>%
  mutate(copy_number = ifelse(CQ == 0, 0, sapply(CQ, sc_mic)))

print(qPCR_data_Mic)


qPCR_data_Quant <- qPCR_data_Quant %>%
  mutate(copy_number = sapply(CQ, scQuant))

print(qPCR_data_Quant)

qPCR_data_Mic <- qPCR_data_Mic %>%
  mutate(copy_number = sapply(CQ, sc_mic))

print(qPCR_data_Mic)

