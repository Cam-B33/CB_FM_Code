output_file <- "experiment2_GLMM.txt"

## open the output file for writing
sink(output_file)

# GLMM for Filtration device comparison
# Load required packages
library(lme4)

#Analysis of IPC data

mydata <- IPC_summary_df

# Test
print(mydata)

## Fit GLMM with Binomial error distribution and river as a random effect.
model <- glmer(cbind(inhibited, not_inhibited) ~ instrument + (1 | river), data = mydata, family = binomial)

summary(model)

#Analysis of Contamination Data

mydata2 <- NC_summary_df

# Test
print(NC_summary_df)

## Fit GLMM with Binomial error distribution and river as a random effect.
model2 <- glmer(cbind(contaminated, not_contaminated) ~ Instrument + (1 | River), data = mydata2, family = binomial)

## View model summary
summary(model2)

# Analysis of Detection Data

mydata3 <- Target_summary_df

print(mydata3)

## Fit GLMM with Binomial error distribution and river as a random effect.
model3 <- glmer(cbind(detected, not_detected) ~ instrument + (1 | river), data = mydata3, family = binomial)

## View model summary
summary(model3)

# Sink output file.
sink()
