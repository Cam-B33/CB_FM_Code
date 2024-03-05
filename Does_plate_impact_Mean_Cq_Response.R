# Is inter qPCR run significant?
# Load Packages
library(ggplot2)
library(tidyverse)

#Internal Positive Control Data
IPC_PMAR <- as_tibble(Mean_Cq_for_DIY_vs_OSMOS)

IPC_PMAR <-IPC_PMAR %>% filter(Target == "IPC")
IPC_PMAR$Mean_Cq <-as.numeric(IPC_PMAR$Mean_Cq)

print(IPC_PMAR)

IPC_PMAR$Plate <- as.factor(IPC_PMAR$Plate)

# Mean Cq
plate_IPC_Response <- aov(Mean_Cq ~ Plate, data=IPC_PMAR)
anova(plate_IPC_Response)

plot <- ggplot(IPC_PMAR, aes(Plate, Mean_Cq, group = Plate)) +
  geom_point() +
  geom_line() +
  labs(x = "Analysis Plate",
       y = "Mean IPC Cq Value")
ggsave(filename = "MeanIPCCq.png", plot = plot, width = 6, height = 4, dpi = 300)

# CV Cq
plate_IPC_Variation <- aov(CV ~ Plate, data=IPC_PMAR)

anova(plate_IPC_Variation)

ggplot(IPC_PMAR, aes(Plate, CV, group = Plate)) +
  geom_point() +
  geom_line() +
  labs(x = "Analysis Plate",
       y = "Coefficient of Variation for IPC Cq")
ggsave(filename = "CVIPCCq.png", plot = plot, width = 6, height = 4, dpi = 300)

#Target Assay Data
CytB_PMAR <- as_tibble(Mean_Cq_for_DIY_vs_OSMOS)

CytB_PMAR <-CytB_PMAR %>% filter(Target == "P.Mar cytb")
CytB_PMAR$Mean_Cq <-as.numeric(CytB_PMAR$Mean_Cq)

print(CytB_PMAR)

CytB_PMAR$Plate <- as.factor(CytB_PMAR$Plate)

# Mean Cq
plate_Target_Response <- aov(Mean_Cq ~ Plate, data=CytB_PMAR)

anova(plate_Target_Response)

ggplot(CytB_PMAR, aes(Plate, Mean_Cq, group = Plate)) +
  geom_point() +
  geom_line() +
  labs(x = "Analysis Plate",
       y = "Mean Target Cq Value")
ggsave(filename = "MeanTargetCq.png", plot = plot, width = 6, height = 4, dpi = 300)
# CV Cq
plate_Target_Variation <- aov(CV ~ Plate, data=CytB_PMAR)

anova(plate_Target_Variation)

ggplot(CytB_PMAR, aes(Plate, CV, group = Plate)) +
  geom_point() +
  geom_line() +
  labs(x = "Analysis Plate",
       y = "Coefficient of Variation for Target Cq")
ggsave(filename = "CVTargetCq.png", plot = plot, width = 6, height = 4, dpi = 300)
