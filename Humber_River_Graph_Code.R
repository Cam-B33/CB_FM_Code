#Humber River Graph Code

# Load Packages
library(readxl)
library(dplyr)
library(ggplot2)

HumbereDNA <- read.csv(Graph_Humber_River.csv)

# Assume your data frame is named "YourDataFrame"
HumbereDNA <- HumbereDNA %>% arrange(Date)

# Test
print(HumbereDNA)

ggplot(data = HumbereDNA, aes(x = Date, y = proportion, group = 1)) +
  geom_line() +
  scale_fill_gradient(low = "white", high = "gray") +
  labs(x = "Date", y = "Proportion of Amplifying Replicates", vjust = 2.5, hjust = 1.5) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20), axis.title = element_text(size = 20, margin = margin(t = 5))) +
  guides(fill = FALSE) +
  theme(aspect.ratio = 2 / 3) +
  ggtitle(NULL)

ggsave("Humber_river_graph.png", width = 10, height = 7, dpi = 300)
