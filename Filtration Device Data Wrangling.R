#Filtration Device Data Wrangling

# Load required packages
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl)

df <- read.csv("filtration_device_data.csv")

# Test
print(df)

df$River <- substr(df$Station, 1, 2)

df$Mean_Cq <- rowMeans(df[,3:6], na.rm = TRUE)

df <- df %>% filter(!Station %in% c("ET-002", "ET-004"))
IPC_df <- df %>% filter(Target == "IPC")

# Test
print(IPC_df)

IPC_df <- IPC_df %>% select(-c(1, 3:6))

IPC_df$Inhibited <- ifelse(IPC_df$Mean_Cq > 30 | IPC_df$Mean_Cq == 0, 1, 0)

IPC_df <- IPC_df %>% filter(!is.na(Instrument))

IPC_df <- IPC_df %>% filter(!grepl("NC", Lot))

IPC_Summary_df <- IPC_df %>%
  group_by(Station, Instrument) %>%
  summarise(n = n())

# Test
print(IPC_Summary_df)

IPC_df_subset <- IPC_df %>% select(Inhibited, Instrument, River, Station)

IPC_df_grouped <- IPC_df_subset %>% group_by(River, Station, Instrument)

IPC_summary_df <- IPC_df_grouped %>%
  summarise(inhibited = sum(Inhibited == 1, na.rm = TRUE),
            not_inhibited = sum(Inhibited == 0, na.rm = TRUE))

IPC_summary_df <- IPC_summary_df %>% select(inhibited, not_inhibited, instrument = Instrument, river = River, station = Station)

IPC_summary_inhibited <- IPC_summary_df %>%
  group_by(instrument) %>%
  summarise(sum_inhibited = sum(inhibited))

# Test
print(IPC_summary_inhibited)
print(IPC_summary_df)

Target_df <- df %>% filter(Target == "P.Mar cytb")
Target_df <- Target_df %>% select(-c(1, 3:6))

Target_df <- Target_df %>% filter(!is.na(Instrument))

NC_df <- Target_df %>% filter(grepl("NC", Lot))

print(NC_df)

NC_df$contamination <- ifelse(NC_df$Mean_Cq > 0, 1, 0)

NC_df$contamination <- ifelse(NC_df$contamination == 1, "contaminated", "not_contaminated")

NC_df_subset <- NC_df %>% select(contamination, Instrument, River, Station)

NC_df_grouped <- NC_df_subset %>% group_by(River, Station, Instrument)

NC_summary_df <- NC_df_grouped %>% 
    summarise(contaminated = sum(contamination == "contaminated", na.rm = TRUE),
                        not_contaminated = sum(contamination == "not_contaminated", na.rm = TRUE))

# Test
print(NC_summary_df)

NC_summary_contaminated <- NC_summary_df %>%
  group_by(Instrument) %>%
  summarise(sum_contaminated = sum(contaminated))

# Test
print(NC_summary_contaminated)

inhibited_stations <- IPC_summary_df %>%
  filter(inhibited > 0) %>%
  pull(station)

# Test
print(inhibited_stations)

contaminated_stations <- NC_summary_df %>%
  filter(contaminated > 0) %>%
  pull(Station)

# Test
print(contaminated_stations)

Target_df <- Target_df %>%
  filter(!Station %in% inhibited_stations)

Target_df <- Target_df %>%
  filter(!Station %in% contaminated_stations)

Target_df <- Target_df %>% filter(!grepl("NC", Lot))

Target_df <- Target_df %>% filter(!is.na(Instrument))

Target_df$detection <- ifelse(Target_df$Mean_Cq > 0, 1, 0)

Target_df_subset <- Target_df %>% select(Mean_Cq, Instrument, River, Station)

Target_df_grouped <- Target_df_subset %>% group_by(River, Station, Instrument)

Target_summary_df <- Target_df_grouped %>%
  summarise(detected = sum(Mean_Cq > 0, na.rm = TRUE),
            not_detected = sum(Mean_Cq == 0, na.rm = TRUE))

Target_summary_df <- Target_summary_df %>% select(detected, not_detected, instrument = Instrument, river = River, station = Station)

Target_summary_df <- Target_summary_df %>% filter(!station == "ET-002")

# Test
print(Target_summary_df)

Target_summary_detected <- Target_summary_df %>%
  group_by(instrument) %>%
  summarise(sum_detected = sum(detected))

# Test
print(Target_summary_detected)

summary_combined <- data.frame(
  Target_summary_detected,
  NC_summary_contaminated,
  IPC_summary_inhibited
)

summary_combined <- summary_combined %>% select(-c(3, 5))

summary_combined_long <- reshape2::melt(summary_combined, id.vars = "instrument")

# Test
print(summary_combined_long)

summary_combined_long$variable <- ifelse(summary_combined_long$variable == "sum_detected", "Detections",
					 ifelse(summary_combined_long$variable == "sum_contaminated", "Contamination Events",
                                                  ifelse(summary_combined_long$variable == "sum_inhibited", "Inhibited Field Samples", NA)))

ggplot(data = summary_combined_long, aes(x = instrument, y = value, fill = instrument)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  labs(x = NULL, y = "Count", fill = NULL, title = "Summary of Detected, Contaminated, and Inhibited Samples") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text = element_text(size = 24), axis.title = element_text(size = 24),
    text = element_text(size = 24)) +
  ggtitle(NULL) +
  guides(fill = FALSE) +
  theme(aspect.ratio = 3 / 2) +
  ylim(0, 30) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3)

ggsave("filtration_device_effect_size_graph.png", width = 10, height = 7, dpi = 300)
