# install required packages
install.packages("ggplot2")
install.packages("FSA")
install.packages("qqplotr")
install.packages("DescTools")
install.packages("PMCMRplus")
install.packages("dunn.test")
install.packages("httr")
install.packages("openssl")
install.packages("benchmarkme")
install.packages("opdisDownsampling")
install.packages("qqconf")

# Load required libraries
library(qqplotr)
library(dplyr)
library(ggplot2)
library(dunn.test)

# Mic Data
# Perform Shapiro-Wilk normality test and summarize the results by group
qPCR_data_Mic %>%
  group_by(Filter) %>%
  summarise(
    W = shapiro.test(copy_number)$statistic,
    p.value = shapiro.test(copy_number)$p.value
  )

# Creat QQ Plot
ggplot(data = qPCR_data_Mic, mapping = aes(sample = CQ)) +
  stat_qq_band(alpha = 0.5, conf = 0.95, bandType = "pointwise") +
  stat_qq_line() +
  stat_qq_point(col = "black") +
  facet_wrap(~Filter, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        strip.text = element_text(size = 20))

ggsave("qq_plot_2.png", width = 10, height = 7, dpi = 300)


# Creat Violin Plot
palette <- c("dimgray", "darkgray", "gray", "lightgray")

ggplot(qPCR_data_Mic, aes(x = Filter, y = copy_number, fill = Filter)) +
  geom_violin(scale = "width", adjust = 1) +
  geom_jitter(width = 0.2, size = 1, color = "black") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 20, face = "bold"),
  ) +
  labs(y = "Copy Number per Microliter")

ggsave("Violin_2.png", width = 10, height = 7, dpi = 300)

# Perform Kruskal-Wallis test and posthoc Dunn's test
kruskal.test(copy_number ~ Filter, data = qPCR_data_Mic)

dunn_result <- dunn.test(qPCR_data_Mic$copy_number, g = qPCR_data_Mic$Filter, method = "bonferroni")

print(dunn_result)


# Quant Data
# Perform Shapiro-Wilk normality test and summarize the results by group
qPCR_data_Quant %>%
  group_by(Filter) %>%
  summarise(
    W = shapiro.test(copy_number)$statistic,
    p.value = shapiro.test(copy_number)$p.value
  )

# Create a QQ Plot
ggplot(data = qPCR_data_Quant, mapping = aes(sample = CQ)) +
  stat_qq_band(alpha = 0.5, conf = 0.95, bandType = "pointwise") +
  stat_qq_line() +
  stat_qq_point(col = "black") +
  facet_wrap(~Filter, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        strip.text = element_text(size = 20))

  ggsave("qq_plot_1.png", width = 10, height = 7, dpi = 300)


# Create Violin Plot
palette <- c("dimgray", "darkgray", "gray", "lightgray")

ggplot(qPCR_data_Quant, aes(x = Filter, y = copy_number, fill = Filter)) +
  geom_violin(scale = "width", adjust = 1) +
  geom_jitter(width = 0.2, size = 1, color = "black") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 20, face = "bold"),
  ) +
  labs(y = "Copy Number per Microliter")

ggsave("Violin_1.png", width = 10, height = 7, dpi = 300)

# Perform Kruskal-Wallis test and posthoc Dunn's test
kruskal.test(copy_number ~ Filter, data = qPCR_data_Quant)

dunn_result2 <- dunn.test(qPCR_data_Quant$copy_number, g = qPCR_data_Quant$Filter)

print(dunn_result2)


