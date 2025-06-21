
# 05_bubble_plot.R
# This script creates a bubble plot of lipid features by carbon and unsaturation

library(openxlsx)
library(tidyverse)
library(ggplot2)
library(viridis)

# -------- Parameters --------
input_file <- "data/bubble_mock_data.xlsx"
output_plot <- "results/figures/bubble_plot.png"

# -------- Load Data --------
df <- read.xlsx(input_file)

# -------- Plot Bubble --------
plot <- ggplot(df, aes(x = Number_unsaturations, y = Number_carbons)) +
  geom_point(aes(size = count, color = -log10(pvalue)), alpha = 0.8) +
  scale_size_continuous(name = "Lipid Count", range = c(3, 10)) +
  scale_color_gradient(low = "#ffe3ff", high = "#650a6d", name = "-log10(p-value)") +
  labs(title = "Lipid Structure Bubble Plot",
       x = "Number of Unsaturations",
       y = "Number of Carbons") +
  theme_minimal() +
  theme(legend.position = "right")

# -------- Save Plot --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(output_plot, plot, width = 6, height = 5, dpi = 300)

message("Bubble plot saved.")
