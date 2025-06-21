
# 06_gender_foldchange_plot.R
# This script plots fold change comparison between females and males in MOD-EX group

library(openxlsx)
library(tidyverse)
library(ggrepel)

# -------- Parameters --------
input_file <- "data/gender_foldchange_mock.xlsx"
output_plot <- "results/figures/female_vs_male_fc.png"

# -------- Load Data --------
df <- read.xlsx(input_file)

# Merge male and female MOD_EX values for each lipid
df_combined <- inner_join(
  df %>% filter(Gender == "Men") %>% select(Lipids, Label, Men_Value = MOD_EX),
  df %>% filter(Gender == "Women") %>% select(Lipids, Women_Value = MOD_EX),
  by = "Lipids"
)

# Compute plot ranges
min_val <- floor(min(c(df_combined$Men_Value, df_combined$Women_Value), na.rm = TRUE))
max_val <- ceiling(max(c(df_combined$Men_Value, df_combined$Women_Value), na.rm = TRUE))

# Create dominant shading areas
female_dominant_area <- data.frame(
  x = c(min_val, min_val, max_val),
  y = c(min_val, max_val, max_val)
)

male_dominant_area <- data.frame(
  x = c(min_val, max_val, max_val),
  y = c(min_val, min_val, max_val)
)

# Select top 3 diff lipids per subclass
top3_lipids <- df_combined %>%
  mutate(diff = abs(Women_Value - Men_Value)) %>%
  group_by(Label) %>%
  slice_max(order_by = diff, n = 3, with_ties = FALSE) %>%
  ungroup()

# -------- Plot --------
plot <- ggplot(df_combined, aes(x = Men_Value, y = Women_Value, color = Label)) +
  geom_polygon(data = female_dominant_area, aes(x = x, y = y),
               fill = "plum", alpha = 0.15, inherit.aes = FALSE) +
  geom_polygon(data = male_dominant_area, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.15, inherit.aes = FALSE) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(data = top3_lipids, aes(label = Lipids), size = 3.5) +
  labs(title = "Fold Change Comparison: Female vs Male (MOD-EX)",
       x = "Male log2 Fold Change",
       y = "Female log2 Fold Change") +
  theme_minimal()

# -------- Save Plot --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(output_plot, plot, width = 6.5, height = 5.5, dpi = 300)

message("Sex difference plot saved.")
