
# 03_volcano_plot.R
# This script performs a paired t-test and plots a volcano graph
# comparing two conditions (e.g., PRE vs POST)

library(openxlsx)
library(tidyverse)
library(ggrepel)

# -------- Parameters --------
input_file <- "data/mock_input.xlsx"
output_plot <- "results/figures/volcano_plot.png"
output_table <- "results/tables/volcano_results.csv"

# -------- Load Data --------
df <- read.xlsx(input_file)

# Simulate PRE vs POST data based on existing groups
# Here we pretend that S1-S5 are PRE and S6-S10 are POST for illustration
df$Timepoint <- rep(c("PRE", "POST"), each = 5)

# -------- Prepare Long Format --------
lipid_cols <- grep("^Lipid_", names(df), value = TRUE)
df_long <- df %>%
  select(SampleID, Timepoint, all_of(lipid_cols)) %>%
  pivot_longer(cols = starts_with("Lipid_"), names_to = "Lipid", values_to = "Value")

# -------- Paired t-test --------
volcano_data <- df_long %>%
  group_by(Lipid) %>%
  summarise(
    log2FC = log2(mean(Value[Timepoint == "POST"]) / mean(Value[Timepoint == "PRE"])),
    pval = t.test(Value[Timepoint == "POST"], Value[Timepoint == "PRE"], paired = TRUE)$p.value
  ) %>%
  mutate(
    negLog10P = -log10(pval),
    sig = ifelse(pval < 0.05 & abs(log2FC) > 0.5, "Significant", "Not Significant")
  )

# -------- Plot Volcano --------
plot <- ggplot(volcano_data, aes(x = log2FC, y = negLog10P, color = sig)) +
  geom_point() +
  geom_text_repel(data = subset(volcano_data, sig == "Significant"),
                  aes(label = Lipid), size = 3, max.overlaps = 10) +
  scale_color_manual(values = c("gray", "red")) +
  labs(title = "Volcano Plot (POST vs PRE)",
       x = "Log2 Fold Change",
       y = "-Log10(p-value)") +
  theme_minimal()

# -------- Save Results --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

ggsave(output_plot, plot, width = 7, height = 5, dpi = 300)
write_csv(volcano_data, output_table)

message("Volcano plot created.")
