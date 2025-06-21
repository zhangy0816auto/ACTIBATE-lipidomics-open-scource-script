
# 01_zscore_correlation.R
# This script performs z-score normalization and correlation analysis
# between lipidomics features and VO2peak changes

library(openxlsx)
library(tidyverse)
library(Hmisc)
library(ggplot2)

# -------- Parameters --------
input_file <- "data/mock_input.xlsx"
output_plot <- "results/figures/zscore_vs_vo2peak.png"
output_zscore <- "results/tables/zscore_data.xlsx"

# -------- Load Data --------
df <- read.xlsx(input_file)

# -------- Z-score normalization --------
lipid_cols <- grep("^Lipid_", names(df), value = TRUE)
df_z <- df %>%
  mutate(across(all_of(lipid_cols), scale))  # standardize each lipid

# -------- Example correlation analysis --------
# Choose one lipid feature and correlate with VO2peak response
df_z <- df_z %>% mutate(Lipid_1 = as.numeric(Lipid_1))  # ensure numeric
res <- rcorr(df_z$Lipid_1, df_z$Responder_VO2peak)
r_val <- round(res$r[1, 2], 2)
p_val <- signif(res$P[1, 2], 2)

# -------- Scatter plot --------
plot <- ggplot(df_z, aes(x = Lipid_1, y = Responder_VO2peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("text", x = min(df_z$Lipid_1, na.rm = TRUE),
           y = max(df_z$Responder_VO2peak, na.rm = TRUE),
           label = paste0("r = ", r_val, ", p = ", p_val),
           hjust = 0, size = 5) +
  labs(title = "Correlation between Lipid_1 and VO2peak Response",
       x = "Lipid_1 (z-score)", y = "Responder VO2peak") +
  theme_minimal()

# -------- Save results --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

ggsave(output_plot, plot, width = 6, height = 4, dpi = 300)
write.xlsx(df_z, output_zscore, rowNames = FALSE)

message("Z-score correlation analysis complete.")
