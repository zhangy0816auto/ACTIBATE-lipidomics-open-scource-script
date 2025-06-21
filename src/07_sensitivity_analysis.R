
# 07_sensitivity_analysis.R
# This script performs PLS-DA to compare responders vs non-responders

library(openxlsx)
library(tidyverse)
library(mixOmics)

# -------- Parameters --------
input_file <- "data/mock_input.xlsx"
output_plot <- "results/figures/plsda_responder.png"

# -------- Load Data --------
df <- read.xlsx(input_file)
df$Responder_VO2peak <- as.factor(df$Responder_VO2peak)

# Extract lipid data and class label
lipid_cols <- grep("^Lipid_", names(df), value = TRUE)
X <- df[, lipid_cols]
Y <- df$Responder_VO2peak

# -------- Fit PLS-DA model --------
model <- plsda(X, Y, ncomp = 2)

# -------- Plot --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png(output_plot, width = 2200, height = 1500, res = 300, type = "cairo")

plotIndiv(model,
          ind.names = FALSE,
          legend = TRUE,
          ellipse = TRUE,
          title = "PLS-DA: Responder vs Non-Responder",
          col = c("grey", "lightskyblue"),
          pch = 17)

dev.off()

message("Responder PLS-DA plot saved.")
