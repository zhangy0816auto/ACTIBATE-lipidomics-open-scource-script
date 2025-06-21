
# 04_plsda.R
# This script performs PLS-DA on lipidomics data grouped by condition

library(openxlsx)
library(tidyverse)
library(mixOmics)

# -------- Parameters --------
input_file <- "data/mock_input.xlsx"
output_plot <- "results/figures/plsda_plot.png"

# -------- Load Data --------
df <- read.xlsx(input_file)

# Extract features and labels
lipid_cols <- grep("^Lipid_", names(df), value = TRUE)
X <- df[, lipid_cols]
Y <- as.factor(df$Group)

# -------- Fit PLS-DA model --------
model <- plsda(X, Y, ncomp = 2)

# -------- Plot --------
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
png(output_plot, width = 2000, height = 1600, res = 300, type = "cairo")

plotIndiv(model, 
          ind.names = FALSE, 
          legend = TRUE,
          ellipse = TRUE,
          title = "PLS-DA Plot",
          col = c("orange", "lightskyblue"),
          pch = 16)

dev.off()

message("PLS-DA plot saved.")
