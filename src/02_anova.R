
# 02_anova.R
# This script performs one-way ANOVA on lipid features by group

library(openxlsx)
library(tidyverse)
library(broom)

# -------- Parameters --------
input_file <- "data/mock_input.xlsx"
output_file <- "results/tables/anova_pvalues.csv"

# -------- Load Data --------
df <- read.xlsx(input_file)

# Replace 0 with small value to avoid log problems (if applicable)
lipid_cols <- grep("^Lipid_", names(df), value = TRUE)
df <- df %>%
  mutate(across(all_of(lipid_cols), ~ replace(., . == 0, 0.0001)))

# -------- Run ANOVA --------
anova_results <- map_dfr(
  lipid_cols,
  function(colname) {
    model <- lm(as.formula(paste0(colname, " ~ Group")), data = df)
    tidy(aov(model)) %>%
      filter(term == "Group") %>%
      mutate(Lipid = colname) %>%
      select(Lipid, everything())
  }
)

# -------- Save Results --------
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(anova_results, output_file)

message("ANOVA completed for ", length(lipid_cols), " lipids.")
