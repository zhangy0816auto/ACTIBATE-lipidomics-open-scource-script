#!/usr/bin/env Rscript

# This script performs paired t-tests and generates volcano plots for lipidomics data.
# Usage: Rscript scripts/volcano_analysis.R <datafile>

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggrepel)
})

#' Load data in wide format
#' @param file CSV or Excel file with columns Sample, Group, <lipid columns>
#' @return long format tibble with columns Sample, Group, Lipid, Value
load_data <- function(file) {
  ext <- tools::file_ext(file)
  if (ext %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(file)
  } else {
    df <- readr::read_csv(file, show_col_types = FALSE)
  }

  df <- df %>% mutate(across(-c(Sample, Group), as.numeric))
  pivot_longer(df, -c(Sample, Group), names_to = "Lipid", values_to = "Value") %>%
    mutate(log2_value = log2(Value)) %>%
    drop_na()
}

#' Perform paired tests for each lipid
#' @param df long format tibble
#' @return tibble with statistics
run_tests <- function(df) {
  lipids <- unique(df$Lipid)
  results <- map_dfr(lipids, function(lipid) {
    tmp <- df %>% filter(Lipid == lipid)
    wil <- wilcox.test(log2_value ~ Group, tmp)$p.value
    tt  <- t.test(log2_value ~ Group, tmp, paired = TRUE)$p.value
    tibble(Lipid = lipid, p_wilcox = wil, p_ttest = tt)
  })
  results %>%
    mutate(wilcox_fdr = p.adjust(p_wilcox, method = "fdr"),
           t_fdr = p.adjust(p_ttest, method = "fdr"))
}

#' Create volcano plot
#' @param stats stats tibble from run_tests
#' @param outfile path to PNG output
create_volcano <- function(stats, outfile) {
  plt <- ggplot(stats, aes(x = log2(p_wilcox), y = -log10(p_ttest), label = Lipid)) +
    geom_point() +
    geom_text_repel(max.overlaps = 20) +
    theme_minimal()
  ggsave(outfile, plt, width = 6, height = 5)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    stop("Usage: Rscript scripts/volcano_analysis.R <datafile>")
  }
  data_file <- args[1]
  df <- load_data(data_file)
  stats <- run_tests(df)
  dir.create("results", showWarnings = FALSE)
  create_volcano(stats, file.path("results", "volcano.png"))
  write_csv(stats, file.path("results", "stats.csv"))
}

if (identical(environment(), globalenv())) {
  main()
}

