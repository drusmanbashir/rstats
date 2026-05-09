#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

input_csv <- "/s/fran_storage/projects/kits23/logs/KITS23-SIRIG/case_recorder_compiled.csv"

raw_df <- read.csv(input_csv, stringsAsFactors = FALSE)

wide_df <- raw_df %>%
  distinct(stage, epoch, case_id, label, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = c(stage, epoch, case_id),
    names_from = label,
    values_from = loss_dice
  ) %>%
  arrange(stage, epoch, case_id)

print(head(wide_df, 20))
# fcat("Rows:", nrow(wide_df), "Columns:", ncol(wide_df), "\n")
# write_csv(wide_df, "case_recorder_wide.csv")
# %%
