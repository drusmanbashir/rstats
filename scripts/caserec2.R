
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

library(dplyr)
setwd("/home/ub/code/label_analysis/label_analysis/rstats")

options(pillar.sigfig = 2)
# %%
# folder <-"/t/fran_storage/predictions/lits/ensemble_LITS-408_LITS-385_LITS-383_LITS-357"

folder <- "/s/tmp"
file_name <- paste(folder, "valid_cases2.csv", sep = "/")
dfs_n <- file.path(file_name)
df <- read_csv(dfs_n, col_types = cols(label = col_character()), show_col_types = FALSE)
# %%  functions
# %%
#SECTION:-------------------- wide format--------------------------------------------------------------------------------------
print(names(df))
df  %>% pivot_wider(id_cols =  case_id, names_from = label, values_from=loss_dice)

# %%
wide_summary <- df %>%
  group_by(case_id, label) %>%
  summarise(
    median_loss = median(loss_dice, na.rm = TRUE),
    mean_loss   = mean(loss_dice, na.rm = TRUE),
    n           = n(),
    .groups = "drop"
  )


out_fn = paste(folder, "wide_summary2.csv", sep = "/")

write.csv(wide_summary, out_fn, row.names = FALSE)
# %%
