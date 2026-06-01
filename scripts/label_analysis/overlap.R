# %%

library(purrr)
library(tidyr)
library(readr)

library(dplyr)
setwd("/home/ub/code/rstats/scripts/label_analysis")

options(pillar.sigfig = 2)
# %%
# folder <-"/t/fran_storage/predictions/lits/ensemble_LITS-408_LITS-385_LITS-383_LITS-357"

folder <- "/s/fran_storage/predictions/lits/LITS-ROOST/litq/results_bbox_union_test"
folder <- "/s/fran_storage/predictions/lits/LITS-ROOST/drli/results/"
file_name <- paste(folder, "results.csv", sep = "/")
# folder  <- "/s/fran_storage/predictions/lits/LITS-ROOST/drli/results_last"
# file_name <- paste(folder, "drli_thresh1mm_all.csv",sep="/")
dfs_n <- file.path(file_name)
df <- read_csv(dfs_n, col_types = cols(label = col_character()), show_col_types = FALSE)
# %%  functions
names(df)
# %%
# SECTION:-------------------- Case based DSC--------------------------------------------------------------------------------------
df_cases <- df %>%
  drop_na(gt_label_org) %>%
  distinct(case_id, dsc_overall, .keep_all = TRUE)

# %%
fn2<- paste(folder, "dices2.csv", sep = "/")
write.csv(df_cases, fn2, row.names = FALSE)

# %%
df_summ = df %>%
  group_by(case_id) %>%
  summarise(
    n_total = n(),
    gt_label_cc_count = sum(!is.na(gt_label_cc)),
    pred_label_cc_count = sum(!is.na(pred_label_cc)),
    gt_volume = sum(gt_volume_cc, na.rm=TRUE),
    pred_volume = sum(pred_volume_cc, na.rm=TRUE),
    dsc_overall = first(dsc_overall),
    med_dsc = round(median(dsc,na.rm=TRUE),2),
  )  %>% arrange(med_dsc)
# %%

fn_summ  <- paste(folder, "summary2.csv", sep = "/")
write.csv(df_summ, fn_summ, row.names = FALSE)
# %%
df2 = df %>%
  group_by(case_id) %>%
  summarise(n_all = n(),gt_lesions = sum(!is.na(gt_label_cc)), pred_lesions = sum(!is.na(pred_label_cc)), med_dsc = round(median(dsc,na.rum=TRUE), 2), dsc_overall = first(dsc))
# %%
# SECTION:-------------------- group by case_id--------------------------------------------------------------------------------------n
