# %%
options(max.print=100)
options(width=250)
library(dplyr)
library(readr)
library(purrr)
library(purrr)
library(tidyr)
library(readr)

library(dplyr)
setwd("/home/ub/code/rstats/scripts/label_analysis")

# %%
#SECTION:-------------------- functions--------------------------------------------------------------------------------------
lesion_stats_one_case <- function(g) {
  gt_ids   <- unique(na.omit(g$gt_label_cc))
  pred_ids <- unique(na.omit(g$pred_label_cc))

  lesions_gt   <- length(gt_ids)
  lesions_pred <- length(pred_ids)

  # TP = GT component has at least one matched predicted centroid/component
  tp_gt_ids <- gt_ids[
    map_lgl(gt_ids, \(x) {
      any(!is.na(g$pred_cent[g$gt_label_cc == x]), na.rm = TRUE)
    })
  ]

  tp <- length(tp_gt_ids)
  fn <- lesions_gt - tp

  # FP = predicted component which never oveg[["gt_label_cc"]]rlaps any GT component in this case
  fp_pred_ids <- pred_ids[
    map_lgl(pred_ids, \(x) {
      all(is.na(g$gt_label_cc[g$pred_label_cc == x]))
    })
  ]

  fp <- length(fp_pred_ids)
  dsc_overall = first(g$dsc_overall)

  tibble(
    lesions_gt = lesions_gt,
    lesions_pred = lesions_pred,
    dsc= dsc_overall,
    tp = tp,
    fn = fn,
    fp = fp,

    precision = if_else(tp + fp > 0, tp / (tp + fp), NA_real_),
    recall = if_else(tp + fn > 0, tp / (tp + fn), NA_real_),
    f1 = if_else(
      !is.na(precision) & !is.na(recall) & precision + recall > 0,
      2 * precision * recall / (precision + recall),
      NA_real_
    ),
    # tp_gt_ids = list(tp_gt_ids),
    # fp_pred_ids = list(fp_pred_ids),
    # fn_gt_ids = list(setdiff(gt_ids, tp_gt_ids))
  )
}

lesion_stats_by_case <- function(df) {
  df %>%
    group_by(case_id) %>%
    group_modify(~ lesion_stats_one_case(.x)) %>%
    ungroup()
}
    
#%%
# %%
# folder <-"/t/fran_storage/predictions/lits/ensemble_LITS-408_LITS-385_LITS-383_LITS-357"

folder <- "/s/fran_storage/predictions/lits/LITS-ROOST/litq/results_bbox_union_test"
folder <- "/s/fran_storage/predictions/lits/LITS-ROOST/drli/results/"
file_name <- paste(folder, "results.csv", sep = "/")
# folder  <- "/s/fran_storage/predictions/lits/LITS-ROOST/drli/results_last"

options(pillar.sigfig = 2)
# file_name <- paste(folder, "drli_thresh1mm_all.csv",sep="/")
dfs_n <- file.path(file_name)

df <- read_csv(dfs_n, show_col_types = FALSE)
df <- read_csv(dfs_n, col_types = cols(label = col_character()), show_col_types = FALSE)

d3  <- lesion_stats_by_case(df)

d2  <- split(df,df$case_id)

# %%
# %%
# %%  functions
names(df)
# %%
# SECTION:-------------------- Case based DSC--------------------------------------------------------------------------------------
fn2<- paste(folder, "dices2.csv", sep = "/")
write.csv(d3, fn2, row.names = FALSE)


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
fn_long = "/home/ub/Downloads/ex2.csv"
df_long = read_csv(fn_long, show_col_types = FALSE)

wide <- df_long |> pivot_wider(id_cols = c(case_id, label), names_from = label, values_from = loss_dice)

# SECTION:-------------------- group by case_id--------------------------------------------------------------------------------------n
