# rstats/scripts

Quick reference for reusable case-recorder analysis helpers in `case_recorder_analysis.R`.

## Main Functions

- `read_case_recorder_wide(path)`: Read wide CSV and validate required columns (`stage`, `epoch`, `case_id`).
- `detect_metric_columns(df, metric_regex = NULL, metric_cols = NULL)`: Pick metric columns by regex or explicit names.
- `summarize_case_trends(df, metric_cols/metric_regex, start_epoch = NULL, end_epoch = NULL, lower_is_better = TRUE)`: Build per-case start/end comparisons and per-metric `delta`.
- `rank_stubborn_cases(case_summary, score_col = "aggregate_delta", n = NULL, keep_nonnegative_only = FALSE)`: Rank least-improved cases (smallest delta first).
- `analyze_stubborn_cases(...)`: Wrapper that runs summary + default stubborn ranking.
- `print_top_stubborn_cases(ranked_cases, n = 10, score_col = "aggregate_delta", title = NULL)`: Print top-N table.

## Typical Use

```r
source("/home/ub/code/rstats/scripts/case_recorder_analysis.R")

df <- read_case_recorder_wide("/path/to/case_recorder_wide.csv") |>
  dplyr::filter(stage == "train", epoch <= 10)

res <- analyze_stubborn_cases(
  df = df,
  metric_cols = c("loss_dice_label2", "loss_dice_label3"),
  lower_is_better = TRUE
)

ranked <- rank_stubborn_cases(
  res$case_summary,
  score_col = "aggregate_delta",
  n = 20,
  keep_nonnegative_only = TRUE
)

print_top_stubborn_cases(ranked, n = 20, score_col = "aggregate_delta")
```
