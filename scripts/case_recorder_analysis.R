# Reusable helpers for case-recorder-wide CSVs.

required_case_recorder_columns <- function() {
  c("stage", "epoch", "case_id")
}

validate_case_recorder_schema <- function(df,
                                          required_cols = required_case_recorder_columns(),
                                          metric_cols = NULL) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(names(df)) > 0) {
    stop("Input data frame has duplicated column names.", call. = FALSE)
  }

  epoch_numeric <- suppressWarnings(as.numeric(df$epoch))
  if (any(is.na(epoch_numeric) & !is.na(df$epoch))) {
    stop("Column `epoch` must be numeric or coercible to numeric.", call. = FALSE)
  }

  if (!is.null(metric_cols)) {
    missing_metric_cols <- setdiff(metric_cols, names(df))
    if (length(missing_metric_cols) > 0) {
      stop(
        sprintf(
          "Missing metric columns: %s",
          paste(missing_metric_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

read_case_recorder_wide <- function(path,
                                    required_cols = required_case_recorder_columns()) {
  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  validate_case_recorder_schema(df, required_cols = required_cols)

  non_id_cols <- setdiff(names(df), c("stage", "case_id"))

  df$stage <- as.character(df$stage)
  df$case_id <- as.character(df$case_id)
  df[non_id_cols] <- lapply(df[non_id_cols], function(x) suppressWarnings(as.numeric(x)))

  validate_case_recorder_schema(df, required_cols = required_cols)
  df
}

detect_metric_columns <- function(df,
                                  metric_regex = NULL,
                                  metric_cols = NULL,
                                  exclude_cols = required_case_recorder_columns()) {
  validate_case_recorder_schema(df)

  if (!is.null(metric_cols)) {
    validate_case_recorder_schema(df, metric_cols = metric_cols)
    return(unique(metric_cols))
  }

  candidate_cols <- setdiff(names(df), exclude_cols)
  if (is.null(metric_regex)) {
    stop("Provide either `metric_cols` or `metric_regex`.", call. = FALSE)
  }

  matched <- grep(metric_regex, candidate_cols, value = TRUE)
  if (length(matched) == 0) {
    stop(
      sprintf("No metric columns matched regex `%s`.", metric_regex),
      call. = FALSE
    )
  }

  matched
}

pick_case_boundary_rows <- function(df, start_epoch = NULL, end_epoch = NULL) {
  validate_case_recorder_schema(df)

  if (!is.null(start_epoch) && !is.null(end_epoch) && start_epoch > end_epoch) {
    stop("`start_epoch` must be <= `end_epoch`.", call. = FALSE)
  }

  start_source <- df
  end_source <- df

  if (!is.null(start_epoch)) {
    start_source <- dplyr::filter(start_source, epoch >= start_epoch)
  }
  if (!is.null(end_epoch)) {
    end_source <- dplyr::filter(end_source, epoch <= end_epoch)
  }

  start_rows <- start_source |>
    dplyr::group_by(case_id) |>
    dplyr::slice_min(order_by = epoch, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(start_epoch_used = epoch)

  end_rows <- end_source |>
    dplyr::group_by(case_id) |>
    dplyr::slice_max(order_by = epoch, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(end_epoch_used = epoch)

  dplyr::inner_join(
    start_rows,
    end_rows |> dplyr::select(case_id, end_epoch_used),
    by = "case_id"
  )
}

summarize_case_trends <- function(df,
                                  metric_cols = NULL,
                                  metric_regex = NULL,
                                  start_epoch = NULL,
                                  end_epoch = NULL,
                                  lower_is_better = TRUE) {
  validate_case_recorder_schema(df)
  selected_metrics <- detect_metric_columns(
    df = df,
    metric_regex = metric_regex,
    metric_cols = metric_cols
  )

  boundary_rows <- pick_case_boundary_rows(
    df = df,
    start_epoch = start_epoch,
    end_epoch = end_epoch
  )

  if (nrow(boundary_rows) == 0) {
    stop("No cases remain after applying the requested epoch bounds.", call. = FALSE)
  }

  start_long <- boundary_rows |>
    dplyr::select(case_id, start_epoch_used, dplyr::all_of(selected_metrics)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_metrics),
      names_to = "metric",
      values_to = "start_value"
    )

  end_long <- df |>
    dplyr::semi_join(boundary_rows |> dplyr::select(case_id, end_epoch_used),
                     by = c("case_id", "epoch" = "end_epoch_used")) |>
    dplyr::rename(end_epoch_used = epoch) |>
    dplyr::select(case_id, end_epoch_used, dplyr::all_of(selected_metrics)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_metrics),
      names_to = "metric",
      values_to = "end_value"
    )

  metric_summary <- start_long |>
    dplyr::inner_join(end_long, by = c("case_id", "metric")) |>
    dplyr::mutate(
      raw_change = end_value - start_value,
      delta = if (lower_is_better) start_value - end_value else end_value - start_value,
      improvement = delta,
      direction = dplyr::case_when(
        is.na(delta) ~ "missing",
        delta > 0 ~ "improved",
        delta < 0 ~ "worsened",
        TRUE ~ "flat"
      )
    ) |>
    dplyr::arrange(delta, case_id, metric)

  case_summary <- metric_summary |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(
      start_epoch_used = dplyr::first(start_epoch_used),
      end_epoch_used = dplyr::first(end_epoch_used),
      aggregate_delta = if (all(is.na(delta))) NA_real_ else mean(delta, na.rm = TRUE),
      aggregate_improvement = aggregate_delta,
      metrics_with_values = sum(!is.na(delta)),
      improved_metrics = sum(delta > 0, na.rm = TRUE),
      worsened_metrics = sum(delta < 0, na.rm = TRUE),
      .groups = "drop"
    )

  delta_wide <- metric_summary |>
    dplyr::select(case_id, metric, delta) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = delta,
      names_prefix = "delta_"
    )

  start_wide <- metric_summary |>
    dplyr::select(case_id, metric, start_value) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = start_value,
      names_prefix = "start_"
    )

  end_wide <- metric_summary |>
    dplyr::select(case_id, metric, end_value) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = end_value,
      names_prefix = "end_"
    )

  case_summary <- case_summary |>
    dplyr::left_join(start_wide, by = "case_id") |>
    dplyr::left_join(end_wide, by = "case_id") |>
    dplyr::left_join(delta_wide, by = "case_id") |>
    dplyr::arrange(aggregate_delta, case_id)

  list(
    metric_cols = selected_metrics,
    metric_summary = metric_summary,
    case_summary = case_summary
  )
}

rank_stubborn_cases <- function(case_summary,
                                score_col = "aggregate_delta",
                                n = NULL,
                                keep_nonnegative_only = FALSE,
                                start_dice_threshold = NULL,
                                sort_digits = 6) {
  if (!score_col %in% names(case_summary)) {
    stop(sprintf("Column `%s` is not present in `case_summary`.", score_col), call. = FALSE)
  }

  ranked <- case_summary
  if (!is.null(start_dice_threshold)) {
    if (!is.numeric(start_dice_threshold) || length(start_dice_threshold) != 1 ||
        is.na(start_dice_threshold) || start_dice_threshold < 0 || start_dice_threshold > 1) {
      stop("`start_dice_threshold` must be a single numeric value in [0, 1].", call. = FALSE)
    }

    start_loss_cols <- grep("^start_", names(ranked), value = TRUE)
    if (length(start_loss_cols) == 0) {
      stop("No `start_*` columns found; cannot apply start dice threshold.", call. = FALSE)
    }

    start_dice_mat <- as.matrix(ranked[, start_loss_cols, drop = FALSE])
    keep_start <- apply(
      start_dice_mat,
      1,
      function(x) all(is.na(x) | x >= start_dice_threshold)
    )
    ranked <- ranked[keep_start, , drop = FALSE]
  }

  if (keep_nonnegative_only) {
    ranked <- ranked[is.na(ranked[[score_col]]) | ranked[[score_col]] >= 0, , drop = FALSE]
  }

  score_for_sort <- round(ranked[[score_col]], digits = sort_digits)
  ranked <- ranked[order(score_for_sort, ranked$case_id, na.last = TRUE), , drop = FALSE]

  if (!is.null(n)) {
    ranked <- utils::head(ranked, n)
  }

  ranked
}

analyze_stubborn_cases <- function(df,
                                   metric_cols = NULL,
                                   metric_regex = NULL,
                                   start_epoch = NULL,
                                   end_epoch = NULL,
                                   lower_is_better = TRUE) {
  trend_summary <- summarize_case_trends(
    df = df,
    metric_cols = metric_cols,
    metric_regex = metric_regex,
    start_epoch = start_epoch,
    end_epoch = end_epoch,
    lower_is_better = lower_is_better
  )

  ranked_cases <- rank_stubborn_cases(trend_summary$case_summary)

  c(trend_summary, list(ranked_cases = ranked_cases))
}

print_top_stubborn_cases <- function(ranked_cases,
                                     n = 10,
                                     score_col = "aggregate_delta",
                                     title = NULL) {
  if (!is.null(title)) {
    cat("\n", title, "\n", sep = "")
  }

  display_cols <- c(
    "case_id",
    "start_epoch_used",
    "end_epoch_used",
    score_col,
    grep("^delta_", names(ranked_cases), value = TRUE)
  )
  display_cols <- unique(display_cols[display_cols %in% names(ranked_cases)])

  print(
    ranked_cases |>
      dplyr::select(dplyr::all_of(display_cols)) |>
      dplyr::slice_head(n = n),
    n = n
  )

  invisible(ranked_cases |>
              dplyr::select(dplyr::all_of(display_cols)) |>
              dplyr::slice_head(n = n))
}

format_block_worst_table <- function(case_summary,
                                     top_n = 10,
                                     digits = 6,
                                     case_prefix = "kits23_") {
  required_cols <- c(
    "case_id",
    "start_epoch_used",
    "end_epoch_used",
    "delta_loss_dice_label2",
    "delta_loss_dice_label3",
    "aggregate_delta",
    "end_loss_dice_label2",
    "end_loss_dice_label3"
  )
  missing_cols <- setdiff(required_cols, names(case_summary))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing columns for block table: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  out <- rank_stubborn_cases(
    case_summary = case_summary,
    score_col = "aggregate_delta",
    n = top_n,
    sort_digits = digits
  ) |>
    dplyr::transmute(
      case_id = sub(paste0("^", case_prefix), "", case_id),
      used_epochs = paste0(start_epoch_used, "->", end_epoch_used),
      delta_l2 = round(delta_loss_dice_label2, digits),
      delta_l3 = round(delta_loss_dice_label3, digits),
      agg_delta = round(aggregate_delta, digits),
      end_dice_l2 = round(end_loss_dice_label2, digits),
      end_dice_l3 = round(end_loss_dice_label3, digits)
    )

  out
}


# Scratch region for line-by-line execution on specific files.

input_path <- "/home/ub/code/projects/arcnet/r/case_recorder_wide.csv"

case_recorder_df <- read_case_recorder_wide(input_path)

train_epoch_le_10 <- case_recorder_df |>
  dplyr::filter(stage == "train", epoch <= 10)

label2_results <- analyze_stubborn_cases(
  df = train_epoch_le_10,
  metric_cols = "loss_dice_label2",
  lower_is_better = TRUE
)

label3_results <- analyze_stubborn_cases(
  df = train_epoch_le_10,
  metric_cols = "loss_dice_label3",
  lower_is_better = TRUE
)

label23_results <- analyze_stubborn_cases(
  df = train_epoch_le_10,
  metric_cols = c("loss_dice_label2", "loss_dice_label3"),
  lower_is_better = TRUE
)

label2_stubborn <- rank_stubborn_cases(
  label2_results$case_summary,
  n = 15,
  start_dice_threshold = 0.05
)
label3_stubborn <- rank_stubborn_cases(
  label3_results$case_summary,
  n = 15,
  start_dice_threshold = 0.05
)
label23_stubborn <- rank_stubborn_cases(
  label23_results$case_summary,
  n = 15,
  start_dice_threshold = 0.05
)

label2_monotonic_violations <- label2_results$case_summary |>
  dplyr::filter(aggregate_delta < 0) |>
  dplyr::arrange(aggregate_delta, case_id)
label3_monotonic_violations <- label3_results$case_summary |>
  dplyr::filter(aggregate_delta < 0) |>
  dplyr::arrange(aggregate_delta, case_id)
label23_monotonic_violations <- label23_results$case_summary |>
  dplyr::filter(aggregate_delta < 0) |>
  dplyr::arrange(aggregate_delta, case_id)

label2_stubborn_monotonic <- rank_stubborn_cases(
  label2_results$case_summary,
  n = 15,
  keep_nonnegative_only = TRUE,
  start_dice_threshold = 0.05
)
label3_stubborn_monotonic <- rank_stubborn_cases(
  label3_results$case_summary,
  n = 15,
  keep_nonnegative_only = TRUE,
  start_dice_threshold = 0.05
)
label23_stubborn_monotonic <- rank_stubborn_cases(
  label23_results$case_summary,
  n = 15,
  keep_nonnegative_only = TRUE,
  start_dice_threshold = 0.05
)

print_top_stubborn_cases(
  label2_stubborn,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 stubborn training cases for label 2 (epochs <= 10)"
)

print_top_stubborn_cases(
  label3_stubborn,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 stubborn training cases for label 3 (epochs <= 10)"
)

print_top_stubborn_cases(
  label23_stubborn,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 stubborn training cases for labels 2 + 3 aggregate (epochs <= 10)"
)

print_top_stubborn_cases(
  label2_stubborn_monotonic,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 monotonic-safe stubborn training cases for label 2 (delta >= 0)"
)

print_top_stubborn_cases(
  label3_stubborn_monotonic,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 monotonic-safe stubborn training cases for label 3 (delta >= 0)"
)

print_top_stubborn_cases(
  label23_stubborn_monotonic,
  n = 15,
  score_col = "aggregate_delta",
  title = "Top 15 monotonic-safe stubborn training cases for labels 2 + 3 (delta >= 0)"
)

cat("\nMonotonicity violations (delta < 0):\n")
cat("label2:", nrow(label2_monotonic_violations), "\n")
cat("label3:", nrow(label3_monotonic_violations), "\n")
cat("label2+3 aggregate:", nrow(label23_monotonic_violations), "\n")
