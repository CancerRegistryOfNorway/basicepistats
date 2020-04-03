




#' @title Prevalence
#' @description
#' Compute subject prevalence at specific time points and optionally
#' conditioning on time since event at the time points.
#' @param x `[data.table]` (mandatory, no default)
#'
#' dataset containing one or more records by subject
#' @template arg_by
#' @param subject_id_col_nm `[character, NULL]` (optional, default `NULL`)
#'
#' -`character`: name of column in `x` which identifies subjects; one subject
#'  may have one or more rows in `x`
#' -`NULL`: `subject_id_col_nm` is created on-the-fly with the assumption
#'  that each record in `x` is a subject
#' @param follow_up_time_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` which specifies values in the time scale in which
#' prevalence follow-up windows are determined (e.g. prevalence of subjects
#' with event within 1, 5, 10 years)
#' @param follow_up_time_window_widths `[numeric]` (mandatory, default `Inf`)
#'
#' widhts of windowss in time scale given in `follow_up_time_col_nm`;
#' e.g. `c(1, 5, 10, Inf)` for windows of width 1, 5, 10 and window containing
#' all subjects
#' @param stratum_col_nms `[character, NULL]` (optional, default `NULL`)
#'
#' passed to [stat_count].
#' - `character`: prevalence counts are stratified by these stratifying columns
#' - `NULL`: no stratification
#' @template arg_subset
#' @template arg_subset_style
stat_prevalent_subject_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  subject_id_col_nm = NULL,
  by = NULL,
  subset = NULL,
  subset_style = "zero",
  observation_time_points
) {
  # split x at prevalence_time_points and count number of subjects identified
  # by subject_id_col_nm

  dt <- data.table::setDT(mget(names(x), as.environment(x)))
  data.table::setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  data.table::setattr(x, "time.scales", c(follow_up_time_col_nm, names(observation_time_points)))

  dt <- popEpi::splitMulti(
    data = dt,
    breaks = observation_time_points,
  )
  subset <- handle_subset_arg(dataset = x)
  by <- handle_by_arg(dataset = dt, subset = subset, by = by)

  fut_window_breaks <- sort(union(0, follow_up_time_window_widths))
  fut_window_labels <- paste0(
    "[", fut_window_breaks[-length(fut_window_breaks)],
    ", ",
    fut_window_breaks[-1],
    ")"
  )
  data.table::set(
    dt,
    j = ".__fut_window",
    value = cut(
      x = dt[[follow_up_time_col_nm]],
      breaks = fut_window_breaks,
      labels = fut_window_labels,
      right = FALSE
    )
  )
  by <- list(
    by = by,
    fut_window = factor(seq_along(fut_window_labels),
                        labels = fut_window_labels)
  )
  by <- level_space_list_to_level_space_data_table(by)
  count_dt <- stat_count(
    x = dt, by = by, subset = subset, subset_style = subset_style
  )

  return(count_dt[])
}

stat_prevalence <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  subject_id_col_nm = NULL,
  by = NULL,
  subset = NULL,
  subset_style = "zero",
  adjust_col_nms = NULL,
  adjust_weights = NULL,
  genpop = NULL
) {
  # use stat_prevalent_subject_count and then use stat_rate
}
