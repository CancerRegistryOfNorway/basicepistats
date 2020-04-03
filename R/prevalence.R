




#' @title Prevalence
#' @description
#' Compute subject prevalence at specific time points and optionally
#' conditioning on time since event at the time points.
#' @param x `[data.table]` (mandatory, no default)
#'
#' dataset containing one or more records by subject
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
#' @param follow_up_time_window_widths `[numeric]` (mandatory, no default)
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
#' @template arg_joint_column_level_space
stat_prevalent_subject_count <- function(
  x,
  subject_id_col_nm,
  follow_up_time_col_nm,
  follow_up_time_window_widths,
  stratum_col_nms = NULL,
  subset = NULL,
  subset_style = "zero",
  joint_column_level_space = NULL
) {
  # split x at prevalence_time_points and count number of subjects identified
  # by subject_id_col_nm
}

stat_prevalence <- function(
  x,
  subject_id_col_nm,
  prevalence_time_scale_col_nm,
  prevalence_time_points,
  prevalence_window_widths,
  offset_dt,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  subset = NULL,
  adjust_weigths = NULL,
  joint_column_level_space = NULL
) {
  # use stat_prevalent_subject_count and then use stat_rate
}
