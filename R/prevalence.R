

stat_prevalent_subject_count <- function(
  x,
  subject_id_col_nm,
  prevalence_time_scale_col_nm,
  prevalence_time_points,
  prevalence_window_widths,
  stratum_col_nms = NULL,
  subset = NULL,
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
