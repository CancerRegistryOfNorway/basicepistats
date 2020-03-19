




#' @title Rates
#' @description
#' Event rates table, optionally stratified and directly adjusted.
#' @param x `[data.table]` (mandatory, no default)
#'
#' passed to [basicepistats::stat_count]
#' @param offset_dt `[data.table]` (mandatory, no default)
#'
#' contains offsets (typically mean population sizes) for the counts produced
#' using `x`; see [basicepistats::offset_dt]
#' @param stratum_col_nms output will be stratified by these columns
#' @param adjust_col_nms output will be adjusted by these columns; see
#' [directadjusting::directly_adjusted_estimates]
#' @param subset passed to [basicepistats::stat_count]
#' @param adjust_weights passed to
#' [directadjusting::directly_adjusted_estimates] argument `weights`
#' @param joint_column_level_space passed to [basicepistats::stat_count]
#'
#' @examples
#' library("data.table")
#' offset_dt <- data.table::CJ(
#'   sex = 1:2,
#'   agegroup = 1:18
#' )
#' offset_dt[, "offset" := rpois(.N, lambda = 15000)]
#' @export
stat_rate <- function(
  x,
  offset_dt,
  stratum_col_nms = NULL,
  adjust_col_nms = NULL,
  subset = NULL,
  adjust_weigths = NULL,
  joint_column_level_space = NULL
) {

}




