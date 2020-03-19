




#' @title Counts
#' @description
#' Produce a stratified count table.
#' @param x `[data.table]` (mandatory, no default)
#'
#' compute counts in this dataset
#' @param stratum_col_nms `[NULL, character]` (optional, default `NULL`)
#'
#' - `NULL`: produces total counts in `x`
#' - `character`: produces counts stratified by these columns
#' @param subset `[NULL, integer, logical]` (optional, default `NULL`)
#'
#' - `NULL`: no subsetting, i.e. use whole dataset
#' - `integer`: subset to these rows before computing counts;
#'   `NA` values throw a warning and are not included
#' - `logical`: subset to rows where this is `TRUE` before computing counts;
#'   `NA` values throw a warning and are not included
#' @param joint_column_level_space `[NULL, data.table]` (optional, default `NULL`)
#'
#' - `NULL`: counts are produced using the levels observed in the
#'   (subset of the) dataset
#' - `data.table`: describes the stratum column levels and the relationships
#'   between stratum columns; must have a column for all `stratum_col_nms`
#'   if that was not `NULL`; see **Examples**
#'
#' @examples
#' library("data.table")
#' sls <- data.table::CJ(sex = 1:2, area_2 = 1:5)
#' area_sls <- data.table::data.table(
#'    area_1 = c(1L, 1L, 1L, 2L, 2L), area_2 = 1:5
#' )
#' sls <- merge(sls, area_sls, by = "area_2")
#' data.table::setcolorder(sls, c("sex", "area_1", "area_2"))
#'
#' my_dataset <- data.table::data.table(
#'    sex = 1L,
#'    area_1 = 1L,
#'    area_2 = 1L
#' )
#'
#' stat_count(my_dataset, c("sex", "area_1", "area_2"), joint_column_level_space = sls)
#' @importFrom data.table setkeyv .N :=
#' @export
stat_count <- function(
  x,
  stratum_col_nms = NULL,
  subset = NULL,
  joint_column_level_space = NULL
) {
  if (is.null(joint_column_level_space) && !is.null(stratum_col_nms)) {
    joint_column_level_space <- unique(x, by = stratum_col_nms)
    data.table::setkeyv(joint_column_level_space, stratum_col_nms)
  }

  expr <- quote(
    x[
      j = .N
      ]
  )
  if (!is.null(subset)) {
    expr[["i"]] <- quote(subset)
  }
  if (!is.null(stratum_col_nms)) {
    expr[["keyby"]] <- stratum_col_nms
  }

  count_dt <- eval(expr)

  if (!is.null(joint_column_level_space) && !is.null(stratum_col_nms)) {
    count_dt <- enforce_level_space(
      x = count_dt,
      value_col_nms = "N",
      fill = 0L,
      joint_column_level_space = joint_column_level_space
    )
  }

  return(count_dt[])
}





