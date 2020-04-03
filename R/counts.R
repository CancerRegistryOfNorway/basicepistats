




#' @title Counts
#' @description
#' Produce a stratified count table.
#' @param x `[data.table]` (mandatory, no default)
#'
#' compute counts in this dataset
#' @param stratum_col_nms `[NULL, character]` (optional, default `NULL`)
#'
#' - `NULL`: produces total counts in `x`, unless `joint_column_level_space`
#'   is not `NULL`; then `names(joint_column_level_space)` is used and
#'   counts are produced by each of these stratifying column
#' - `character`: produces counts stratified by these columns
#' @template arg_subset
#' @template arg_subset_style
#' @template arg_by
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
#' stat_count(
#'   x = my_dataset,
#'   by = sls
#' )
#' stat_count(
#'   x = my_dataset,
#'   by = list(sex = 1:2, area_1 = 1:2)
#' )
#' stat_count(
#'   x = my_dataset,
#'   by = list(sex = 1:2, area = area_sls)
#' )
#' @importFrom data.table setkeyv .N :=
#' @export
stat_count <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1]
) {
  easyassertions::assert_is_data_table(x)
  easyassertions::assert_is_one_of(
    by,
    fun_nms = c("assert_is_data_table", "assert_is_character_nonNA_vector",
                "assert_is_list", "assert_is_NULL")
  )
  subset <- handle_subset_arg(dataset = x)
  easyassertions::assert_atom_is_in_set(
    x = subset_style,
    x_nm = "subset_style",
    set = c("zeros", "drop")
  )
  if (data.table::is.data.table(by)) {
    stratum_col_nms <- names(by)
  } else if (inherits(by, "list")) {
    by <- level_space_list_to_level_space_data_table(by)
    stratum_col_nms <- names(by)
  } else if (is.character(by)) {
    stratum_col_nms <- by
    by_expr <- quote(
      unique(x[j = .SD, .SDcols = stratum_col_nms], by = stratum_col_nms)
    )
    if (!is.null(subset)) {
      by_expr[["i"]] <- quote(subset)
    }
    by <- eval(by_expr)
    data.table::setkeyv(by, stratum_col_nms)
  }
  easyassertions::assert_is_data_table_with_required_names(
    x = x, required_names = stratum_col_nms
  )

  expr <- quote(x[j = .N])
  if (!is.null(subset)) {
    expr[["i"]] <- quote(subset)
  }
  if (!is.null(stratum_col_nms)) {
    expr[["keyby"]] <- stratum_col_nms
  }
  count_dt <- eval(expr)

  if (data.table::is.data.table(by)) {
    if (subset_style == "drop") {
      expr <- quote(x[j = .SD, .SDcols = names(by)])
      if (!is.null(subset)) {
        expr[["i"]] <- quote(subset)
      }
      sub_space <- unique(eval(expr), by = names(by))
      by <- by[
        i = sub_space,
        on = names(sub_space)
      ]
    }
    count_dt <- enforce_level_space(
      x = count_dt,
      value_col_nms = "N",
      fill = 0L,
      joint_column_level_space = by
    )
  }

  return(count_dt[])
}





