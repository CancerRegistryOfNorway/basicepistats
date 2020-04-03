




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
#' @template arg_joint_column_level_space
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
#'   stratum_col_nms = c("sex", "area_1", "area_2"),
#'   joint_column_level_space = sls
#' )
#' stat_count(
#'   x = my_dataset,
#'   stratum_col_nms = c("sex", "area_1", "area_2"),
#'   joint_column_level_space = list(sex = 1:2, area_1 = 1:2)
#' )
#' stat_count(
#'   x = my_dataset,
#'   stratum_col_nms = c("sex", "area_1", "area_2"),
#'   joint_column_level_space = list(sex = 1:2, area = area_sls)
#' )
#' @importFrom data.table setkeyv .N :=
#' @export
stat_count <- function(
  x,
  stratum_col_nms = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  joint_column_level_space = NULL
) {
  easyassertions::assert_is_one_of(
    stratum_col_nms,
    fun_nms = c("assert_is_character_nonNA_vector", "assert_is_NULL")
  )
  easyassertions::assert_atom_is_in_set(
    x = subset_style,
    x_nm = "subset_style",
    set = c("zeros", "drop")
  )
  easyassertions::assert_is_one_of(
    joint_column_level_space,
    fun_nms = c("assert_is_data_table", "assert_is_list", "assert_is_NULL")
  )
  if (is.null(stratum_col_nms)) {
    stratum_col_nms <- character(0L)
    if (!is.null(joint_column_level_space)) {
      stratum_col_nms <- names(joint_column_level_space)
    }
  }
  easyassertions::assert_is_data_table_with_required_names(
    x = x, required_names = stratum_col_nms
  )
  subset <- handle_subset_arg(dataset = x)

  if (data.table::is.data.table(joint_column_level_space)) {
    easyassertions::assert_has_only_names(
      x = joint_column_level_space,
      required_names = stratum_col_nms
    )
  } else if (inherits(joint_column_level_space, "list")) {
    joint_column_level_space <- level_space_list_to_level_space_data_table(
      joint_column_level_space
    )
    easyassertions::assert_has_only_names(
      x = joint_column_level_space,
      x_nm = paste0(
        "joint_column_level_space data.table produced from list-type ",
        "joint_column_level_space"
      ),
      required_names = stratum_col_nms
    )
  } else if (is.null(joint_column_level_space) && !is.null(stratum_col_nms)) {
    joint_column_level_space <- unique(x, by = stratum_col_nms)[subset, ]
    data.table::setkeyv(joint_column_level_space, stratum_col_nms)
  }

  expr <- quote(x[j = .N])
  if (!is.null(subset)) {
    expr[["i"]] <- quote(subset)
  }
  if (!is.null(stratum_col_nms)) {
    expr[["keyby"]] <- stratum_col_nms
  }
  count_dt <- eval(expr)

  if (data.table::is.data.table(joint_column_level_space)) {
    if (subset_style == "drop") {
      expr <- quote(x[j = .SD, .SDcols = names(joint_column_level_space)])
      if (!is.null(subset)) {
        expr[["i"]] <- quote(subset)
      }
      sub_space <- unique(eval(expr), by = names(joint_column_level_space))
      joint_column_level_space <- joint_column_level_space[
        i = sub_space,
        on = names(sub_space)
      ]
    }
    count_dt <- enforce_level_space(
      x = count_dt,
      value_col_nms = "N",
      fill = 0L,
      joint_column_level_space = joint_column_level_space
    )
  }

  return(count_dt[])
}





