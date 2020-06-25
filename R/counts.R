




#' @title Counts
#' @description
#' Produce a stratified count table.
#' @param x `[data.table]` (mandatory, no default)
#'
#' compute counts in this dataset
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
#' @name counts
NULL

#' @rdname counts
#' @details
#' - `stat_count` produces the number of records by strata.
#' @export
stat_count <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1]
) {
  stat_expr(
    x = x,
    expr = quote(.N),
    by = by,
    subset = subset,
    subset_style = subset_style
  )
}


#' @rdname counts
#' @param unique_by `[character]` (mandatory, no default)
#'
#' names of columns in `x`; unique combinations of these columns are counted;
#' e.g. `unique_by = "my_subject_id"` to count numbers of subjects by strata
#' @details
#' - `stat_unique_count` produces the number of unique combinations of columns
#'   defined in `unique_by`; e.g. the number of unique subjects by strata.
#' @export
stat_unique_count <- function(
  x,
  unique_by,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1]
) {
  easyassertions::assert_is_character_nonNA_vector(unique_by)
  easyassertions::assert_is_data_table_with_required_names(
    x,
    required_names = unique_by
  )
  stat_expr(
    x = x,
    expr = substitute(uniqueN(.SD, by = UB), list(UB = unique_by)),
    by = by,
    subset = subset,
    subset_style = subset_style
  )
}


#' @importFrom data.table setkeyv .N := is.data.table
#' @importFrom easyassertions assert_is_data_table assert_atom_is_in_set
stat_expr <- function(
  x,
  expr = quote(.N),
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {
  easyassertions::assert_is_data_table(x)
  stopifnot(
    inherits(expr, c("call", "name"))
  )
  subset <- handle_subset_arg(dataset = x)
  easyassertions::assert_atom_is_in_set(
    x = subset_style,
    x_nm = "subset_style",
    set = c("zeros", "drop")
  )
  by <- handle_by_arg(
    by = by,
    dataset = x,
    subset = subset,
    subset_style = subset_style
  )
  stratum_col_nms <- names(by)

  x_expr <- quote(x[])
  x_expr[["j"]] <- expr
  if (grepl(".SD", paste0(deparse(expr), collapse = ""))) {
    x_expr[[".SDcols"]] <- names(x)
  }
  if (!is.null(subset)) {
    x_expr[["i"]] <- quote(subset)
  }
  if (!is.null(stratum_col_nms)) {
    x_expr[["keyby"]] <- stratum_col_nms
  }
  result_dt <- eval(x_expr)

  result_dt <- enforce_level_space(
    x = result_dt,
    value_col_nms = setdiff(names(result_dt), stratum_col_nms),
    fill = 0L,
    joint_column_level_space = by
  )
  set_stat_table(
    result_dt,
    stratum_col_nms = names(by),
    value_col_nms = setdiff(names(result_dt), names(by))
  )
  return(result_dt[])
}



