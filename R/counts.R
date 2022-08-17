




#' @title Counts
#' @description
#' Produce a stratified count table.
#' @param x `[data.table]` (mandatory, no default)
#'
#' compute counts in this dataset
#' @template arg_subset
#' @template arg_subset_style
#' @template arg_by
#' @template arg_assertion_type
#' @template arg_melt
#' @examples
#'
#' # basicepistats::stat_count
#' library("data.table")
#' sls <- data.table::CJ(sex = 1:2, area_2 = c(11L, 12L, 13L, 21L, 22L))
#' area_sls <- data.table::data.table(
#'    area_1 = c(10L, 10L, 10L, 20L, 20L),
#'    area_2 = c(11L, 12L, 13L, 21L, 22L)
#' )
#' sls <- merge(sls, area_sls, by = "area_2")
#' data.table::setcolorder(sls, c("sex", "area_1", "area_2"))
#' data.table::setkeyv(sls, names(sls))
#'
#' my_dataset <- data.table::data.table(
#'    sex = 1L,
#'    area_1 = 10L,
#'    area_2 = 11L
#' )
#'
#' basicepistats::stat_count(
#'   x = my_dataset,
#'   by = sls
#' )
#' basicepistats::stat_count(
#'   x = my_dataset,
#'   by = list(sex = 1:2, area_1 = c(10L, 20L))
#' )
#' basicepistats::stat_count(
#'   x = my_dataset,
#'   by = list(sex = 1:2, area = area_sls)
#' )
#'
#' ## melt
#' observed <- basicepistats::stat_count(
#'   x = my_dataset,
#'   by = sls,
#'   melt = list(area = c("area_1", "area_2"))
#' )
#' stopifnot(
#'   observed[["N"]][observed[["area"]] == 1 & observed[["area"]] == 10L] == 1,
#'   observed[["N"]][observed[["area"]] == 1 & observed[["area"]] == 11L] == 1
#' )
#'
#' @name counts
NULL

#' @rdname counts
#' @details
#' - `stat_count` produces the number of records by strata; this function is
#'   intended to be used directly by the end-user
#' @export
#' @importFrom data.table .N
stat_count <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "user_input"
) {
  # @codedoc_comment_block news("basicepistats::stat_count", "2022-07-26", "0.2.0")
  # `basicepistats::stat_count` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_count", "2022-07-26", "0.2.0")

  dbc::assert_is_data_table(x, assertion_type = assertion_type)
  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)
  assert_is_arg_melt(melt, allowed_col_nms = names(x),
                     assertion_type = assertion_type)

  dt <- stat_expr_(
    x = x,
    expr = quote(list(N = .N)),
    by = by,
    subset = subset,
    subset_style = subset_style,
    assertion_type = "prod_input"
  )
  dt <- melt_sum(dt = dt, melt = melt)
  return(dt[])
}


#' @rdname counts
#' @details
#' - `stat_count_` produces the number of records by strata; this function is
#'   intended to be used within other functions
#' @export
#' @importFrom data.table .N
stat_count_ <- function(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "prod_input"
) {
  # @codedoc_comment_block news("basicepistats::stat_count_", "2022-07-26", "0.2.0")
  # `basicepistats::stat_count_` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_count_", "2022-07-26", "0.2.0")

  dbc::assert_is_data_table(x, assertion_type = assertion_type)

  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)
  assert_is_arg_melt(melt, allowed_col_nms = names(x),
                     assertion_type = assertion_type)

  dt <- stat_expr_(
    x = x,
    expr = quote(list(N = .N)),
    by = by,
    subset = subset,
    subset_style = subset_style,
    assertion_type = "prod_input"
  )
  dt <- melt_sum(dt = dt, melt = melt)
  return(dt[])
}



#' @rdname counts
#' @param unique_by `[character]` (mandatory, no default)
#'
#' names of columns in `x`; unique combinations of these columns are counted;
#' e.g. `unique_by = "my_subject_id"` to count numbers of subjects by strata;
#' note that this causes each thing to be counted zero or one
#' times in each stratum separately --- so e.g. the same person can appear
#' once in multiple strata; see **Examples**
#' @details
#' - `stat_unique_count` produces the number of unique combinations of columns
#'   defined in `unique_by`; e.g. the number of unique subjects by strata;
#'   this function is intended to be used directly by the end-user
#' @export
#' @examples
#'
#' # basicepistats::stat_unique_count
#' dt <- data.table::data.table(id = c(1, 1, 2), grp = 1:3)
#' observed <- basicepistats::stat_unique_count(
#'   x = dt,
#'   unique_by = "id",
#'   by = "grp"
#' )
#' expected <- data.table::data.table(grp = 1:3, N = 1L, key = "grp")
#' stopifnot(all.equal(observed, expected, check.attributes = FALSE))
#'
stat_unique_count <- function(
  x,
  unique_by,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("basicepistats::stat_unique_count", "2022-07-26", "0.2.0")
  # `basicepistats::stat_unique_count` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_unique_count", "2022-07-26", "0.2.0")

  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_vector(
    unique_by,
    assertion_type = assertion_type
  )
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = unique_by,
    assertion_type = assertion_type
  )
  assert_is_arg_melt(melt, allowed_col_nms = names(x),
                     assertion_type = assertion_type)

  dt <- stat_unique_count_(
    x = x,
    unique_by = unique_by,
    by = by,
    subset = subset,
    subset_style = subset_style,
    assertion_type = "prod_input"
  )
  dt <- melt_sum(dt = dt, melt = melt)
  return(dt[])
}

#' @rdname counts
#' @details
#' - `stat_unique_count_` produces the number of unique combinations of columns
#'   defined in `unique_by`; e.g. the number of unique subjects by strata;
#'   this function is intended to be used inside other functions
#' @export
stat_unique_count_ <- function(
  x,
  unique_by,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "prod_input"
) {
  # @codedoc_comment_block news("basicepistats::stat_unique_count_", "2022-07-26", "0.2.0")
  # `basicepistats::stat_unique_count_` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_unique_count_", "2022-07-26", "0.2.0")

  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_vector(
    unique_by, assertion_type = assertion_type
  )
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = unique_by,
    assertion_type = assertion_type
  )
  assert_is_arg_melt(melt, allowed_col_nms = names(x),
                     assertion_type = assertion_type)

  dt <- stat_expr_(
    x = x,
    expr = substitute(
      list(N = data.table::uniqueN(.SD, by = UB)),
      list(UB = unique_by)
    ),
    by = by,
    subset = subset,
    subset_style = subset_style,
    assertion_type = "prod_input"
  )
  dt <- melt_sum(dt = dt, melt = melt)
  return(dt[])
}

#' @importFrom data.table .N
stat_expr_ <- function(
  x,
  expr = quote(list(N = .N)),
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  assertion_type = "prod_input"
) {
  # @codedoc_comment_block news("basicepistats::stat_expr_", "2022-07-26", "0.2.0")
  # `basicepistats::stat_expr_` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_expr_", "2022-07-26", "0.2.0")

  # @codedoc_comment_block news("basicepistats::stat_expr_", "2022-07-26", "0.2.1")
  # `basicepistats::stat_expr_` fix: refer to `uniqueN` with
  # `data.table::uniqueN`. Now `data.table` need not be loaded with
  # `library(data.table)`.
  # @codedoc_comment_block news("basicepistats::stat_expr_", "2022-07-26", "0.2.1")

  dbc::assert_is_data_table(x, assertion_type = assertion_type)
  dbc::assert_has_one_of_classes(expr, classes = c("call", "name"),
                                 assertion_type = assertion_type)
  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)
  subset <- handle_subset_arg(dataset = x)
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
  if (!inherits(result_dt, "data.table")) {
    stop("basicepistas internal error: ",
         "result of expression ", as.character(x_expr), " did ",
         "not evaluate to a data.table; if you see this, complain to the ",
         "author or maintainer of the function you just used")
  }

  if (length(stratum_col_nms) > 0L) {
    value_col_nms <- setdiff(names(result_dt), stratum_col_nms)
    result_dt <- enforce_level_space(
      x = result_dt,
      value_col_nms = value_col_nms,
      fill = 0L,
      joint_column_level_space = by
    )
  }
  stat_table_set(
    result_dt,
    stratum_col_nms = names(by),
    value_col_nms = setdiff(names(result_dt), names(by))
  )
  return(result_dt[])
}



