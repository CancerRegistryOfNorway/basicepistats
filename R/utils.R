




#' @importFrom data.table is.data.table set
enforce_level_space <- function(
  x,
  value_col_nms,
  fill = 0L,
  joint_column_level_space
) {
  easyassertions::assert_is_character_nonNA_vector(value_col_nms)
  easyassertions::assert_is_data_table_with_required_names(
    x,
    required_names = c(value_col_nms, names(joint_column_level_space))
  )
  easyassertions::assert_is_number_nonNA_vector(fill)
  easyassertions::assert_is_data_table(joint_column_level_space)

  if (length(fill) == 1L) {
    fill <- rep(fill, length(value_col_nms))
  }
  names(fill) <- value_col_nms
  x <- x[
    i = joint_column_level_space,
    on = names(joint_column_level_space)
    ]
  lapply(value_col_nms, function(value_col_nm) {
    is_na <- is.na(x[[value_col_nm]])
    data.table::set(
      x = x,
      i = which(is_na),
      j = value_col_nm,
      value = fill[value_col_nm]
    )
    NULL
  })

  x[]
}





tf <- function(x, subset_1) {
  handle_subset_arg(subset_arg_nm = "subset_1", dataset = x)
}





handle_subset_arg <- function(
  subset_arg_nm = "subset",
  dataset = emptyenv(),
  function_env = parent.frame(1L),
  enclosing_env = parent.frame(2L),
  function_call = sys.call(1L)
) {
  easyassertions::assert_is_character_nonNA_atom(subset_arg_nm)
  easyassertions::assert_has_one_of_classes(
    dataset, classes = c("data.frame", "environment")
  )
  easyassertions::assert_has_class(
    function_env, required_class = "environment"
  )
  easyassertions::assert_has_class(
    enclosing_env, required_class = "environment"
  )
  stopifnot(
    is.name(parse(text = subset_arg_nm)[[1L]])
  )

  # symbol of subset arg of function where this function is used
  # e.g. my_subset_arg in my_fun(my_subset_arg = column > 1L, dataset = my_data)
  subset_arg_symbol <- parse(text = subset_arg_nm)[[1L]]
  # infer the expression supplied to the subset arg of the
  # function where this function is used; e.g.
  # e.g. column > 1L in my_fun(my_subset_arg = column > 1L, dataset = my_data)
  subset_expr <- eval(substitute(
    substitute(SUBSET_OBJ), list(SUBSET_OBJ = subset_arg_symbol)
  ), envir = function_env)
  # evaluate that inferred expression in the context of the dataset and
  # secondarily the enclosing env
  value_eval_env <- as.environment(dataset)
  parent.env(value_eval_env) <- enclosing_env
  subset_value <- eval(subset_expr, envir = value_eval_env)

  subset_expr_text <- paste0(deparse(subset_expr), collapse = "")
  easyassertions::assert_is_one_of(
    subset_value,
    x_nm = subset_expr_text,
    fun_nms = c("assert_is_NULL", "assert_is_logical_vector",
                "assert_is_number_vector")
  )

  if (anyNA(subset_value)) {
    is_na <- is.na(subset_value)
    msg <- paste0(
      "there were ", sum(is_na), " NA values passed to argument ",
      deparse(subset_arg_nm),"; they will not be included in subset"
    )
    warning(simpleWarning(msg, call = function_call))
    if (is.logical(subset_value)) {
      subset_value[is_na] <- FALSE
    } else {
      subset_value <- subset_value[!is_na]
    }
  }

  if (is.numeric(subset_value) && any(subset_value %% 1L != 0L)) {
    stop("numeric subset is not integer-like; e.g. subset = c(1, 5) ",
         "is fine, but subset = c(1.1, 5.1) is not")
  }

  if (is.data.frame(dataset) && !is.null(subset_value)) {
    if (is.logical(subset_value)) {
      if (!length(subset_value) %in% c(1L, nrow(dataset))) {
        stop("dataset has ", nrow(dataset), " rows but logical subset is of ",
             "length ", length(subset_value))
      }
    }
    if (is.integer(subset_value) && max(subset_value) > nrow(dataset)) {
      stop("max(", subset_expr_text, ") > number of rows in dataset")
    }
  }

  return(subset_value)
}








