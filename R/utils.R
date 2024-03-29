




# level_list <- list(
#   sex = 1:2,
#   area = 1:5
# )
# level_space_list_to_level_space_data_table(level_list)
#
# level_list <- list(
#   sex = 1:2,
#   area = data.table::data.table(a1 = c(1, 1, 1, 2, 2), a2 = c(1, 2, 3, 1, 2))
# )
# level_space_list_to_level_space_data_table(level_list)
#
# level_list <- list(
#   type = data.table::data.table(t1 = c(1,1,2,2), t2 = c(1,2,3,4)),
#   area = data.table::data.table(a1 = c(1,1,1,2,2), a2 = c(1,2,3,1,2))
# )
# level_space_list_to_level_space_data_table(level_list)
level_space_list_to_level_space_data_table <- function(
  x
) {
  dbc::assert_is_uniquely_named_list(x)
  this_call <- match.call()
  lapply(seq_along(x), function(i) {
    dbc::assert_is_one_of(
      x = x[[i]],
      x_nm = paste0("x[[", i, "]]"),
      call = this_call,
      funs = list(
        dbc::report_is_NULL,
        dbc::report_is_data.table,
        dbc::report_is_vector
      )
    )
  })

  x[vapply(x, is.null, logical(1L))] <- NULL
  dt <- do.call(data.table::CJ, lapply(seq_along(x), function(i) {
    if (data.table::is.data.table(x[[i]])) {
      1:nrow(x[[i]])
    } else {
      seq_along(x[[i]])
    }
  }))
  pos_col_nms <- paste0("_____", names(dt), "_pos")
  names(pos_col_nms) <- names(x)
  data.table::setnames(dt, names(dt), pos_col_nms)
  lapply(seq_along(x), function(i) {
    pos_col_nm <- pos_col_nms[i]
    x_i_is_dt <- data.table::is.data.table(x[[i]])
    x_i <- x[[i]]
    value_col_nms <- if (x_i_is_dt) names(x_i) else names(x)[i]
    pos_vec <- dt[[pos_col_nm]]
    data.table::set(
      x = dt,
      j = value_col_nms,
      value = if (x_i_is_dt) x_i[pos_vec, ] else x_i[pos_vec]
    )
    NULL
  })
  data.table::set(x = dt, j = pos_col_nms, value = NULL)
  data.table::setkeyv(dt, names(dt))
  return(dt[])
}



enforce_level_space <- function(
  x,
  value_col_nms,
  fill = 0L,
  joint_column_level_space
) {
  dbc::assert_is_character_nonNA_vector(value_col_nms)
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = c(value_col_nms, names(joint_column_level_space))
  )
  dbc::assert_is_number_nonNA_vector(fill)
  dbc::assert_is_data_table(joint_column_level_space)

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






handle_subset_arg <- function(
  subset_arg_nm = "subset",
  dataset = emptyenv(),
  function_env = parent.frame(1L),
  enclosing_env = parent.frame(2L),
  function_call = sys.call(1L)
) {
  dbc::assert_is_character_nonNA_atom(subset_arg_nm)
  dbc::assert_has_one_of_classes(
    dataset, classes = c("data.frame", "environment")
  )
  dbc::assert_has_class(
    function_env, required_class = "environment"
  )
  dbc::assert_has_class(
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
  dbc::assert_prod_interim_is_one_of(
    subset_value,
    x_nm = subset_expr_text,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_logical_vector,
                dbc::report_is_number_vector)
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
      if (!length(subset_value) %in% nrow(dataset)) {
        stop("dataset has ", nrow(dataset), " rows but logical subset is of ",
             "length ", length(subset_value))
      }
    }
    if (is.integer(subset_value) && length(subset_value) > 0L && max(subset_value) > nrow(dataset)) {
      stop("max(", subset_expr_text, ") > number of rows in dataset")
    }
  }

  return(subset_value)
}




handle_by_arg <- function(
  by,
  dataset,
  subset,
  subset_style,
  assertion_type = "prod_input"
) {
  # returns a data.table usually but NULL if by = NULL.
  assert_is_arg_by(by, assertion_type = assertion_type)
  if (data.table::is.data.table(by)) {
    dbc::assert_is_data_table_with_required_names(
      x = dataset, required_names = names(by)
    )
  } else if (inherits(by, "list")) {
    by <- level_space_list_to_level_space_data_table(by)
    dbc::assert_prod_interim_is_data_table_with_required_names(
      x = dataset, required_names = names(by)
    )
  } else if (is.character(by)) {
    stratum_col_nms <- by
    by_expr <- quote(
      unique(x = dataset[j = .SD, .SDcols = stratum_col_nms],
             by = stratum_col_nms)
    )
    if (!is.null(subset)) {
      by_expr[["x"]][["i"]] <- quote(subset)
    }
    by <- eval(by_expr)
    data.table::setkeyv(by, stratum_col_nms)
  }

  if (data.table::is.data.table(by)) {
    if (subset_style == "drop") {
      expr <- quote(dataset[j = .SD, .SDcols = names(by)])
      if (!is.null(subset)) {
        expr[["i"]] <- quote(subset)
      }
      sub_space <- unique(eval(expr), by = names(by))
      by <- by[
        i = sub_space,
        on = names(sub_space)
      ]
    }
  }
  return(by)
}

subset_style_options <- function() {
  c("zeros", "drop")
}
report_is_arg_subset_style <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  rbind(
    dbc::report_is_character_nonNA_atom(x, x_nm = "subset_style", call = call),
    dbc::report_atom_is_in_set(
      x, x_nm = "subset_style", set = subset_style_options(),
      call = call
    )
  )
}

assert_is_arg_subset_style <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::report_to_assertion(
    report_is_arg_subset_style(x, x_nm = x_nm, call = call),
    assertion_type = assertion_type
  )
}

report_is_arg_subset <- function(
  x,
  n_dataset_rows,
  x_nm = NULL,
  call = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  report_df <- dbc::report_has_one_of_classes(
    x = x, x_nm = "subset", call = call,
    classes = c("NULL", "integer", "logical")
  )
  if (inherits(x, "logical")) {
    report_df <- rbind(
      report_df,
      dbc::report_is_of_length(
        x = x, x_nm = "subset", call = call,
        expected_length = n_dataset_rows
      )
    )
  } else if (inherits(x, "integer")) {
    report_df <- rbind(
      report_df,
      dbc::report_is_between_inclusive(
        x = x,
        x_nm = "subset",
        call = call,
        lo = -n_dataset_rows,
        hi = n_dataset_rows
      )
    )
  }
  return(report_df)
}

assert_is_arg_subset <- function(
  x,
  n_dataset_rows,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::report_to_assertion(
    report_is_arg_subset(x, n_dataset_rows, x_nm = x_nm, call = call),
    assertion_type = assertion_type
  )
}


assert_is_arg_by <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = "input"
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  dbc::assert_is_one_of(
    x,
    x_nm = x_nm,
    call = call,
    funs = c(dbc::report_is_data_table,
             dbc::report_is_character_nonNA_vector,
             dbc::report_is_list,
             dbc::report_is_NULL),
    assertion_type = assertion_type
  )
}






call_with_arg_list <- function(
    fun,
    arg_list = NULL,
    envir = NULL,
    assertion_type = "prod_input"
) {
  dbc::assert_is_one_of(
    fun,
    funs = list(dbc::report_is_function,
                dbc::report_is_character_nonNA_atom),
    assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    arg_list,
    funs = list(dbc::report_is_list,
                dbc::report_is_NULL),
    assertion_type = assertion_type
  )
  dbc::assert_is_one_of(
    envir,
    funs = list(dbc::report_is_environment,
                dbc::report_is_NULL),
    assertion_type = assertion_type
  )
  if (is.null(envir)) {
    envir <- parent.frame(1L)
  }

  fun_expr <- substitute(fun)
  if (is.character(fun)) {
    fun_nm <- fun
    fun <- eval(parse(text = fun_nm), envir = envir)
  } else if (is.name(fun_expr)) {
    fun_nm <- deparse1(fun_expr)
  } else {
    fun_nm <- "an_anonymous_function"
  }
  if (is.null(arg_list)) {
    arg_list <- mget(names(formals(fun)), envir = envir)
  }

  arg_exprs <- lapply(seq_along(arg_list), function(i) {
    is_unnamed_arg <- names(arg_list)[i] == ""
    if (is_unnamed_arg) {
      substitute(arg_list[[i]], list(i = i))
    } else {
      substitute(arg_list[[arg_nm]], list(arg_nm = names(arg_list)[i]))
    }
  })
  names(arg_exprs) <- names(arg_list)

  eval_expr <- do.call(
    call,
    c(name = fun_nm, arg_exprs),
    quote = TRUE
  )

  eval_env <- new.env(parent = envir)
  eval_env[["arg_list"]] <- arg_list

  return(eval(eval_expr, envir = eval_env))
}



