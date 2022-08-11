




#' @title List of Statistics TableS
#' @description Call statistics function multiple times to create a list
#' of statistics tables.
#' @param varying_arg_list `[list]` (mandatory, no default)
#'
#' list of arguments that vary; function named `stat_fun_nm` can be called
#' several times, and each time the arguments supplied here can be different;
#' each element of this list must in turn be a list and have as name one of
#' the names of the arguments of the function. each element of
#' `varying_arg_list` must be of the same length. the function will be called
#' as many times as each element of `varying_arg_list` has elements in turn.
#' e.g. `list(by = list("sex", "agegroup", c("sex", "agegroup")))`. see
#' Examples.
#'
#' @param fixed_arg_list `[list]` (optional, default `list()`)
#'
#' list of arguments used by function named `stat_fun_nm`; these arguments
#' will be the same each time the function is called. `fixed_arg_list`
#' and `varying_arg_list` cannot contain the same arguments.
#' @param stat_fun_nm `[character]` (mandatory, no default)
#'
#' Name of a function; must be discoverable by [match.fun]. The function must
#' return a `stat_table` --- see `?basicepistats::stat_table`.
#' @export
#' @examples
#' library("data.table")
#'
#' my_dataset <- data.table::data.table(
#'    sex = 1L,
#'    area_1 = 1L,
#'    area_2 = 1L
#' )
#'
#' stl <- stat_table_list(
#'   varying_arg_list = list(
#'     by = list("sex", "area_1")
#'   ),
#'   fixed_arg_list = list(x = my_dataset),
#'   stat_fun_nm = "stat_count"
#' )
#'
stat_table_list <- function(
    varying_arg_list,
    fixed_arg_list = list(),
    stat_fun_nm
) {
  match.fun(stat_fun_nm)
  stopifnot(
    inherits(fixed_arg_list, "list"),
    inherits(varying_arg_list, "list"),
    length(names(fixed_arg_list)) == length(fixed_arg_list),
    names(fixed_arg_list) %in% names(formals(stat_fun_nm)),
    length(names(varying_arg_list)) == length(varying_arg_list),
    names(varying_arg_list) %in% names(formals(stat_fun_nm)),
    vapply(varying_arg_list, length, integer(1L)) == length(varying_arg_list[[1]]),
    !names(varying_arg_list) %in% names(fixed_arg_list)
  )
  n_tables <- length(varying_arg_list[[1L]])
  stat_table_list <- lapply(1:n_tables, function(table_no) {
    arg_list <- fixed_arg_list
    varied <- lapply(varying_arg_list, function(arg_set) {
      arg_set[[table_no]]
    })
    arg_list[names(varying_arg_list)] <- varied
    st <- call_with_arg_list(stat_fun_nm, arg_list = arg_list)
    if (!inherits(st, stat_table_class_name())) {
      stop("stat_fun_nm = \"", stat_fun_nm, "\" did not return an object with ",
           "class \"", stat_table_class_name(), "\"")
    }
    return(st)
  })
  stat_table_list_set(
    stat_table_list,
    stat_fun_nms = rep(stat_fun_nm, length(stat_table_list))
  )
  return(stat_table_list)
}

stat_table_list_set <- function(x, stat_fun_nms) {
  dbc::assert_is("length(stat_fun_nms) == nrow(x)",
                 assertion_type = "input")
  stat_table_list_class_set(x)
  stat_table_list_meta_set(x, stat_fun_nms = stat_fun_nms)
}

stat_table_list_meta_name <- function() "stat_table_list_meta"
stat_table_list_meta_get <- function(x) {
  data.table::copy(attr(x, stat_table_list_meta_name()))
}
stat_table_list_meta_set <- function(x, stat_fun_nms) {
  dbc::assert_is("length(stat_fun_nms) == nrow(x)",
                 assertion_type = "input")
  meta <- data.table::data.table(stat_fun_nm = stat_fun_nms)
  data.table::setattr(x, stat_table_list_meta_name(), meta)
}

stat_table_list_class_name <- function() "stat_table_list"
stat_table_list_class_set <- function(x) {
  data.table::setattr(x, "class", c(stat_table_list_class_name(), "list"))
}



#' @export
"[.stat_table_list" <- function(x, i, ...) {
  y <- NextMethod()
  x_meta <- stat_table_list_meta_get(x)
  y_meta <- x_meta
  y_meta[["stat_fun_nm"]] <- y_meta[["stat_fun_nm"]][i]
  stat_table_list_set(y, y_meta)
  return(y)
}

#' @export
print.stat_table_list <- function(x, ...) {
  cat("* stat_table_list of length", length(x), "\n")
  cat("* access stat_table objects using e.g. x[[1]]\n")
  stl_meta_dt <- stat_table_list_meta_get(x)
  dt <- data.table::rbindlist(lapply(seq_along(x), function(pos) {
    stat_table <- x[[pos]]
    info_dt <- stl_meta_dt[pos, ]
    st_meta_list <- stat_table_meta_get(stat_table)
    lapply(names(st_meta_list), function(nm) {
      info_dt[, (nm) := list(st_meta_list[nm])]
      NULL
    })
    info_dt[, "col_nm_set" := list(list(names(stat_table)))]
    info_dt[, "n_row" := nrow(stat_table)]
    return(info_dt)
  }))
  print(dt)
}

#' @export
c.stat_table_list <- function(...) {
  stll <- data.table::copy(list(...))
  stopifnot(
    vapply(stll, inherits, logical(1L), what = stat_table_list_class_name())
  )
  invisible(lapply(stll, function(stl) {
    data.table::setattr(stl, "class", "list")
    NULL
  }))
  stl <- do.call(c, stll)
  meta <- data.table::rbindlist(lapply(stll, stat_table_list_meta_get))
  stat_table_list_set(stl, stat_fun_nms = meta[["stat_fun_nm"]])
  return(stl)
}

