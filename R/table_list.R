




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
#' name of a function; must be discoverable by [match.fun]
#' @export
#' @importFrom data.table setattr
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
    do.call(stat_fun_nm, arg_list)
  })
  data.table::setattr(
    stat_table_list, "class", union("stat_table_list", class(stat_table_list))
  )
  data.table::setattr(
    stat_table_list, "table_list_meta", list(
      stat_fun_nm = rep(stat_fun_nm, length(stat_table_list))
    )
  )
  return(stat_table_list)
}


#' @importFrom data.table copy setattr
#' @export
"[.stat_table_list" <- function(x, i, ...) {
  x_meta <- data.table::copy(attr(x, "table_list_meta"))
  data.table::setattr(x, "class", "list")
  x_subset <- x[i]
  data.table::setattr(x, "class", c("stat_table_list", "list"))
  data.table::setattr(
    x_subset, "class", union("stat_table_list", class(x_subset))
  )
  which_were_kept <- match(x_subset, x)
  x_meta[["stat_fun_nm"]] <- x_meta[["stat_fun_nm"]][which_were_kept]
  data.table::setattr(x_subset, "table_list_meta", x_meta)
  x_subset
}

#' @importFrom data.table rbindlist setDT
#' @export
print.stat_table_list <- function(x, ...) {
  cat("stat_table_list of length ", length(x), "\n:")
  meta <- attr(x, "table_list_meta")
  stat_fun_nms <- meta[["stat_fun_nm"]]
  dt <- data.table::rbindlist(lapply(seq_along(x), function(pos) {
    table <- x[[pos]]
    data.table::setDT(list(
      stat_fun_nm = stat_fun_nms[pos],
      col_nm_set = deparse(names(table)),
      n_rows = nrow(table)
    ))
  }))
  print(dt)
}

#' @importFrom data.table copy setattr
#' @export
c.stat_table_list <- function(...) {
  tll <- data.table::copy(list(...))
  stopifnot(
    vapply(tll, inherits, logical(1L), what = "stat_table_list")
  )
  invisible(lapply(tll, function(tl) {
    data.table::setattr(tl, "class", "list")
  }))
  tl <- do.call(c, tll)
  data.table::setattr(tl, "class", c("stat_table_list", "list"))
  meta_list <- lapply(tll, attr, which = "table_list_meta")
  meta <- list(
    stat_fun_nm = unlist(lapply(meta_list, function(meta) {
      meta[["stat_fun_nm"]]
    }))
  )
  data.table::setattr(tl, "table_list_meta", meta)
  tl
}

