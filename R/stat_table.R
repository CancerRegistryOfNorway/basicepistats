


#' @title Stat Tables
#' @description
#' Class `stat_table` methods and functions.
#' @param x `[R object]` (mandatory, no default)
#' - `stat_table`, `as.stat_table`, `set_stat_table`:
#'   an R object to coerce to `stat_table`
#' - `print.stat_table`: object to print
#' - `[.stat_table`: object to extract from
#' - `stat_table_meta`, `stat_table_stratum_col_nms`
#'   `stat_table_value_col_nms`: object to extract `stat_table` meta data from
#' @param stratum_col_nms `[character]` (mandatory, no default)
#'
#' names of stratifying columns in `x`
#' @param value_col_nms `[character]` (mandatory, no default)
#'
#' names of columns in `x` containing the actual values (statistics)
#' @name stat_table


#' @rdname stat_table
#' @importFrom data.table copy
#' @export
stat_table_meta <- function(x) {
  lapply(attr(x, "stat_table_meta"), intersect, y = names(x))
}

#' @rdname stat_table
#' @export
stat_table_stratum_col_nms <- function(x) {
  stat_table_meta(x)[["stratum_col_nms"]]
}

#' @rdname stat_table
#' @export
stat_table_value_col_nms <- function(x) {
  stat_table_meta(x)[["value_col_nms"]]
}

#' @rdname stat_table
#' @importFrom data.table setattr
#' @importFrom dbc assert_is_data.frame assert_is_character_nonNA_vector
#' assert_vector_elems_are_in_set
#' @export
set_stat_table <- function(x, stratum_col_nms, value_col_nms) {
  # dbc::assert_is_data.frame(x)
  # dbc::assert_is_character_nonNA_vector(stratum_col_nms)
  # dbc::assert_is_character_nonNA_vector(value_col_nms)
  # dbc::assert_vector_elems_are_in_set(stratum_col_nms, set = names(x))
  # dbc::assert_vector_elems_are_in_set(value_col_nms, set = names(x))
  data.table::setattr(
    x,
    "class",
    union("stat_table", class(x))
  )
  data.table::setattr(
    x,
    "stat_table_meta",
    mget(c("stratum_col_nms", "value_col_nms"))
  )
  invisible(NULL)
}

#' @rdname stat_table
#' @export
#' @importFrom data.table copy
stat_table <- function(x, stratum_col_nms, value_col_nms) {
  x <- data.table::copy(x)
  set_stat_table(x, stratum_col_nms, value_col_nms)
  x[]
}

#' @rdname stat_table
#' @export
as.stat_table <- function(x, stratum_col_nms, value_col_nms) {
  UseMethod("as.stat_table")
}

#' @rdname stat_table
#' @export
as.stat_table.data.frame <- function(x, stratum_col_nms, value_col_nms) {
  stat_table(
    x = x, stratum_col_nms = stratum_col_nms, value_col_nms = value_col_nms
  )
}

#' @rdname stat_table
#' @export
print.stat_table <- function(x, ...) {
  meta <- attr(x, "stat_table_meta")
  meta <- lapply(meta, intersect, x = names(x))
  cat("* stat_table with\n")
  cat("*  - stratum_col_nms =", deparse(meta[["stratum_col_nms"]]), "\n")
  cat("*  - value_col_nms =", deparse(meta[["value_col_nms"]]), "\n")
  NextMethod()
}

#' @rdname stat_table
#' @importFrom data.table setattr
#' @export
#' @param ... passed to next method (see `?"["`)
"[.stat_table" <- function(x, ...) {
  meta <- attr(x, "stat_table_meta")
  data.table::setattr(x, "class", setdiff(class(x), "stat_table"))
  on.exit(data.table::setattr(x, "class", union("stat_table", class(x))))
  y <- x[...]
  if (is.data.frame(y)) {
    set_stat_table(
      x = y,
      stratum_col_nms = intersect(meta[["stratum_col_nms"]], names(y)),
      value_col_nms = intersect(meta[["value_col_nms"]], names(y))
    )
  }
  return(y)
}


#' @rdname stat_table
#' @importFrom data.table setattr
#' @export
#' @param value see `?"[<-"`
#'
"[<-.stat_table" <- function(x, ..., value) {
  data.table::setattr(x, "class", setdiff(class(x), "stat_table"))
  meta <- stat_table_meta(x)
  set_stat_table(
    x,
    stratum_col_nms = intersect(meta[["stratum_col_nms"]], names(x)),
    value_col_nms = intersect(meta[["value_col_nms"]], names(x))
  )
  `[<-`(x = x, ..., value = value)
}



#' @rdname stat_table
#' @importFrom data.table setattr
#' @export
"[[<-.stat_table" <- function(x, ..., value) {
  data.table::setattr(x, "class", setdiff(class(x), "stat_table"))
  meta <- stat_table_meta(x)
  set_stat_table(
    x,
    stratum_col_nms = intersect(meta[["stratum_col_nms"]], names(x)),
    value_col_nms = intersect(meta[["value_col_nms"]], names(x))
  )
  `[[<-`(x = x, ..., value = value)
}
