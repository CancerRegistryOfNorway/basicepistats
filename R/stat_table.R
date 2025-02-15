


#' @title Stat Tables
#' @description
#' Class `stat_table` methods and functions.
#' @param x `[R object]` (mandatory, no default)
#' - `stat_table`, `as.stat_table`, `stat_table_set`:
#'   an R object to coerce to `stat_table`
#' - `print.stat_table`: object to print
#' - `[.stat_table`: object to extract from
#' - `stat_table_meta_get`: object to extract `stat_table` meta data from
#' @param stratum_col_nms `[character]` (mandatory, no default)
#'
#' names of stratifying columns in `x`
#' @param value_col_nms `[character]` (mandatory, no default)
#'
#' names of columns in `x` containing the actual values (statistics)
#' @name stat_table
NULL


stat_table_meta_name <- function() {
  "stat_table_meta"
}

#' @rdname stat_table
#' @export
stat_table_meta_get <- function(x) {
  lapply(attr(x, stat_table_meta_name()), intersect, y = names(x))
}

stat_table_meta_set <- function(x, value) {
  data.table::setattr(x, name = stat_table_meta_name(), value = value)
}


stat_table_class_name <- function() "stat_table"

stat_table_class_set <- function(x) {
  data.table::setattr(
    x,
    "class",
    union(stat_table_class_name(), class(x))
  )
}

#' @rdname stat_table
#' @export
stat_table_set <- function(
  x,
  stratum_col_nms = character(0),
  value_col_nms = character(0)
) {
  dbc::assert_is_data_table(x)
  dbc::assert_is_character_nonNA_vector(stratum_col_nms)
  dbc::assert_is_character_nonNA_vector(value_col_nms)
  dbc::assert_vector_elems_are_in_set(stratum_col_nms, set = names(x))
  dbc::assert_vector_elems_are_in_set(value_col_nms, set = names(x))
  stat_table_class_set(x)
  stat_table_meta_set(x, mget(c("stratum_col_nms", "value_col_nms")))
  return(invisible(NULL))
}

#' @rdname stat_table
#' @export
stat_table <- function(
  x,
  stratum_col_nms = character(0),
  value_col_nms = character(0)
) {
  x <- data.table::copy(x)
  stat_table_set(x, stratum_col_nms, value_col_nms)
  x[]
}

#' @rdname stat_table
#' @export
as.stat_table <- function(
  x,
  stratum_col_nms = character(0),
  value_col_nms = character(0)
) {
  UseMethod("as.stat_table")
}

#' @rdname stat_table
#' @export
as.stat_table.data.table <- function(x, stratum_col_nms, value_col_nms) {
  stat_table(
    x = x, stratum_col_nms = stratum_col_nms, value_col_nms = value_col_nms
  )
}

#' @rdname stat_table
#' @export
print.stat_table <- function(x, ...) {
  if (!data.table::shouldPrint(x)) {
    ## when := is used in this fashion: x[, "my_col" := value]
    return(invisible(NULL))
  }
  meta <- stat_table_meta_get(x)
  meta <- lapply(meta, intersect, x = names(x))
  cat("* stat_table with\n")
  cat("*  - stratum_col_nms =", deparse(meta[["stratum_col_nms"]]), "\n")
  cat("*  - value_col_nms =", deparse(meta[["value_col_nms"]]), "\n")
  NextMethod()
}

#' @rdname stat_table
#' @export
#' @param ... passed to next method (see `?"["`)
#' @examples
#'
#' # basicepistats::`[.stat_table`
#' dt <- data.table::CJ(sex = 1:2, agegroup = 1:18)
#' dt[, "n" := sample(1e3L, .N)]
#' basicepistats::stat_table_set(dt, stratum_col_nms = c("sex", "agegroup"),
#'                               value_col_nms = "n")
#'
#' stopifnot(
#'   inherits(dt[1:3, ], class(dt)[1]),
#'   dt[1:3, sum(.SD[[1]]), .SDcols = "sex"] == 3
#' )
"[.stat_table" <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    stat_table_meta_set(y, stat_table_meta_get(x))
  }
  return(y)
}

#' @rdname stat_table
#' @export
#' @examples
#'
#' # basicepistats::setnames
#' dt <- data.table::CJ(sex = 1:2, agegroup = 1:18)
#' dt[, "n" := sample(1e3L, .N)]
#' basicepistats::stat_table_set(dt, stratum_col_nms = c("sex", "agegroup"),
#'                               value_col_nms = "n")
#' basicepistats::stat_table_setnames(dt, c("sex", "n"), c("my_sex", "my_n"))
#' stopifnot(
#'   c("my_sex", "my_n") %in% names(dt),
#'   "my_sex" %in% basicepistats::stat_table_meta_get(dt)[["stratum_col_nms"]],
#'   "my_n" %in% basicepistats::stat_table_meta_get(dt)[["value_col_nms"]]
#' )
stat_table_setnames <- function(
  x,
  old,
  new,
  skip_absent = NULL
) {
  # @codedoc_comment_block news("basicepistats::stat_table_setnames", "2025-01-27", "0.2.5")
  # New function `basicepistats::stat_table_setnames`. Rename columns while
  # also renaming them in the `stat_table`'s metadata.
  # @codedoc_comment_block news("basicepistats::stat_table_setnames", "2025-01-27", "0.2.5")
  if (is.null(skip_absent)) {
    skip_absent <- formals(data.table::setnames)[["skip_absent"]]
  }
  dbc::assert_is_data_table(x)
  nm_dt <- data.table::data.table(
    old = names(x)
  )
  #' @param new `[character]` (no default)
  #'
  #' See `[data.table::setnames]`.
  #' @param old `[character]` (no default)
  #'
  #' See `[data.table::setnames]`.
  #' @param skip_absent `[NULL, logical]` (default `NULL`)
  #'
  #' See `[data.table::setnames]`.
  #' - `NULL`: Use default defined in `[data.table::setnames]`.
  #' - `logical`: Use this.
  meta <- stat_table_meta_get(x)
  data.table::setnames(x = x, old = old, new = new, skip_absent = skip_absent)
  nm_dt[j = "new" := names(x)]
  nm_dt <- nm_dt[nm_dt[["old"]] != nm_dt[["new"]], ]
  meta <- lapply(meta, function(col_nm_set) {
    dt <- data.table::setDT(list(col_nm = col_nm_set))
    i.new <- NULL # for R CMD CHECK.
    dt[
      i = nm_dt,
      on = c(col_nm = "old"),
      j = "col_nm" := i.new
    ]
    dt[["col_nm"]]
  })
  stat_table_meta_set(x = x, value = meta)
  return(x[])
}
