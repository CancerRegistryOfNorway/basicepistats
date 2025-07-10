
assert_is_arg_melt <- function(
    x,
    x_nm = NULL,
    call = NULL,
    assertion_type = "input",
    allowed_col_nms
) {
  call <- dbc::handle_arg_call(call)
  x_nm <- dbc::handle_arg_x_nm(x_nm)

  dbc::assert_is_one_of(
    x,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_uniquely_named_list),
    assertion_type = assertion_type
  )
  if (!is.null(x)) {
    lapply(names(x), function(nm) {
      dbc::assert_is_character_nonNA_vector(
        x = x[[nm]],
        x_nm = paste0(x_nm, "[[\"", nm, "\"]]"),
        assertion_type = assertion_type
      )
      dbc::assert_is(
        x = parse(text = paste0("x$", nm, " %in% ", deparse1(allowed_col_nms)))[[1]],
        x_nm = paste0(x_nm, "$", nm, " %in% ", deparse1(allowed_col_nms)),
        assertion_type = assertion_type
      )
    })
  }
  return(invisible(NULL))
}



#' @importFrom data.table .SD
melt_sum <- function(dt, melt = NULL) {
  if (is.null(melt)) {
    return(dt[])
  }
  stabli::stat_table_assert(dt)

  dt_meta <- stabli::stat_table_meta_get(dt)
  stratum_col_nms <- dt_meta[["stratum_col_nms"]]

  for (i in seq_along(melt)) {
    melt_col_nms <- melt[[i]]
    value_nm <- names(melt)[i]
    dt <- data.table::melt(
      dt,
      id.vars = setdiff(names(dt), melt_col_nms),
      measure.vars = melt_col_nms,
      value.name = value_nm,
      variable.name = ".__variable"
    )
    data.table::set(dt, j = ".__variable", value = NULL)
    data.table::setDT(dt)
    stratum_col_nms[stratum_col_nms %in% melt_col_nms] <- value_nm
    stratum_col_nms <- unique(stratum_col_nms)
  }
  value_col_nms <- intersect(dt_meta[["value_col_nms"]], names(dt))

  dt <- dt[
    j = lapply(.SD, sum),
    keyby = stratum_col_nms,
    .SDcols = value_col_nms
  ]

  stabli::stat_table_set(
    dt,
    list(
      stratum_col_nms = stratum_col_nms,
      value_col_nms = value_col_nms
    )
  )

  return(dt[])
}

