




enforce_level_space <- function(
  x,
  value_col_nms,
  fill = 0L,
  column_level_space
) {
  if (length(fill) == 1L) {
    fill <- rep(fill, length(value_col_nms))
  }
  names(fill) <- value_col_nms
  x <- x[
    i = column_level_space,
    on = names(column_level_space)
  ]
  lapply(value_col_nms, function(value_col_nm) {

    is_na <- is.na(x[[value_col_nm]])
    data.table::set(x, i = which(is_na), value = fill[value_col_nm])

  })

  x[]
}



