




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



