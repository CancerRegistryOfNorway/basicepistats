#' @param melt `[NULL, list]` (default `NULL`)
#'
#' You may use this argument to easily melt
#' (see `[data.table::melt.data.table]`) stratifying columns of your choice.
#' E.g. `list(area = c("area_1", "area_2"))` to combine to area columns into
#' one.
#'
#' The table is re-aggregated if any melting is performed by summing the
#' value columns. This avoids duplicated strata in the table.
#'
#' - `NULL`: No melting performed.
#' - `list`: List of one or more elements, where each element is a character
#'   string vector of column names existing in the table.
