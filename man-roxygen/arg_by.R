
#' @param by `[NULL, data.table, list]` (optional, default `NULL`)
#'
#' - `NULL`: counts are produced using the levels observed in the
#'   dataset; see `subset_style` for the effect of subsetting
#' - `data.table`: describes the stratum column levels and the relationships
#'   between stratum columns; must have a column for all `stratum_col_nms`
#'   unless `stratum_col_nms` is `NULL`
#' - `list`: a named `list`, where each name must be a name of a column in `x`;
#'   each element of the `list` contains the set of levels for the correspoding
#'   column; the `list` is turned into a `data.table` and that is used as if
#'   you had supplied a `data.table`
#'
