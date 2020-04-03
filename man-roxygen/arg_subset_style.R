




#' @param subset_style `[character]` (optional, default `"zeros"`)
#'
#' - `"zeros"`: every combination given in `joint_column_level_space`
#'   (or existing in data _before_ subsetting, if `joint_column_level_space`
#'   is `NULL`) will be included in output, but strata outside the subset
#'   will have zero counts
#' - `"drop"`: output will not have strata that are not in the subset
#'   (zero counts are impossible)




