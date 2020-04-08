




#' @param subset_style `[character]` (optional, default `"zeros"`)
#'
#' - `"zeros"`: every combination of stratifying columns supplied via `by`
#'   (or existing in data _before_ subsetting, if `by`
#'   is `NULL`) will be included in output, but strata outside the subset
#'   will have zero counts
#' - `"drop"`: output will not have strata that are not in the subset
#'   (zero counts are impossible)




