




#' @title Prevalence
#' @description
#' Compute subject prevalence at specific time points and optionally
#' conditioning on time since event at the time points.
#' @param x `[data.table]` (mandatory, no default)
#'
#' dataset containing one or more records by subject
#' @template arg_by
#' @param subject_id_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` which identifies subjects; one subject
#' may have one or more rows in `x`
#' @param follow_up_time_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` which specifies values in the time scale in which
#' prevalence follow-up windows are determined (e.g. prevalence of subjects
#' with event within 1, 5, 10 years)
#' @param follow_up_time_window_widths `[numeric]` (mandatory, default `Inf`)
#'
#' widhts of windowss in time scale given in `follow_up_time_col_nm`;
#' e.g. `c(1, 5, 10, Inf)` for windows of width 1, 5, 10 and window containing
#' all subjects
#' @param stratum_col_nms `[character, NULL]` (optional, default `NULL`)
#'
#' passed to [stat_count].
#' - `character`: prevalence counts are stratified by these stratifying columns
#' - `NULL`: no stratification
#' @template arg_subset
#' @template arg_subset_style
#' @param observation_time_points `[list]` (mandatory, no default)
#'
#' a named list, where the names correspond to time scales in `x` and the
#' elements are points in the time scale where prevalence counts are intended
#' to be observed; e.g. `list(date = as.Date("1999-12-31"))` observes
#' prevalences on the last day of 1999. multiple time scales can be supplied,
#' whereupon prevalences are observed along the different time scales
#' independently of each other, e.g.
#' `list(date = as.Date("1999-12-31"), age = 60 * 365.25)`
#' observes prevalence at the end of 1999 and separately at the 60th birthday
#' and NOT instances that turned 60 at the end of 1999.
#' @name prevalence
NULL

#' @rdname prevalence
#' @details
#' - `stat_prevalent_record_count`: counts record prevalence; if one subject
#'   has more than one records in the dataset, all their records are included
#'   in the counts
#' @export
#' @examples
#' library("data.table")
#'
#' my_dataset <- data.table(
#'   sex = 1:2,
#'   follow_up_time = c(2.5, 4.5),
#'   calendar_time = c(2001.5, 2000.5)
#' )
#' stat_prevalent_record_count(
#'   x = my_dataset,
#'   follow_up_time_col_nm = "follow_up_time",
#'   follow_up_time_window_widths = c(1, 5, Inf),
#'   by = "sex",
#'   observation_time_points = list(calendar_time = 2000.0)
#' )
#'
stat_prevalent_record_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points
) {
  unique_by <- NULL
  do.call(stat_prevalence_count, mget(names(formals(stat_prevalence_count))))
}


stat_prevalence_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  by = NULL,
  unique_by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points
) {
  # assertions -----------------------------------------------------------------
  easyassertions::assert_is_character_nonNA_atom(follow_up_time_col_nm)
  easyassertions::assert_is_number_nonNA_vector(follow_up_time_window_widths)
  time_scale_names <- c(follow_up_time_col_nm, names(observation_time_points))
  easyassertions::assert_is_data.table_with_required_names(
    x,
    required_names = time_scale_names
  )
  easyassertions::assert_is_one_of(
    unique_by,
    fun_nms = c("assert_is_character_nonNA_vector", "assert_is_NULL")
  )

  # create a Lexis data.table --------------------------------------------------
  reserved_col_nms <- c(
    "lex.dur", "lex.id", "lex.Cst", "lex.Xst", ".__fut_window"
  )
  keep_col_nms <- setdiff(names(x), reserved_col_nms)
  dt <- data.table::setDT(mget(keep_col_nms, as.environment(x)))
  data.table::setattr(dt, "class", c("Lexis", "data.table", "data.frame"))
  data.table::setattr(dt, "time.scales", time_scale_names)
  old_breaks_list <- structure(vector("list", length(time_scale_names)),
                               names = time_scale_names)
  data.table::setattr(dt, "breaks", old_breaks_list)
  dt[, (reserved_col_nms) := NA_integer_]
  dt[, "lex.dur" := .SD[[1]], .SDcols = follow_up_time_col_nm]
  lapply(time_scale_names, function(ts_nm) {
    dt[
      j = (ts_nm) := .SD[[1L]] - .SD[[2]],
      .SDcols = c(ts_nm, "lex.dur")
      ]
  })
  dt[, "lex.id" := 1:.N]

  # split ----------------------------------------------------------------------
  dt <- popEpi::splitMulti(
    data = dt,
    breaks = observation_time_points,
  )

  # handle subset, by ----------------------------------------------------------
  subset <- handle_subset_arg(dataset = dt)
  is_at_an_observation_point <- rowSums(
    data.table::setDT(lapply(names(observation_time_points), function(ts_nm) {
      time_points <- observation_time_points[[ts_nm]]
      dt[[ts_nm]] %in% time_points
    }))
  ) > 0L
  if (is.logical(subset)) {
    subset <- subset & is_at_an_observation_point
  } else if (is.integer(subset)) {
    subset <- intersect(subset, which(is_at_an_observation_point))
  } else {
    subset <- is_at_an_observation_point
  }
  by <- handle_by_arg(dataset = dt, subset = subset, by = by)
  dt <- dt[subset, ]

  # create follow-up time windows ----------------------------------------------
  fut_window_breaks <- sort(union(0L, follow_up_time_window_widths))
  fut_window_labels <- paste0("[0, ", fut_window_breaks[-1], ")")
  data.table::set(
    dt,
    j = ".__fut_window",
    value = cut(
      x = dt[[follow_up_time_col_nm]],
      breaks = fut_window_breaks,
      labels = fut_window_labels,
      right = FALSE
    )
  )

  # tabulate counts ------------------------------------------------------------
  by <- list(
    by = by,
    .__fut_window = factor(seq_along(fut_window_labels),
                           labels = fut_window_labels)
  )
  if (is.null(unique_by)) {
    count_dt <- stat_count(
      x = dt, by = by, subset_style = subset_style
    )
  } else {
    count_dt <- stat_unique_count(
      x = dt, by = by, subset_style = subset_style,
      unique_by = unique_by
    )
  }

  stratum_col_nms <- setdiff(names(count_dt), "N")
  data.table::setkeyv(count_dt, stratum_col_nms)
  count_dt[
    j = "N" := cumsum(N),
    by = eval(setdiff(stratum_col_nms, ".__fut_window"))
    ]

  return(count_dt[])
}


#' @rdname prevalence
#' @details
#' - `stat_prevalent_subject_count`: counts subject prevalence; if one subject
#'  has multiple records, it is included in the counts only once and not
#'  as many times as it has records
#' @export
#' @examples
#' library("data.table")
#'
#' my_dataset <- data.table(
#'   id = 1:2,
#'   sex = 1:2,
#'   follow_up_time = c(2.5, 4.5),
#'   calendar_time = c(2001.5, 2000.5)
#' )
#' stat_prevalent_subject_count(
#'   x = my_dataset,
#'   follow_up_time_col_nm = "follow_up_time",
#'   follow_up_time_window_widths = c(1, 5, Inf),
#'   subject_id_col_nm = "id",
#'   by = "sex",
#'   observation_time_points = list(calendar_time = 2000.0)
#' )
#'
stat_prevalent_subject_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  subject_id_col_nm,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points
) {
  easyassertions::assert_is_character_nonNA_atom(subject_id_col_nm)
  easyassertions::assert_is_data.table_with_required_names(
    x,
    required_names = subject_id_col_nm
  )
  unique_by <- subject_id_col_nm
  do.call(stat_prevalence_count, mget(names(formals(stat_prevalence_count))))
}






