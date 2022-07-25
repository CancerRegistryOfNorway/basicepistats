




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
#' with event within 1, 5, 10 years); the values in the column should be the
#' values at the start of follow-up (e.g. time of diagnosis)
#' @param follow_up_time_window_widths `[numeric]` (mandatory, default `Inf`)
#'
#' widhts of windowss in time scale given in `follow_up_time_col_nm`;
#' e.g. `c(1, 5, 10, Inf)` for windows of width 1, 5, 10 and window containing
#' all subjects
#' @template arg_subset
#' @template arg_subset_style
#' @template arg_assertion_type
#' @param observation_time_points `[vector]` (mandatory, no default)
#'
#' a vector of non-NA values of the same class as `x[[entry_time_col_nm]]`;
#' prevalence is observed at each time point supplied here;
#' e.g. `c(1999.999, 2000.999)` for the ends of years 1999 and 2000
#' in fractional years if `x[[entry_time_col_nm]]` contains calendar
#' time values in fractional years
#' @param entry_time_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` along which prevalence snapshots are taken; snapshots
#' are supplied via `observation_time_points`
#' @name prevalence
#' @details
#'
#' The following logic determines whether a record is prevalent at a specific
#' observation time point:
#'
#' ```
#' entry_time <= observation_time_point < entry_time + follow_up_time
#' ```
#'
#' Additionally, a record's entry to follow-up occurred no more than some
#' specified time ago if
#'
#' ```
#' (observation_time_point - entry_time) < follow_up_time_window_width
#' ```
#'
#' Therefore, e.g. the number of prevalent cases that entered follow-up no more
#' than 5 years ago from the time sligthly before midnight 1999-12-31 can be
#' found using fractional years data by
#'
#' ```
#' entry_time <= 1999.9999 < entry_time + follow_up_time
#' ```
#' and
#'
#' ```
#' 1999.9999 - entry_time < 5.0
#' ```

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
#'   observation_time_points = 2000.0,
#'   entry_time_col_nm = "calendar_time"
#' )
#'
stat_prevalent_record_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points,
  entry_time_col_nm,
  assertion_type = "input"
) {
  do.call(stat_prevalence_count, mget(names(formals(stat_prevalence_count))))
}

#' @importFrom data.table :=
stat_prevalence_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points,
  entry_time_col_nm,
  assertion_type = "input"
) {
  # assertions -----------------------------------------------------------------
  dbc::assert_is_character_nonNA_atom(follow_up_time_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_number_nonNA_vector(follow_up_time_window_widths,
                                     assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(entry_time_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_nonNA(observation_time_points, assertion_type = assertion_type)
  dbc::assert_is_vector(observation_time_points,
                        assertion_type = assertion_type)
  time_scale_names <- c(follow_up_time_col_nm, entry_time_col_nm)
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = time_scale_names,
    assertion_type = assertion_type
  )

  # handle by & subset ---------------------------------------------------------
  subset <- handle_subset_arg(dataset = x)
  by <- handle_by_arg(
    by = by, dataset = x, subset = subset, subset_style = subset_style
  )

  window_labels <- paste0("[0, ", follow_up_time_window_widths, ")")
  by <- list(
    by = by,
    time_since_entry = factor(window_labels, levels = window_labels)
  )
  if (is.null(by[["by"]])) {
    by["by"] <- NULL
  }
  by <- level_space_list_to_level_space_data_table(by)

  # working dataset ---------------------------------------------------------
  dt <- data.table::setDT(list(
    obs_t_start = x[[entry_time_col_nm]],
    fut = x[[follow_up_time_col_nm]]
  ))
  data.table::set(
    dt, j = "time_since_entry", value = by[["time_since_entry"]][1L]
  )
  data.table::set(
    dt, j = "obs_t_stop", value = dt[["obs_t_start"]] + dt[["fut"]]
  )
  stratum_col_nms <- setdiff(names(by), "time_since_entry")
  if (length(stratum_col_nms) > 0L) {
    data.table::set(
      dt,
      j = stratum_col_nms,
      value = mget(stratum_col_nms, envir = as.environment(x))
    )
  }

  # counts ---------------------------------------------------------------------
  fut_breaks <- c(0, follow_up_time_window_widths)
  count_dt <- data.table::rbindlist(
    lapply(seq_along(observation_time_points), function(i) {
      obs_t_point <- observation_time_points[i]
      data.table::set(
        x = dt,
        j = "in_follow_up_at_obs_t_point",
        value = data.table::between(
          x = obs_t_point,
          lower = dt[["obs_t_start"]], # [a, b] bounds
          upper = dt[["obs_t_stop"]],
          incbounds = TRUE
        )
      )
      data.table::set(
        x = dt,
        j = "time_since_entry",
        value = cut(
          as.numeric(obs_t_point - dt[["obs_t_start"]]),
          breaks = fut_breaks,
          right = FALSE,
          labels = window_labels
        )
      )
      select <- dt[["in_follow_up_at_obs_t_point"]]

      if (is.logical(subset)) {
        subset <- subset & select
      } else if (is.integer(subset)) {
        subset <- intersect(subset, which(select))
      } else if (is.null(subset)) {
        subset <- select
      } else {
        stop("internal error: subset not logical, integer, nor NULL")
      }
      count_dt <- stat_count_(
        x = dt, by = by, subset = subset, subset_style = subset_style
      )
      data.table::set(count_dt, j = entry_time_col_nm,
                      value = obs_t_point)
      count_dt[]

    })
  )

  # final touches --------------------------------------------------------------
  nonvalue_col_nms <- c(
    stratum_col_nms, entry_time_col_nm, "time_since_entry"
  )
  all_col_nms <- c(nonvalue_col_nms, "N")
  data.table::setcolorder(count_dt, all_col_nms)
  data.table::setkeyv(count_dt, nonvalue_col_nms)
  N <- NULL # to appease R CMD CHECK
  count_dt[
    j = "N" := cumsum(N),
    by = eval(setdiff(nonvalue_col_nms, "time_since_entry"))
    ]
  data.table::setnames(count_dt, entry_time_col_nm, "time_of_observation")
  return(count_dt[])
}


#' @rdname prevalence
#' @details
#' - `stat_prevalent_subject_count_`: counts subject prevalence; if one subject
#'  has multiple records, it is included in the counts only once and not
#'  as many times as it has records; this function intended for use inside other
#'  functions
#' @export
#' @examples
#' library("data.table")
#'
#' my_dataset <- data.table(
#'   id = c(1,1,2,2),
#'   sex = c(1,1,2,2),
#'   follow_up_time = c(2.5, 4.5),
#'   calendar_time = c(2001.5, 2000.5)
#' )
#' stat_prevalent_subject_count(
#'   x = my_dataset,
#'   follow_up_time_col_nm = "follow_up_time",
#'   follow_up_time_window_widths = c(1, 5, Inf),
#'   subject_id_col_nm = "id",
#'   by = "sex",
#'   observation_time_points = 2000.0,
#'   entry_time_col_nm = "calendar_time"
#' )
#'
stat_prevalent_subject_count_ <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  subject_id_col_nm,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points,
  entry_time_col_nm,
  assertion_type = "input"
) {
  dbc::assert_is_character_nonNA_atom(subject_id_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = subject_id_col_nm,
    assertion_type = assertion_type
  )
  select <- !duplicated(x, by = subject_id_col_nm)
  if (is.logical(subset)) {
    subset <- subset & select
  } else if (is.integer(subset)) {
    subset <- intersect(subset, which(select))
  } else if (is.null(subset)) {
    subset <- select
  } else {
    stop("internal error: subset not logical, integer, nor NULL")
  }
  do.call(stat_prevalence_count, mget(names(formals(stat_prevalence_count))))
}

#' @rdname prevalence
#' @details
#' - `stat_prevalent_subject_count`: counts subject prevalence; if one subject
#'  has multiple records, it is included in the counts only once and not
#'  as many times as it has records; this function intended for use directly
#'  by the end-user
#' @export
stat_prevalent_subject_count <- function(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  subject_id_col_nm,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points,
  entry_time_col_nm,
  assertion_type = "input"
) {
  dbc::assert_is_data.table(x, assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(follow_up_time_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_number_nonNA_vector(follow_up_time_window_widths,
                                     assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(subject_id_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(entry_time_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_data.table_with_required_names(
    x,
    required_names = c(follow_up_time_col_nm, subject_id_col_nm,
                       entry_time_col_nm),
    assertion_type = assertion_type
  )
  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)

  call_with_arg_list("stat_prevalent_subject_count_")
}


#' @importFrom data.table := .EACHI .SD
stat_year_based_prevalence_count__ <- function(
  x,
  entry_year_col_nm,
  exit_year_col_nm,
  observation_years,
  maximum_follow_up_years = c(1L, 3L, 5L, 1e3L),
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  verbose = FALSE,
  assertion_type = "prod_input"
) {

  # @codedoc_comment_block stat_year_based_prevalence_count__
  # Year-based prevalence counts are computed in the (internal, not intended
  # for users) function `stat_year_based_prevalence_count__`
  # @codedoc_comment_block stat_year_based_prevalence_count__

  # assertions -----------------------------------------------------------------
  whole_run_start_time <- proc.time()
  assertion_start_time <- proc.time()
  dbc::assert_is_character_nonNA_atom(entry_year_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_character_nonNA_atom(exit_year_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_integer_nonNA_vector(maximum_follow_up_years,
                                      assertion_type = assertion_type)
  dbc::assert_is_integer_nonNA_vector(observation_years,
                                      assertion_type = assertion_type)
  time_scale_names <- c(entry_year_col_nm, exit_year_col_nm)
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = time_scale_names,
    assertion_type = assertion_type
  )
  if (verbose) {
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "assertions done; ", data.table::timetaken(assertion_start_time))
  }

  # handle by & subset ---------------------------------------------------------
  by_subset_start_time <- proc.time()
  subset <- handle_subset_arg(dataset = x)
  if (is.null(subset)) {
    subset <- rep(TRUE, nrow(x))
  }
  by <- handle_by_arg(
    by = by, dataset = x, subset = subset, subset_style = subset_style
  )
  if (verbose) {
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "handle by & subset done; ",
            data.table::timetaken(by_subset_start_time))
  }

  # working dataset ------------------------------------------------------------
  working_dataset_start_time <- proc.time()
  dt <- data.table::setDT(list(
    entry_year = x[[entry_year_col_nm]],
    exit_year = x[[exit_year_col_nm]]
  ))
  stratum_col_nms <- setdiff(names(by), names(dt))
  if (length(stratum_col_nms) > 0L) {
    data.table::set(
      dt,
      j = stratum_col_nms,
      value = mget(stratum_col_nms, envir = as.environment(x))
    )
    data.table::setcolorder(dt, c(stratum_col_nms, "entry_year", "exit_year"))
  }
  dt <- dt[
    i = subset,
    j = .N,
    keyby = names(dt)
  ]
  if (verbose) {
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "working dataset done; ",
            data.table::timetaken(working_dataset_start_time))
  }

  # counts ---------------------------------------------------------------------
  counts_start_time <- proc.time()
  window_labels <- paste0("0 - ", maximum_follow_up_years - 1L)
  window_labels[window_labels == "0 - 0"] <- "0"
  fut_breaks <- c(0, maximum_follow_up_years)
  join_dt <- level_space_list_to_level_space_data_table(
    list(by = by,
         in_follow_up_at_obs_y = TRUE,
         full_years_since_entry = factor(window_labels, labels = window_labels))
  )
  output <- lapply(seq_along(observation_years), function(i) {
    # @codedoc_comment_block stat_year_based_prevalence_count__
    # Prevalence counts are computed by looping over `observation_years`.
    # For each observation year, we identify observations in `x` which are still
    # in follow-up at the last microsecond of that year. This is done by testing
    # whether `entry_year <= observation_year <= exit_year + 1L`.
    # @codedoc_comment_block stat_year_based_prevalence_count__
    observation_year <- observation_years[i]
    data.table::set(
      x = dt,
      j = "in_follow_up_at_obs_y",
      value = data.table::between( # [a, b[ bounds
        observation_year,
        dt[["entry_year"]],
        dt[["exit_year"]] - 1L,
        incbounds = TRUE
      )
    )
    # @codedoc_comment_block stat_year_based_prevalence_count__
    # The number of years survived before the observation year
    # (`full_years_since_entry`) is identified for each case via a call to
    # `[cut]`, which categorises the survival times based on what was supplied
    # to arg `maximum_follow_up_years`.
    # @codedoc_comment_block stat_year_based_prevalence_count__
    data.table::set(
      x = dt,
      j = "full_years_since_entry",
      value = cut(
        observation_year - dt[["entry_year"]],
        breaks = fut_breaks,
        right = FALSE,
        labels = window_labels
      )
    )
    # @codedoc_comment_block stat_year_based_prevalence_count__
    # Then we simply count the number of cases for each stratum
    # (`stratum_col_nms`) and `full_years_since_entry` category for the current
    # observation year.
    # @codedoc_comment_block stat_year_based_prevalence_count__
    count_dt <- dt[
      i = join_dt,
      on = names(join_dt),
      j = list(N = sum(.SD[[1L]])), # note: NA in N means no such rows in dt.
      .SDcols = "N",
      keyby = .EACHI
    ]
    data.table::set(count_dt, j = "observation_year", value = observation_year)
    data.table::set(count_dt, j = "in_follow_up_at_obs_y", value = NULL)
    count_dt[]
  })
  output <- data.table::rbindlist(output)
  output[is.na(output[["N"]]), "N" := 0L]
  if (verbose) {
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "counts done; ", data.table::timetaken(counts_start_time))
  }
  # @codedoc_comment_block stat_year_based_prevalence_count__
  # The result is a table of counts by `observation_year`,
  # `full_years_since_entry`, and any user-requested stratifying columns.
  # @codedoc_comment_block stat_year_based_prevalence_count__

  # final touches --------------------------------------------------------------
  final_touches_start_time <- proc.time()
  nonvalue_col_nms <- union(
    stratum_col_nms, c("observation_year", "full_years_since_entry")
  )
  all_col_nms <- c(nonvalue_col_nms, "N")
  data.table::setcolorder(output, all_col_nms)
  data.table::setkeyv(output, nonvalue_col_nms)
  output[
    j = "N" := cumsum(.SD[[1L]]),
    .SDcols = "N",
    by = eval(setdiff(nonvalue_col_nms, "full_years_since_entry"))
  ]
  if (verbose) {
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "final touches done; ",
            data.table::timetaken(final_touches_start_time))
    message("* basicepistats:::stat_year_based_prevalence_count__: ",
            "whole run done; ", data.table::timetaken(whole_run_start_time))
  }
  return(output[])
}




codedoc_stat_year_based_prevalence_count <- function() {
  requireNamespace("codedoc")
  re <- "year_based_prevalence"
  lines <- c(
    "@section Under the hood:",
    codedoc::codedoc_lines(re)
  )
  return(lines)
}



#' @title Year-Based Prevalence
#' @name year_based_prevalence
#' @description
#' Compute prevalent records and subjects using only year-level information.
#'
#' @param x `[data.table]` (mandatory, no default)
#'
#' dataset containing one or more records by subject
#' @template arg_by
#' @param entry_year_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` for the year of entry into follow-up
#' @param exit_year_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` for the year of exit from follow-up
#' @param observation_years `[observation_years]` (mandatory, no default)
#'
#' vector of years; the number of prevalent cases / subjects is computed
#' for the **ends** of these years; e.g. if a person exits follow-up in the
#' year of observation, this presumably occurs before the last millisecond
#' of that year, and is **not** considered to be prevalent for that
#' `observation_years` value; see also **Details**
#' @param maximum_follow_up_years `[integer]`
#' (mandatory, default `c(1L, 3L, 5L, 1e3L)`)
#'
#' each element of `maximum_follow_up_years` defines the upper limit of a window
#' of follow-up; this is intended to stratify (or filter out) observations
#' based on how long ago they entered follow-up relative to an individual
#' point of observation (see `observation_years`); each element of this argument
#' is a exclusive upper limit of an interval (`[a, b[`); e.g. with
#' `observation_years = 2010L` and `maximum_follow_up_years = 1L` an observation
#' is only considered prevalent if it entered follow-up in 2010 (not 2009,
#' and certainly not 2011); see also **Details**
#' @template arg_subset
#' @template arg_subset_style
#' @template arg_assertion_type
#' @details
#'
#' The following logic determines whether a record is prevalent at a specific
#' observation year:
#'
#' ```
#' entry_year <= observation_year < exit_year
#' ```
#'
#' Additionally, e.g. one-year prevalence is computed by supplying
#' `maximum_follow_up_years = 1L`. To generalise, an observation belongs to
#' the n-year prevalence group if
#'
#' ```
#' (observation_year - entry_year) < n
#' ```
#'
#' Therefore, e.g. an observation is in the one-year prevalence group if
#' `observation_year - entry_year  < 1`, e.g. `2020 - 2020 = 0 < 1`.
#'
#'
#' @eval codedoc_stat_year_based_prevalence_count()
#'
#' @examples
#' library("data.table")
#' my_dataset <- data.table::data.table(
#'   id = 1:4,
#'   sex = c(1L, 1L, 2L, 2L),
#'   entry_year = 2000:2003,
#'   exit_year = 2000:2003 + 1:4
#' )
#'
#' # NOTE: person that entered in 2003 _is_ counted, person that left in 2003
#' # is _not_ counted
#' basicepistats::stat_year_based_prevalent_record_count(
#'   x = my_dataset,
#'   entry_year_col_nm = "entry_year",
#'   exit_year_col_nm = "exit_year",
#'   maximum_follow_up_years = c(1L, 3L, 5L, 100L),
#'   by = "sex",
#'   observation_years = 2003L
#' )
#'
#' @export
stat_year_based_prevalent_record_count <- function(
  x,
  entry_year_col_nm,
  exit_year_col_nm,
  observation_years,
  maximum_follow_up_years = c(1L, 3L, 5L, 1e3L),
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  assertion_type = "input"
) {
  verbose <- FALSE
  call_with_arg_list("stat_year_based_prevalence_count__")
}





#' @rdname year_based_prevalence
#' @details
#' - `stat_year_based_prevalent_subject_count`: intended for use directly by
#'   the end-user
#' @export
stat_year_based_prevalent_subject_count <- function(
  x,
  entry_year_col_nm,
  exit_year_col_nm,
  observation_years,
  subject_id_col_nm,
  maximum_follow_up_years = c(1L, 3L, 5L, 1e3L),
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  assertion_type = "input"
) {
  dbc::assert_is_data.table(x)
  dbc::assert_is_character_nonNA_atom(subject_id_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_data.table_with_required_names(
    x,
    required_names = c(subject_id_col_nm, entry_year_col_nm, exit_year_col_nm),
    assertion_type = assertion_type
  )
  dbc::assert_is_integer_nonNA_vector(observation_years,
                                      assertion_type = assertion_type)
  dbc::assert_is_integer_nonNA_gtzero_vector(maximum_follow_up_years,
                                             assertion_type = assertion_type)
  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)

  call_with_arg_list("stat_year_based_prevalent_subject_count_")
}

#' @param subject_id_col_nm `[character]` (mandatory, no default)
#'
#' name of column in `x` which identifies subjects; one subject
#' may have one or more rows in `x`
#' @rdname year_based_prevalence
#' @details
#' - `stat_year_based_prevalent_subject_count_`: intended for use inside other
#'   functions
#' @export
#' @importFrom data.table .SD
stat_year_based_prevalent_subject_count_ <- function(
  x,
  entry_year_col_nm,
  exit_year_col_nm,
  observation_years,
  subject_id_col_nm,
  maximum_follow_up_years = c(1L, 3L, 5L, 1e3L),
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  assertion_type = "input"
) {
  dbc::assert_is_character_nonNA_atom(subject_id_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_data_table_with_required_names(
    x,
    required_names = c(entry_year_col_nm, exit_year_col_nm, subject_id_col_nm),
    assertion_type = assertion_type
  )
  assert_is_arg_by(by, assertion_type = assertion_type)
  assert_is_arg_subset(subset, nrow(x), assertion_type = assertion_type)
  assert_is_arg_subset_style(subset_style, assertion_type = assertion_type)

  subset <- local({
    subset <- handle_subset_arg(dataset = x)
    if (is.null(subset)) {
      subset <- rep(TRUE, nrow(x))
    }
    tmp_dt <- x[, .SD, .SDcols = c(subject_id_col_nm, entry_year_col_nm)]
    tmp_dt[, ".__row_number" := 1:nrow(tmp_dt)]
    tmp_dt <- tmp_dt[subset, ]
    data.table::setkeyv(tmp_dt, c(subject_id_col_nm, entry_year_col_nm))
    wh_earliest_record_by_subject <- tmp_dt[[".__row_number"]][
      !duplicated(tmp_dt, by = subject_id_col_nm)
      ]

    if (is.logical(subset)) {
      subset <- intersect(which(subset), wh_earliest_record_by_subject)
    } else if (is.integer(subset)) {
      subset <- intersect(subset, wh_earliest_record_by_subject)
    } else if (is.null(subset)) {
      subset <- wh_earliest_record_by_subject
    } else {
      stop("internal error: subset not logical, integer, nor NULL")
    }

    subset
  })

  verbose <- FALSE
  call_with_arg_list("stat_year_based_prevalence_count__")
}







