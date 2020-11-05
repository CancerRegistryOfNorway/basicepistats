




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
  entry_time_col_nm
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
  entry_time_col_nm
) {
  # assertions -----------------------------------------------------------------
  dbc::assert_prod_input_is_character_nonNA_atom(follow_up_time_col_nm)
  dbc::assert_prod_input_is_number_nonNA_vector(follow_up_time_window_widths)
  dbc::assert_prod_input_is_character_nonNA_atom(entry_time_col_nm)
  dbc::assert_prod_input_is_nonNA(observation_time_points)
  dbc::assert_prod_input_is_vector(observation_time_points)
  time_scale_names <- c(follow_up_time_col_nm, entry_time_col_nm)
  dbc::assert_prod_input_is_data_table_with_required_names(
    x,
    required_names = time_scale_names
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
  entry_time_col_nm
) {
  dbc::assert_prod_input_is_character_nonNA_atom(subject_id_col_nm)
  dbc::assert_prod_input_is_data_table_with_required_names(
    x,
    required_names = subject_id_col_nm
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
  entry_time_col_nm
) {
  dbc::assert_user_input_is_data.table(x)
  dbc::assert_user_input_is_character_nonNA_atom(follow_up_time_col_nm)
  dbc::assert_user_input_is_number_nonNA_vector(follow_up_time_window_widths)
  dbc::assert_user_input_is_character_nonNA_atom(subject_id_col_nm)
  dbc::assert_user_input_is_character_nonNA_atom(entry_time_col_nm)
  dbc::assert_user_input_is_data.table_with_required_names(
    x,
    required_names = c(follow_up_time_col_nm, subject_id_col_nm,
                       entry_time_col_nm)
  )
  assert_user_input_by(by)
  assert_user_input_subset(subset, nrow(x))
  assert_user_input_subset_style(subset_style)

  call_with_arg_list("stat_prevalent_subject_count_")
}


#' @importFrom data.table := .EACHI
stat_year_based_prevalence_count <- function(
  x,
  entry_year_col_nm,
  exit_year_col_nm,
  observation_years,
  maximum_follow_up_years = c(1L, 3L, 5L, 1e3L),
  by = NULL,
  subset = NULL,
  subset_style = "zeros"
) {

  # @codedoc_comment_block stat_year_based_prevalence_count
  # Year-based prevalence counts are computed in the (internal, not intended
  # for users) function stat_year_based_prevalence_count.
  # @codedoc_comment_block stat_year_based_prevalence_count

  # assertions -----------------------------------------------------------------
  dbc::assert_prod_input_is_character_nonNA_atom(entry_year_col_nm)
  dbc::assert_prod_input_is_character_nonNA_atom(exit_year_col_nm)
  dbc::assert_prod_input_is_integer_nonNA_vector(maximum_follow_up_years)
  dbc::assert_prod_input_is_integer_nonNA_vector(observation_years)
  time_scale_names <- c(entry_year_col_nm, exit_year_col_nm)
  dbc::assert_prod_input_is_data_table_with_required_names(
    x,
    required_names = time_scale_names
  )

  # handle by & subset ---------------------------------------------------------
  subset <- handle_subset_arg(dataset = x)
  by <- handle_by_arg(
    by = by, dataset = x, subset = subset, subset_style = subset_style
  )

  window_labels <- paste0("0 - ", maximum_follow_up_years - 1L)
  window_labels[window_labels == "0 - 0"] <- "0"
  by_year <- data.table::data.table(
    entry_year = unique(x[[entry_year_col_nm]])
  )
  max_exit_year <- max(x[[exit_year_col_nm]])
  entry_year <- NULL # to appease R CMD CHECK
  by_year <- by_year[
    j = list(exit_year = min(entry_year + 1L, max_exit_year):max_exit_year),
    keyby = "entry_year"
  ]
  by <- list(
    by = by,
    by_year
  )
  if (is.null(by[["by"]])) {
    by["by"] <- NULL
  }
  by <- level_space_list_to_level_space_data_table(by)

  # working dataset ---------------------------------------------------------
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
  }

  # overall counts -------------------------------------------------------------
  # @codedoc_comment_block stat_year_based_prevalence_count
  # Year-based prevalence is based on the year of entry and exit.
  # stat_year_based_prevalence_count first computes counts by the year of entry,
  # year of exit, and any additional stratifying columns the user requests.
  # E.g.
  #
  # | entry_year| exit_year|  N|
  # |----------:|---------:|--:|
  # |       2000|      2001|  5|
  # |       2000|      2002| 55|
  # |       2001|      2002| 60|
  # @codedoc_comment_block stat_year_based_prevalence_count
  overall_count_dt <- stat_count_(
    x = dt,
    by = by,
    subset = subset,
    subset_style = subset_style
  )
  rm(list = "dt")

  # counts ---------------------------------------------------------------------
  fut_breaks <- c(0, maximum_follow_up_years)
  data.table::set(by, j = "in_follow_up_at_obs_y", value = TRUE)
  join_dt_col_nms <- setdiff(names(by), c("entry_year", "exit_year"))
  join_dt <- by[
    !duplicated(by, by = join_dt_col_nms),
    .SD,
    .SDcols = join_dt_col_nms
  ]
  join_dt <- level_space_list_to_level_space_data_table(
    list(by = join_dt, full_years_since_entry = window_labels)
  )
  join_dt_col_nms <- names(join_dt)
  count_dt <- data.table::rbindlist(
    lapply(seq_along(observation_years), function(i) {

      # @codedoc_comment_block stat_year_based_prevalence_count
      # Next the table of counts will be further aggregated suitably for each
      # observation_year value (year of observation). E.g. for year of
      # observation 2001, those who left follow-up in 2001 or in earlier years
      # will not be prevalent for that year of observation. Also the number of
      # full years since entry is identified at this point. E.g. for year of
      # observation 2001, rows of the count table with entry_year 2000 have
      # one full year. The appropriate categories of full_years_since_entry
      # are identified and those rows in the count table still "in follow-up"
      # are retained for further aggregating the counts (summing over
      # entry_year and exit_year, but by full_years_since_entry,
      # observation_year and any
      # stratifying columns requested by the user).
      # @codedoc_comment_block stat_year_based_prevalence_count
      obs_y <- observation_years[i]
      data.table::set(
        x = overall_count_dt,
        j = "in_follow_up_at_obs_y",
        value = data.table::between( # [a, b[ bounds
          obs_y,
          overall_count_dt[["entry_year"]],
          overall_count_dt[["exit_year"]] - 1L,
          incbounds = TRUE
        )
      )
      data.table::set(
        x = overall_count_dt,
        j = "full_years_since_entry",
        value = cut(
          obs_y - overall_count_dt[["entry_year"]],
          breaks = fut_breaks,
          right = FALSE,
          labels = window_labels
        )
      )

      count_dt <- overall_count_dt[
        i = join_dt,
        on = join_dt_col_nms,
        j = lapply(.SD, sum),
        .SDcols = "N",
        keyby = .EACHI
      ]
      count_dt[is.na(count_dt[["N"]]), "N" := 0L]
      data.table::set(count_dt, j = "observation_year", value = obs_y)
      data.table::set(count_dt, j = "in_follow_up_at_obs_y", value = NULL)
      count_dt[]
    })
  )

  # @codedoc_comment_block stat_year_based_prevalence_count
  # The result is a table of counts by observation_year and
  # full_years_since_entry (+ user-requested strata).
  # @codedoc_comment_block stat_year_based_prevalence_count

  # final touches --------------------------------------------------------------
  nonvalue_col_nms <- union(
    stratum_col_nms, c("observation_year", "full_years_since_entry")
  )
  all_col_nms <- c(nonvalue_col_nms, "N")
  data.table::setcolorder(count_dt, all_col_nms)
  data.table::setkeyv(count_dt, nonvalue_col_nms)
  N <- NULL # to appease R CMD CHECK
  count_dt[
    j = "N" := cumsum(N),
    by = eval(setdiff(nonvalue_col_nms, "full_years_since_entry"))
  ]
  return(count_dt[])
}




codedoc_stat_year_based_prevalence_count <- function() {
  requireNamespace("codedoc")

  df <- codedoc::extract_keyed_comment_blocks(
    text_file_paths = "R/prevalence.R"
  )
  df <- df[grepl("year_based_prevalence", df$key), ]

  lines <- unlist(lapply(df[["comment_block"]], function(obj) {
    c("", obj, "")
  }))
  return(c("@section Under the hood:", lines))
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
  subset_style = "zeros"
) {
  call_with_arg_list("stat_year_based_prevalence_count")
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
  subset_style = "zeros"
) {
  dbc::assert_user_input_is_data.table(x)
  dbc::assert_user_input_is_character_nonNA_atom(subject_id_col_nm)
  dbc::assert_user_input_is_data.table_with_required_names(
    x,
    required_names = c(subject_id_col_nm, entry_year_col_nm, exit_year_col_nm)
  )
  dbc::assert_user_input_is_integer_nonNA_vector(observation_years)
  dbc::assert_user_input_is_integer_nonNA_gtzero_vector(maximum_follow_up_years)
  assert_user_input_by(by)
  assert_user_input_subset(subset, nrow(x))
  assert_user_input_subset_style(subset_style)

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
  subset_style = "zeros"
) {
  dbc::assert_prod_input_is_character_nonNA_atom(subject_id_col_nm)
  dbc::assert_prod_input_is_data_table_with_required_names(
    x,
    required_names = c(entry_year_col_nm, exit_year_col_nm, subject_id_col_nm)
  )
  assert_prod_input_by(by)
  assert_prod_input_subset(subset, nrow(x))
  assert_prod_input_subset_style(subset_style)

  subset <- handle_subset_arg(dataset = x)
  if (is.null(subset)) {
    subset <- rep(TRUE, nrow(x))
  }

  subset <- local({
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

  call_with_arg_list("stat_year_based_prevalence_count")
}







