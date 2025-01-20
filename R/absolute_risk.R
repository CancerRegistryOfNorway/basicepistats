

#' @title Absolute Risk Curve
#' @description
#' Tools to estimate absolute risk curves. Absolute risk is also known as
#' crude risk and sometimes "cumulative incidence function" due to the
#' cumulative hazard rate estimation method. An absolute risk curve shows e.g.
#' the statistical probability in some population of developing cancer
#' (getting the first cancer diagnosis) over years of age.
#' @name absolute_risk_curve_tools

#' @rdname absolute_risk_curve_tools
#' @eval stat_absolute_risk_docs()
#' @examples
#' dt <- data.table::data.table(
#'   age_group_start = 0:100,
#'   age_group_stop  = c(1:100, 120L),
#'   cancer_count    = 1:101,
#'   death_count     = (1:101 + 10L),
#'   pyrs            = rep(1e4, 101)
#' )
#' dt <- rbind(data.table::copy(dt)[, "sex" := 1L],
#'             data.table::copy(dt)[, "sex" := 2L])
#' dt[dt[["sex"]] == 2L, "cancer_count" := c(rep(1:10, each = 10L), 5L)]
#' ardt <- basicepistats::stat_absolute_risk(
#'   x                     = dt,
#'   stratum_col_nms       = "sex",
#'   interval_start_col_nm = "age_group_start",
#'   interval_stop_col_nm  = "age_group_stop",
#'   event_count_col_nms   = c(cancer = "cancer_count", death = "death_count"),
#'   at_risk_time_col_nm   = "pyrs"
#' )
#'
#' # overall survival and event-specific absolute risks sum to one!
#' stopifnot(all.equal(
#'   ardt[["overall_survival"]] + ardt[["cancer_absolute_risk"]] +
#'     ardt[["death_absolute_risk"]],
#'   rep(1.0, nrow(ardt))
#' ))
#'
#' plot( cancer_absolute_risk ~ age_group_stop,
#'       data = ardt[ardt[["sex"]] == 1L, ], col = "black", type = "l",
#'       ylim = c(0, max(ardt[["cancer_absolute_risk"]])))
#' lines(cancer_absolute_risk ~ age_group_stop,
#'       data = ardt[ardt[["sex"]] == 2L, ], col = "red")
#'
#' plot( death_absolute_risk ~ age_group_stop,
#'       data = ardt[ardt[["sex"]] == 1L, ], col = "black", type = "l",
#'       ylim = c(0, max(ardt[["death_absolute_risk"]])))
#' lines(death_absolute_risk ~ age_group_stop,
#'       data = ardt[ardt[["sex"]] == 2L, ], col = "red")
#'
#' # this example mostly for testing that basicepistats::stat_absolute_risk
#' # works as intended. in this example, 5 people all get cancer and none die.
#' # one gets cancer in the middle of each interval.
#' dt <- data.table::data.table(
#'   age_group_start = 0:4 * 20L,
#'   age_group_stop  = 1:5 * 20L,
#'   cancer_count    = rep(1L, 5L),
#'   death_count     = rep(0L, 5L),
#'   pyrs            = c(100, 80, 60, 40, 20) - 10
#' )
#' ardt <- basicepistats::stat_absolute_risk(
#'   x                     = dt,
#'   interval_start_col_nm = "age_group_start",
#'   interval_stop_col_nm  = "age_group_stop",
#'   event_count_col_nms   = c(cancer = "cancer_count", death = "death_count"),
#'   at_risk_time_col_nm   = "pyrs"
#' )
#' @export
stat_absolute_risk <- function(
  x,
  stratum_col_nms = NULL,
  interval_start_col_nm,
  interval_stop_col_nm,
  event_count_col_nms,
  at_risk_time_col_nm,
  assertion_type = "input"
) {
  # @codedoc_comment_block news("basicepistats::stat_absolute_risk", "2022-07-26", "0.2.0")
  # `basicepistats::stat_absolute_risk` gains arg
  # `assertion_type`.
  # @codedoc_comment_block news("basicepistats::stat_absolute_risk", "2022-07-26", "0.2.0")

  stat_absolute_risk_call <- match.call()
  stat_absolute_risk_env  <- environment()

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param assertion_type `[character]` (default `"input"`)
  #
  # See e.g. `[dbc::assert_is_one_of]`.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_assertion_type(assertion_type, assertion_type = "input")

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param stratum_col_nms `[NULL, character]` (default `NULL`)
  #
  # - `NULL`: no stratification of results.
  # - `character`: Results will be stratified by these columns. E.g.
  #   `stratum_col_nms = c("sex", "area")`
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_one_of(
    stratum_col_nms,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_character_nonNA_atom
    ),
    assertion_type = assertion_type
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param interval_start_col_nm `[character]` (no default)
  #
  # Identifies the start time of the interval for the desired timescale.
  # E.g. `interval_start_col_nm = "age_group_start"`.
  #
  # This columns must have one of the classes `c("numeric", "integer", "Date")`.
  # No missing values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_character_nonNA_atom(interval_start_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_one_of(
    x[[interval_start_col_nm]],
    x_nm = paste0("x$", interval_start_col_nm),
    funs = list(
      dbc::report_is_number_nonNA_vector,
      dbc::report_is_Date_nonNA_vector
    ),
    assertion_type = assertion_type
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param interval_stop_col_nm `[character]` (no default)
  #
  # Identifies the stop time of the interval for the desired timescale.
  # E.g. `interval_stop_col_nm = "age_group_stop"`.
  #
  # This column must have the same class as `x[[interval_start_col_nm]]`.
  # No missing values are allowed. Every stop value must be larger than the
  # corresponding start value.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_character_nonNA_atom(interval_stop_col_nm,
                                      assertion_type = assertion_type)
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      list(
        quote(identical(
          class(x[[interval_start_col_nm]]),
          class(x[[interval_stop_col_nm]])
        )),
        quote(x[[interval_start_col_nm]] < x[[interval_stop_col_nm]])
      ),
      fail_messages = c(
        paste0(
          "x$", interval_start_col_nm, " and ",
          "x$", interval_stop_col_nm, " had different classes: ",
          deparse(class(x[[interval_start_col_nm]])), " vs. ",
          deparse(class(x[[interval_stop_col_nm]]))
        ),
        paste0(
          "x$", interval_start_col_nm, " < ",
          "x$", interval_stop_col_nm, " not true for all observations; ",
          "numbers of first five invalid rows: ${deparse(utils::head(wh_fail))}"
        )
      ),
      pass_messages = c(
        paste0(
          "x$", interval_start_col_nm, " and ",
          "x$", interval_stop_col_nm, " had the same classes."
        ),
        NA_character_
      ),
      call = stat_absolute_risk_call,
      env = stat_absolute_risk_env
    ),
    assertion_type = assertion_type
  )
  dbc::assert_is_nonNA(x[[interval_stop_col_nm]],
                       assertion_type = assertion_type)
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param event_count_col_nms `[character]` (no default)
  #
  # Identifies the columns (one or more) containing counts of events.
  # E.g. `event_count_col_nm = c("event_1", "event_2")`.
  #
  # Each of these columns must have class `integer`. All values must be >= 0.
  # No missing values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_character_nonNA_vector(event_count_col_nms,
                                        assertion_type = assertion_type)
  dbc::assert_is_uniquely_named(event_count_col_nms,
                                assertion_type = assertion_type)
  lapply(event_count_col_nms, function(col_nm) {
    dbc::assert_is_integer_nonNA_gtezero_vector(
      x[[col_nm]],
      x_nm = paste0("x$", col_nm),
      call = stat_absolute_risk_call,
      assertion_type = assertion_type
    )
  })
  dbc::assert_is(quote(length(event_count_col_nms) >= 1L),
                 assertion_type = assertion_type)
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param at_risk_time_col_nm `[character]` (no default)
  #
  # Identifies the column containing time-at-risk values. That is, time at risk
  # of all the possible events (which should exclude e.g. those who already have
  # cancer, if cancer is one of the events).
  # E.g. `at_risk_time_col_nm = "pyrs"`.
  #
  # This column must be either `integer` or `numeric`. All values must be >=
  # and not missing.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dbc::assert_is_character_nonNA_atom(at_risk_time_col_nm,
                                      assertion_type = assertion_type)
  dbc::assert_is_number_nonNA_gtezero_vector(x[[at_risk_time_col_nm]],
                                             assertion_type = assertion_type)

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @param x `[data.table]` (no default)
  #
  # `data.table` with the required columns (see other args). It must not have
  # duplicated rows as identified by
  # `c(stratum_col_nms, interval_start_col_nm)`.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  col_nm_dt <- data.table::data.table(
    x = c(
      NA_character_,
      stratum_col_nms,
      interval_start_col_nm,
      interval_stop_col_nm,
      event_count_col_nms,
      at_risk_time_col_nm,
      rep(NA_character_, 7L)
    ),
    tmp = c(
      "stratum_1",
      paste0(
        rep("stratum_", length(stratum_col_nms)),
        seq_along(stratum_col_nms) + 1L
      ),
      "interval_start",
      "interval_stop",
      paste0("event_", seq_along(event_count_col_nms), "_count"),
      "at_risk_time",
      paste0("event_", seq_along(event_count_col_nms), "_hazard_rate"),
      paste0("event_", seq_along(event_count_col_nms), "_absolute_risk"),
      "overall_hazard_rate",
      "overall_cumulative_hazard_rate",
      "overall_survival"
    ),
    output = c(
      NA_character_,
      stratum_col_nms,
      interval_start_col_nm,
      interval_stop_col_nm,
      event_count_col_nms,
      at_risk_time_col_nm,
      rep(NA_character_, 2L),
      paste0(names(event_count_col_nms), "_absolute_risk"),
      rep(NA_character_, 2L),
      "overall_survival"
    ),
    type = c(
      rep("stratum", length(stratum_col_nms) + 1L),
      rep("interval", 2L),
      rep("event_count", length(event_count_col_nms)),
      "at_risk",
      rep("event_hazard_rate", 2L),
      rep("event_absolute_risk", 2L),
      "overall_hazard_rate",
      "overall_cumulative_hazard_rate",
      "overall_survival"
    )
  )
  tmp_col_nm_sets <- split(col_nm_dt[["tmp"]], f = col_nm_dt[["type"]])
  col_nm_dt[
    j = "is_key" := col_nm_dt[["type"]] %in% c("stratum", "interval")
  ]
  tmp_col_nm_sets[["key"]] <- col_nm_dt[["tmp"]][col_nm_dt[["is_key"]]]
  col_nm_dt[
    j = "identifies_unique" := col_nm_dt[["is_key"]] &
      !col_nm_dt[["tmp"]] %in% "interval_stop"
  ]
  tmp_col_nm_sets[["unique_id"]] <- col_nm_dt[["tmp"]][
    col_nm_dt[["identifies_unique"]]
  ]
  dbc::assert_is_data_table_with_required_names(
    x, required_names = setdiff(col_nm_dt[["x"]], NA_character_),
    assertion_type = assertion_type
  )
  dbc::assert_is(
    quote(!duplicated(x, by = intersect(tmp_col_nm_sets[["unique_id"]], names(x)))),
    assertion_type = assertion_type
  )
  dt <- local({
    type_set <- c("stratum", "interval", "event_count", "at_risk")
    col_no_set <- which(col_nm_dt[["type"]] %in% type_set)
    dt <- lapply(col_no_set, function(i) {
      x_col_nm <- col_nm_dt[["x"]][i]
      tmp_col_nm <- col_nm_dt[["tmp"]][i]
      if (tmp_col_nm == "stratum_1") {
        return(rep(TRUE, nrow(x)))
      }
      x[[x_col_nm]]
    })
    data.table::setDT(dt)
    data.table::setnames(
      dt, col_nm_dt[["tmp"]][1:which(col_nm_dt[["tmp"]] == "at_risk_time")]
    )
    data.table::setkeyv(    dt, tmp_col_nm_sets[["key"]])
    data.table::setcolorder(dt, intersect(col_nm_dt[["tmp"]], names(dt)))
    dt[]
  })
  # @codedoc_comment_block method_reference(basicepistats::stat_absolute_risk)
  #    Chiang, Chin Long, 1986,
  #    Introduction to stochastic processes in biostatistics,
  #    ISBN-13 978-0471155003.
  # @codedoc_comment_block method_reference(basicepistats::stat_absolute_risk)

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # @section Under the hood:
  #
  # `basicepistats::stat_absolute_risk` implements the method described in
  # @codedoc_insert_comment_block method_reference(basicepistats::stat_absolute_risk)
  #
  # The estimation of absolute risk curves is performed via the following steps
  # on working dataset `dt` created based on `x`:
  #
  # @codedoc_comment_block basicepistats::stat_absolute_risk

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  #
  # 1. The hazard rate of each event is estimated via
  #    `dt[[event_count_col_nms[i]]] / dt[[at_risk_time_col_nm]]` for
  #    each `i in seq_along(event_count_col_nms)`. An "overall hazard rate"
  #    is computed as the sum of all the different hazard rates.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  dt[
    j = "interval_width" := .SD[[2L]] - .SD[[1L]],
    .SDcols = tmp_col_nm_sets[["interval"]]
  ]
  dt[, "overall_hazard_rate" := 0.0]
  lapply(seq_along(tmp_col_nm_sets[["event_count"]]), function(i) {
    count_col_nm <- tmp_col_nm_sets[["event_count"]][i]
    haz_col_nm   <- tmp_col_nm_sets[["event_hazard_rate"]][i]
    data.table::set(
      dt,
      j = haz_col_nm,
      value = dt[[count_col_nm]] / dt[["at_risk_time"]]
    )
    data.table::set(
      dt,
      j = "overall_hazard_rate",
      value = dt[["overall_hazard_rate"]] + dt[[haz_col_nm]]
    )
    NULL
  })
  dbc::assert_prod_interim_is_nonNA(dt[["overall_hazard_rate"]])

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  #
  # 2. The overall survival function is estimated via
  #    `exp(-cumsum(dt[["overall_hazard_rate"]] * dt[["interval_width"]]))`.
  #    This is performed separately
  #    for each stratum identified by `stratum_col_nms`, if any exist.
  #    The "overall event probability" for each interval, conditional on survival
  #    up to the start of the interval, is computed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  # overall_cumulative_hazard_rate up to the end of the interval
  dt[
    j = "overall_cumulative_hazard_rate" := .SD[[1L]] * .SD[[2L]],
    .SDcols = c("overall_hazard_rate", "interval_width")
  ]
  dt[
    # see ?data.table::gforce
    j = "overall_cumulative_hazard_rate" := lapply(.SD, cumsum),
    .SDcols = "overall_cumulative_hazard_rate",
    by = eval(tmp_col_nm_sets[["stratum"]])
  ]
  # overall_cumulative_hazard_rate up to the end of the interval
  dt[
    j = "overall_survival" := exp(-.SD[[1L]]),
    .SDcols = "overall_cumulative_hazard_rate"
  ]
  # overall_cumulative_hazard_rate up to the end of the previous interval
  # (start of current interval)
  dt[
    j = "overall_survival_lag_1" := data.table::shift(
      .SD[[1L]], type = "lag", n = 1L, fill = 1.0
    ),
    by = eval(tmp_col_nm_sets[["stratum"]]),
    .SDcols = "overall_survival"
  ]
  # probability of surviving current interval, conditional on having survived
  # up to the start of it
  dt[
    j = "conditional_overall_survival" := .SD[[1L]] / .SD[[2L]],
    .SDcols = c("overall_survival", "overall_survival_lag_1")
  ]
  # probability of getting any event, conditional on having avoided them all
  # up to the start of the interval
  dt[
    j = "conditional_overall_risk" := 1.0 - .SD[[1L]],
    .SDcols = "conditional_overall_survival"
  ]

  # @codedoc_comment_block basicepistats::stat_absolute_risk
  #
  # 3. Event-specific absolute risks are finally estimated via
  #    `cumsum(osl1 * cor * h / ohr)`, where `osl1` is the lag of the overall
  #     survival (survival probability up to the start of the interval),
  #    `cor` the conditional overall survival,
  #    `h` an event-specific hazard rate.
  #
  # @codedoc_comment_block basicepistats::stat_absolute_risk
  local({
    lapply(seq_along(tmp_col_nm_sets[["event_hazard_rate"]]), function(i) {
      osl1 <- dt[["overall_survival_lag_1"]]
      h <- dt[[tmp_col_nm_sets[["event_hazard_rate"]][i]]]
      cor <- dt[["conditional_overall_risk"]]
      ohr <- dt[["overall_hazard_rate"]]
      data.table::set(
        dt,
        j = tmp_col_nm_sets[["event_absolute_risk"]][i],
        value = osl1 * cor * h / ohr
      )
      NULL
    })
    dt[
      j = (tmp_col_nm_sets[["event_absolute_risk"]]) := lapply(.SD, cumsum),
      .SDcols = tmp_col_nm_sets[["event_absolute_risk"]],
      by = eval(tmp_col_nm_sets[["stratum"]])
    ]
    NULL
  })

  # final touches --------------------------------------------------------------
  dt <- dt[
    j = .SD,
    .SDcols = col_nm_dt[["tmp"]][!is.na(col_nm_dt[["output"]])]
  ]
  data.table::setkeyv( dt, intersect(tmp_col_nm_sets[["key"]], names(dt)))
  data.table::setnames(dt, col_nm_dt[["output"]][!is.na(col_nm_dt[["output"]])])

  return(dt[])
}

stat_absolute_risk_docs <- function() {
  requireNamespace("codedoc")
  re <- "basicepistats::stat_absolute_risk"
  codedoc::codedoc_lines(re)
}

