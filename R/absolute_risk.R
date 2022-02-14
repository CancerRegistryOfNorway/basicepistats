

#' @title Absolute Risk Curve
#' @description
#' AKA crude risk, sometimes "cumulative incidence function" due to that
#' estimation method. The tools described here can estimate the curve of the
#' statistical probability of a subject having an event over a timescale.
#' E.g. the curve of probabilities of having the first breast cancer diagnosis
#' can be estimated for some population.
#' @name absolute_risk_curve_tools

#' @rdname absolute_risk_curve_tools
#' @eval stat_absolute_risk_ag_docs()
#' @examples
#' dt <- data.table::data.table(
#'   age_group_start = 0:99,
#'   age_group_stop = 1:100,
#'   cancer_count = 1:100,
#'   death_count = (1:100 + 10L),
#'   cancer_pyrs = 100:1,
#'   death_pyrs = rep(0.0, 100),
#'   pop_pyrs = rep(1e4, 100)
#' )
#' dt <- rbind(data.table::copy(dt)[, "sex" := 1L],
#'             data.table::copy(dt)[, "sex" := 2L])
#' dt[dt[["sex"]] == 2L, "cancer_count" := rep(1:10, each = 10L)]
#' dt[dt[["sex"]] == 2L, "cancer_pyrs"  := rep(1.0, 100L)]
#' ardt <- basicepistats::stat_absolute_risk_ag(
#'   x = dt,
#'   stratum_col_nms               = "sex",
#'   timescale_start_col_nm        = "age_group_start",
#'   timescale_stop_col_nm         = "age_group_stop",
#'   event_count_col_nms           = c("cancer_count", "death_count"),
#'   event_time_amount_col_nms     = c("cancer_pyrs" , "death_pyrs"),
#'   population_time_amount_col_nm = "pop_pyrs"
#' )
#'
#' plot(event_1_absolute_risk ~ age_group_stop,
#'      data = ardt[ardt[["sex"]] == 1L, ], col = "black", type = "l")
#' lines(event_1_absolute_risk ~ age_group_stop,
#'       data = ardt[ardt[["sex"]] == 2L, ], col = "red")
#' @export
stat_absolute_risk_ag <- function(
  x,
  stratum_col_nms = NULL,
  timescale_start_col_nm,
  timescale_stop_col_nm,
  event_count_col_nms,
  event_time_amount_col_nms,
  population_time_amount_col_nm
) {
  stat_absolute_risk_ag_call <- match.call()
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param stratum_col_nms `[NULL, character]` (default `NULL`)
  #
  # - `NULL`: no stratification of results.
  # - `character`: Results will be stratified by these columns. E.g.
  #   `stratum_col_nms = c("sex", "area")`
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_one_of(
    stratum_col_nms,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_character_nonNA_atom
    )
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param timescale_start_col_nm `[character]` (no default)
  #
  # Identifies the start time of the bin for the desired timescale.
  # E.g. `timescale_start_col_nm = "age_group_start"`.
  #
  # This columns must have one of the classes `c("numeric", "integer", "Date")`.
  # No missing values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_character_nonNA_atom(timescale_start_col_nm)
  dbc::assert_is_one_of(
    x[[timescale_start_col_nm]],
    x_nm = paste0("x$", timescale_start_col_nm),
    funs = list(
      dbc::report_is_number_nonNA_vector,
      dbc::report_is_Date_nonNA_vector
    )
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param timescale_stop_col_nm `[character]` (no default)
  #
  # Identifies the stop time of the bin for the desired timescale.
  # E.g. `timescale_start_col_nm = "age_group_stop"`.
  #
  # This column must have the same class as `x[[timescale_start_col_nm]]`.
  # No missing values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_character_nonNA_atom(timescale_stop_col_nm)
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      list(quote(identical(
        class(x[[timescale_start_col_nm]]),
        class(x[[timescale_stop_col_nm]])
      ))),
      fail_messages = paste0(
        "x$", timescale_start_col_nm, " and ",
        "x$", timescale_stop_col_nm, " had different classes: ",
        deparse(class(x[[timescale_start_col_nm]])), " vs. ",
        deparse(class(x[[timescale_stop_col_nm]]))
      ),
      pass_messages = paste0(
        "x$", timescale_start_col_nm, " and ",
        "x$", timescale_stop_col_nm, " had the same classes."
      ),
      call = stat_absolute_risk_ag_call
    )
  )
  dbc::assert_is_nonNA(x[[timescale_stop_col_nm]])
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param event_count_col_nms `[character]` (no default)
  #
  # Identifies the columns (one or more) containing counts of events.
  # E.g. `event_count_col_nm = c("event_1", "event_2")`.
  #
  # Each of these columns must have class `integer`. All values must be >= 0.
  # No missing values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_character_nonNA_vector(event_count_col_nms)
  lapply(event_count_col_nms, function(col_nm) {
    dbc::assert_is_integer_nonNA_gtezero_vector(
      x[[col_nm]],
      x_nm = paste0("x$", col_nm),
      call = stat_absolute_risk_ag_call
    )
  })
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      list(quote(length(event_count_col_nms) >= 1L)),
      call = stat_absolute_risk_ag_call
    )
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param event_time_amount_col_nms `[character]` (no default)
  #
  # Identifies the column (one or more) containing counts of events.
  # E.g. `event_count_col_nm = c("event_1_count", "event_2_count")`.
  #
  # See section **Under the hood** for more information about the purpose of
  # this argument.
  #
  # There must be as many elements in
  # `event_count_col_nms` as in `event_time_amount_col_nms`. These two arguments
  # are assumed to relate to each other elementwise, e.g. the first elements
  # of both are assumed to belong to the same event.
  #
  # Each of these columns must be either `integer` or `numeric`. No missing
  # values are allowed. All values must be >= 0.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_character_nonNA_vector(event_time_amount_col_nms)
  lapply(event_time_amount_col_nms, function(col_nm) {
    dbc::assert_is_number_nonNA_gtezero_vector(
      x[[col_nm]],
      x_nm = paste0("x$", col_nm),
      call = stat_absolute_risk_ag_call
    )
  })
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      list(quote(
        length(event_count_col_nms) == length(event_time_amount_col_nms)
      )),
      call = stat_absolute_risk_ag_call
    )
  )
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param population_time_amount_col_nm `[character]` (no default)
  #
  # Identifies the column containing time amounts for the (whole, general)
  # population regardless of what events the subjects in the population may
  # have encountered. E.g. general population person-years from the national
  # statistics office. E.g. `population_time_amount_col_nm = "pop_pyrs"`.
  #
  # This column must be either `integer` or `numeric`. No missing **NOR ZERO**
  # values are allowed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dbc::assert_is_character_nonNA_atom(population_time_amount_col_nm)
  dbc::assert_is_number_nonNA_gtzero_vector(x[[population_time_amount_col_nm]])
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @param x `[data.table]` (no default)
  #
  # `data.table` with the required columns (see other args). Tt must not have
  # duplicated rows as identified by
  # `c(stratum_col_nms, timescale_start_col_nm)`.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dt_col_nms <- c(
    stratum_col_nms,
    timescale_start_col_nm,
    timescale_stop_col_nm,
    event_count_col_nms,
    event_time_amount_col_nms,
    population_time_amount_col_nm
  )
  dbc::assert_is_data.table_with_required_names(x, required_names = dt_col_nms)
  dbc::report_to_assertion(
    dbc::expressions_to_report(
      list(quote(
        !duplicated(x, by = c(stratum_col_nms, timescale_start_col_nm))
      )),
      call = stat_absolute_risk_ag_call
    )
  )
  dt <- lapply(dt_col_nms, function(col_nm) {
    x[[col_nm]]
  })
  data.table::setDT(dt)
  data.table::setnames(dt, dt_col_nms)
  if (is.null(stratum_col_nms)) {
    stratum_col_nms <- ".__competing_event_count_dummy"
    data.table::set(dt, j = stratum_col_nms, value = TRUE)
    on.exit(
      data.table::set(dt, j = stratum_col_nms, value = NULL),
      add = TRUE
    )
  }
  tmp_stratum_col_nms <- paste0(".__tmp_", stratum_col_nms)
  data.table::setnames(dt, stratum_col_nms, tmp_stratum_col_nms)
  on.exit(
    data.table::setnames(dt, tmp_stratum_col_nms, stratum_col_nms),
    add = TRUE
  )
  key_col_nms <- c(
    tmp_stratum_col_nms, timescale_start_col_nm, timescale_stop_col_nm
  )
  data.table::setkeyv(dt, key_col_nms)

  # @codedoc_comment_block method_reference(basicepistats::stat_absolute_risk_ag)
  #    Chiang, Chin Long, 1986,
  #    Introduction to stochastic processes in biostatistics,
  #    ISBN-13 978-0471155003.
  # @codedoc_comment_block method_reference(basicepistats::stat_absolute_risk_ag)

  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  # @section Under the hood:
  #
  # `basicepistats::stat_absolute_risk_ag` implements the method described in
  # @codedoc_insert_comment_block method_reference(basicepistats::stat_absolute_risk_ag)
  #
  # The estimation of absolute risk curves is performed via the following steps
  # on working dataset `dt` created based on `x`:
  #
  # 1. At-risk time amounts are computed by subtracting each event time amount
  #    from `dt[[population_time_amount_col_nm]]`. This is the total amount of
  #    time in each stratum that the population spent before/without having any
  #    of the events in question. E.g. time the population spent alive and
  #    cancer-free. This is collected into column `dt[["at_risk_time_amount"]]`.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  data.table::set(
    dt,
    j = "at_risk_time_amount",
    value = dt[[population_time_amount_col_nm]]
  )
  lapply(event_time_amount_col_nms, function(col_nm) {
    data.table::set(
      dt,
      j = "at_risk_time_amount",
      value = dt[["at_risk_time_amount"]] - dt[[col_nm]]
    )
    NULL
  })

  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  #
  # 2. The hazard rate of each event is estimated via
  #    `dt[[event_count_col_nms[i]]] / dt[["at_risk_time_amount"]]` for
  #    each `i in seq_along(event_count_col_nms)`. An "overall hazard rate"
  #    is computed as the sum of all the different hazard rates.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  event_hazard_rate_col_nms <- paste0(
    "event_", seq_along(event_count_col_nms),
    "_hazard_rate"
  )
  dt[, "overall_hazard_rate" := 0.0]
  lapply(seq_along(event_count_col_nms), function(i) {
    data.table::set(
      dt,
      j = event_hazard_rate_col_nms[i],
      value = dt[[event_count_col_nms[i]]] / dt[["at_risk_time_amount"]]
    )
    data.table::set(
      dt,
      j = "overall_hazard_rate",
      value = dt[["overall_hazard_rate"]] + dt[[event_hazard_rate_col_nms[i]]]
    )
    NULL
  })
  dbc::assert_prod_interim_is_nonNA(dt[["overall_hazard_rate"]])

  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  #
  # 3. The overall survival function is estimated via
  #    `exp(-cumsum(dt[["overall_hazard_rate"]]))`. This is performed separately
  #    for each stratum identified by `stratum_col_nms`, if any exist.
  #    The "overall event probability" for each bin, conditional on survival
  #    up to the start of the bin, is computed.
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  dt[
    # see ?data.table::gforce
    j = "overall_survival" := lapply(.SD, cumsum),
    by = eval(tmp_stratum_col_nms),
    .SDcols = "overall_hazard_rate"
  ]
  dt[
    j = "overall_survival" := exp(-.SD[[1L]]),
    .SDcols = "overall_survival"
  ]
  dt[
    j = "overall_survival_lag_1" := data.table::shift(
      .SD[[1L]], type = "lag", n = 1L, fill = 1.0
    ),
    by = eval(tmp_stratum_col_nms),
    .SDcols = "overall_survival"
  ]
  dt[
    j = "conditional_overall_survival" := .SD[[1L]] / .SD[[2L]],
    .SDcols = c("overall_survival", "overall_survival_lag_1")
  ]
  dt[
    j = "conditional_overall_risk" := 1.0 - .SD[[1L]],
    .SDcols = "conditional_overall_survival"
  ]

  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  #
  # 4. Event-specific absolute risks are finally estimated via
  #    `conditional_overall_risk * h / overall_hazard_rate`, where
  #    `h` is an event-specific hazard rate.
  #
  # @codedoc_comment_block basicepistats::stat_absolute_risk_ag
  event_absolute_risk_col_nms <- sub(
    "hazard_rate",
    "absolute_risk",
    event_hazard_rate_col_nms
  )
  lapply(seq_along(event_hazard_rate_col_nms), function(i) {
    h <- dt[[paste0(event_hazard_rate_col_nms[i])]]
    data.table::set(
      dt,
      j = event_absolute_risk_col_nms[i],
      value = dt[["conditional_overall_risk"]] * h / dt[["overall_hazard_rate"]]
    )
    NULL
  })

  return(dt[])
}

stat_absolute_risk_ag_docs <- function() {
  requireNamespace("codedoc")
  df <- codedoc::extract_keyed_comment_blocks(
    text_file_paths = "R/absolute_risk.R",
    detect_allowed_keys = function(x) {
      grepl("basicepistats::stat_absolute_risk", x)
    }
  )
  lines <- unlist(lapply(df[["comment_block"]], function(obj) {
    c("", obj, "")
  }))
  return(lines)
}

