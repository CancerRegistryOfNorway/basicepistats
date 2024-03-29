% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/absolute_risk.R
\name{absolute_risk_curve_tools}
\alias{absolute_risk_curve_tools}
\alias{stat_absolute_risk}
\title{Absolute Risk Curve}
\usage{
stat_absolute_risk(
  x,
  stratum_col_nms = NULL,
  interval_start_col_nm,
  interval_stop_col_nm,
  event_count_col_nms,
  at_risk_time_col_nm,
  assertion_type = "input"
)
}
\arguments{
\item{x}{\verb{[data.table]} (no default)

\code{data.table} with the required columns (see other args). It must not have
duplicated rows as identified by
\code{c(stratum_col_nms, interval_start_col_nm)}.

Chiang, Chin Long, 1986,
Introduction to stochastic processes in biostatistics,
ISBN-13 978-0471155003.}

\item{stratum_col_nms}{\verb{[NULL, character]} (default \code{NULL})
\itemize{
\item \code{NULL}: no stratification of results.
\item \code{character}: Results will be stratified by these columns. E.g.
\code{stratum_col_nms = c("sex", "area")}
}}

\item{interval_start_col_nm}{\verb{[character]} (no default)

Identifies the start time of the interval for the desired timescale.
E.g. \code{interval_start_col_nm = "age_group_start"}.

This columns must have one of the classes \code{c("numeric", "integer", "Date")}.
No missing values are allowed.}

\item{interval_stop_col_nm}{\verb{[character]} (no default)

Identifies the stop time of the interval for the desired timescale.
E.g. \code{interval_stop_col_nm = "age_group_stop"}.

This column must have the same class as \code{x[[interval_start_col_nm]]}.
No missing values are allowed. Every stop value must be larger than the
corresponding start value.}

\item{event_count_col_nms}{\verb{[character]} (no default)

Identifies the columns (one or more) containing counts of events.
E.g. \code{event_count_col_nm = c("event_1", "event_2")}.

Each of these columns must have class \code{integer}. All values must be >= 0.
No missing values are allowed.}

\item{at_risk_time_col_nm}{\verb{[character]} (no default)

Identifies the column containing time-at-risk values. That is, time at risk
of all the possible events (which should exclude e.g. those who already have
cancer, if cancer is one of the events).
E.g. \code{at_risk_time_col_nm = "pyrs"}.

This column must be either \code{integer} or \code{numeric}. All values must be >=
and not missing.}

\item{assertion_type}{\verb{[character]} (default \code{"input"})

See e.g. \verb{[dbc::assert_is_one_of]}.}
}
\description{
Tools to estimate absolute risk curves. Absolute risk is also known as
crude risk and sometimes "cumulative incidence function" due to the
cumulative hazard rate estimation method. An absolute risk curve shows e.g.
the statistical probability in some population of developing cancer
(getting the first cancer diagnosis) over years of age.
}
\section{Under the hood}{


\code{basicepistats::stat_absolute_risk} implements the method described in
Chiang, Chin Long, 1986,
Introduction to stochastic processes in biostatistics,
ISBN-13 978-0471155003.

The estimation of absolute risk curves is performed via the following steps
on working dataset \code{dt} created based on \code{x}:
\enumerate{
\item The hazard rate of each event is estimated via
\code{dt[[event_count_col_nms[i]]] / dt[[at_risk_time_col_nm]]} for
each \verb{i in seq_along(event_count_col_nms)}. An "overall hazard rate"
is computed as the sum of all the different hazard rates.
\item The overall survival function is estimated via
\code{exp(-cumsum(dt[["overall_hazard_rate"]] * dt[["interval_width"]]))}.
This is performed separately
for each stratum identified by \code{stratum_col_nms}, if any exist.
The "overall event probability" for each interval, conditional on survival
up to the start of the interval, is computed.
\item Event-specific absolute risks are finally estimated via
\code{cumsum(osl1 * cor * h / ohr)}, where \code{osl1} is the lag of the overall
survival (survival probability up to the start of the interval),
\code{cor} the conditional overall survival,
\code{h} an event-specific hazard rate.
}
}

\examples{
dt <- data.table::data.table(
  age_group_start = 0:100,
  age_group_stop  = c(1:100, 120L),
  cancer_count    = 1:101,
  death_count     = (1:101 + 10L),
  pyrs            = rep(1e4, 101)
)
dt <- rbind(data.table::copy(dt)[, "sex" := 1L],
            data.table::copy(dt)[, "sex" := 2L])
dt[dt[["sex"]] == 2L, "cancer_count" := c(rep(1:10, each = 10L), 5L)]
ardt <- basicepistats::stat_absolute_risk(
  x                     = dt,
  stratum_col_nms       = "sex",
  interval_start_col_nm = "age_group_start",
  interval_stop_col_nm  = "age_group_stop",
  event_count_col_nms   = c(cancer = "cancer_count", death = "death_count"),
  at_risk_time_col_nm   = "pyrs"
)

# overall survival and event-specific absolute risks sum to one!
stopifnot(all.equal(
  ardt[["overall_survival"]] + ardt[["cancer_absolute_risk"]] +
    ardt[["death_absolute_risk"]],
  rep(1.0, nrow(ardt))
))

plot( cancer_absolute_risk ~ age_group_stop,
      data = ardt[ardt[["sex"]] == 1L, ], col = "black", type = "l",
      ylim = c(0, max(ardt[["cancer_absolute_risk"]])))
lines(cancer_absolute_risk ~ age_group_stop,
      data = ardt[ardt[["sex"]] == 2L, ], col = "red")

plot( death_absolute_risk ~ age_group_stop,
      data = ardt[ardt[["sex"]] == 1L, ], col = "black", type = "l",
      ylim = c(0, max(ardt[["death_absolute_risk"]])))
lines(death_absolute_risk ~ age_group_stop,
      data = ardt[ardt[["sex"]] == 2L, ], col = "red")

# this example mostly for testing that basicepistats::stat_absolute_risk
# works as intended. in this example, 5 people all get cancer and none die.
# one gets cancer in the middle of each interval.
dt <- data.table::data.table(
  age_group_start = 0:4 * 20L,
  age_group_stop  = 1:5 * 20L,
  cancer_count    = rep(1L, 5L),
  death_count     = rep(0L, 5L),
  pyrs            = c(100, 80, 60, 40, 20) - 10
)
ardt <- basicepistats::stat_absolute_risk(
  x                     = dt,
  interval_start_col_nm = "age_group_start",
  interval_stop_col_nm  = "age_group_stop",
  event_count_col_nms   = c(cancer = "cancer_count", death = "death_count"),
  at_risk_time_col_nm   = "pyrs"
)
}
