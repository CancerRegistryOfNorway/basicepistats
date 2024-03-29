% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prevalence.R
\name{prevalence}
\alias{prevalence}
\alias{stat_prevalent_record_count}
\alias{stat_prevalent_subject_count_}
\alias{stat_prevalent_subject_count}
\title{Prevalence}
\usage{
stat_prevalent_record_count(
  x,
  follow_up_time_col_nm,
  follow_up_time_window_widths = Inf,
  by = NULL,
  subset = NULL,
  subset_style = "zeros",
  observation_time_points,
  entry_time_col_nm,
  assertion_type = "input"
)

stat_prevalent_subject_count_(
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
)

stat_prevalent_subject_count(
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
)
}
\arguments{
\item{x}{\verb{[data.table]} (mandatory, no default)

dataset containing one or more records by subject}

\item{follow_up_time_col_nm}{\verb{[character]} (mandatory, no default)

name of column in \code{x} which specifies values in the time scale in which
prevalence follow-up windows are determined (e.g. prevalence of subjects
with event within 1, 5, 10 years); the values in the column should be the
values at the start of follow-up (e.g. time of diagnosis)}

\item{follow_up_time_window_widths}{\verb{[numeric]} (mandatory, default \code{Inf})

widhts of windowss in time scale given in \code{follow_up_time_col_nm};
e.g. \code{c(1, 5, 10, Inf)} for windows of width 1, 5, 10 and window containing
all subjects}

\item{by}{\verb{[NULL, data.table, list]} (optional, default \code{NULL})
\itemize{
\item \code{NULL}: counts are produced using the levels observed in the
dataset; see \code{subset_style} for the effect of subsetting
\item \code{data.table}: describes the stratum column levels and the relationships
between stratum columns; must have a column for all \code{stratum_col_nms}
unless \code{stratum_col_nms} is \code{NULL}
\item \code{list}: a named \code{list}, where each name must be a name of a column in \code{x};
each element of the \code{list} contains the set of levels for the correspoding
column; the \code{list} is turned into a \code{data.table} and that is used as if
you had supplied a \code{data.table}
}}

\item{subset}{\verb{[NULL, integer, logical]} (optional, default \code{NULL})
\itemize{
\item \code{NULL}: no subsetting, i.e. use whole dataset
\item \code{integer}: subset to these rows before computations;
\code{NA} values throw a warning and are not included
\item \code{logical}: subset to rows where this is \code{TRUE} before computing counts;
\code{NA} values throw a warning and are not included; the \code{logical} vector
must have as many elements as the dataset has rows
}}

\item{subset_style}{\verb{[character]} (optional, default \code{"zeros"})
\itemize{
\item \code{"zeros"}: every combination of stratifying columns supplied via \code{by}
(or existing in data \emph{before} subsetting, if \code{by}
is \code{NULL}) will be included in output, but strata outside the subset
will have zero counts
\item \code{"drop"}: output will not have strata that are not in the subset
(zero counts are impossible)
}}

\item{observation_time_points}{\verb{[vector]} (mandatory, no default)

a vector of non-NA values of the same class as \code{x[[entry_time_col_nm]]};
prevalence is observed at each time point supplied here;
e.g. \code{c(1999.999, 2000.999)} for the ends of years 1999 and 2000
in fractional years if \code{x[[entry_time_col_nm]]} contains calendar
time values in fractional years}

\item{entry_time_col_nm}{\verb{[character]} (mandatory, no default)

name of column in \code{x} along which prevalence snapshots are taken; snapshots
are supplied via \code{observation_time_points}}

\item{assertion_type}{\verb{[character]}

Passed to one or more assertions. See e.g. \verb{[dbc::report_to_assertion]}.}

\item{subject_id_col_nm}{\verb{[character]} (mandatory, no default)

name of column in \code{x} which identifies subjects; one subject
may have one or more rows in \code{x}}
}
\description{
Compute subject prevalence at specific time points and optionally
conditioning on time since event at the time points.
}
\details{
The following logic determines whether a record is prevalent at a specific
observation time point:

\if{html}{\out{<div class="sourceCode">}}\preformatted{entry_time <= observation_time_point < entry_time + follow_up_time
}\if{html}{\out{</div>}}

Additionally, a record's entry to follow-up occurred no more than some
specified time ago if

\if{html}{\out{<div class="sourceCode">}}\preformatted{(observation_time_point - entry_time) < follow_up_time_window_width
}\if{html}{\out{</div>}}

Therefore, e.g. the number of prevalent cases that entered follow-up no more
than 5 years ago from the time sligthly before midnight 1999-12-31 can be
found using fractional years data by

\if{html}{\out{<div class="sourceCode">}}\preformatted{entry_time <= 1999.9999 < entry_time + follow_up_time
}\if{html}{\out{</div>}}

and

\if{html}{\out{<div class="sourceCode">}}\preformatted{1999.9999 - entry_time < 5.0
}\if{html}{\out{</div>}}

\itemize{
\item \code{stat_prevalent_record_count}: counts record prevalence; if one subject
has more than one records in the dataset, all their records are included
in the counts
}

\itemize{
\item \code{stat_prevalent_subject_count_}: counts subject prevalence; if one subject
has multiple records, it is included in the counts only once and not
as many times as it has records; this function intended for use inside other
functions
}

\itemize{
\item \code{stat_prevalent_subject_count}: counts subject prevalence; if one subject
has multiple records, it is included in the counts only once and not
as many times as it has records; this function intended for use directly
by the end-user
}
}
\examples{
library("data.table")

my_dataset <- data.table(
  sex = 1:2,
  follow_up_time = c(2.5, 4.5),
  calendar_time = c(2001.5, 2000.5)
)
stat_prevalent_record_count(
  x = my_dataset,
  follow_up_time_col_nm = "follow_up_time",
  follow_up_time_window_widths = c(1, 5, Inf),
  by = "sex",
  observation_time_points = 2000.0,
  entry_time_col_nm = "calendar_time"
)

library("data.table")

my_dataset <- data.table(
  id = c(1,1,2,2),
  sex = c(1,1,2,2),
  follow_up_time = c(2.5, 4.5),
  calendar_time = c(2001.5, 2000.5)
)
stat_prevalent_subject_count(
  x = my_dataset,
  follow_up_time_col_nm = "follow_up_time",
  follow_up_time_window_widths = c(1, 5, Inf),
  subject_id_col_nm = "id",
  by = "sex",
  observation_time_points = 2000.0,
  entry_time_col_nm = "calendar_time"
)

}
