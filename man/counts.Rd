% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/counts.R
\name{counts}
\alias{counts}
\alias{stat_count}
\alias{stat_count_}
\alias{stat_unique_count}
\alias{stat_unique_count_}
\title{Counts}
\usage{
stat_count(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "user_input"
)

stat_count_(
  x,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "prod_input"
)

stat_unique_count(
  x,
  unique_by,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "input"
)

stat_unique_count_(
  x,
  unique_by,
  by = NULL,
  subset = NULL,
  subset_style = c("zeros", "drop")[1],
  melt = NULL,
  assertion_type = "prod_input"
)
}
\arguments{
\item{x}{\verb{[data.table]} (mandatory, no default)

compute counts in this dataset}

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

\item{melt}{\verb{[NULL, list]} (default \code{NULL})

You may use this argument to easily melt
(see \verb{[data.table::melt.data.table]}) stratifying columns of your choice.
E.g. \code{list(area = c("area_1", "area_2"))} to combine to area columns into
one.

The table is re-aggregated if any melting is performed by summing the
value columns. This avoids duplicated strata in the table.
\itemize{
\item \code{NULL}: No melting performed.
\item \code{list}: List of one or more elements, where each element is a character
string vector of column names existing in the table.
}}

\item{assertion_type}{\verb{[character]}

Passed to one or more assertions. See e.g. \verb{[dbc::report_to_assertion]}.}

\item{unique_by}{\verb{[character]} (mandatory, no default)

names of columns in \code{x}; unique combinations of these columns are counted;
e.g. \code{unique_by = "my_subject_id"} to count numbers of subjects by strata;
note that this causes each thing to be counted zero or one
times in each stratum separately --- so e.g. the same person can appear
once in multiple strata; see \strong{Examples}}
}
\description{
Produce a stratified count table.
}
\details{
\itemize{
\item \code{stat_count} produces the number of records by strata; this function is
intended to be used directly by the end-user
}

\itemize{
\item \code{stat_count_} produces the number of records by strata; this function is
intended to be used within other functions
}

\itemize{
\item \code{stat_unique_count} produces the number of unique combinations of columns
defined in \code{unique_by}; e.g. the number of unique subjects by strata;
this function is intended to be used directly by the end-user
}

\itemize{
\item \code{stat_unique_count_} produces the number of unique combinations of columns
defined in \code{unique_by}; e.g. the number of unique subjects by strata;
this function is intended to be used inside other functions
}
}
\examples{

# basicepistats::stat_count
library("data.table")
sls <- data.table::CJ(sex = 1:2, area_2 = c(11L, 12L, 13L, 21L, 22L))
area_sls <- data.table::data.table(
   area_1 = c(10L, 10L, 10L, 20L, 20L),
   area_2 = c(11L, 12L, 13L, 21L, 22L)
)
sls <- merge(sls, area_sls, by = "area_2")
data.table::setcolorder(sls, c("sex", "area_1", "area_2"))
data.table::setkeyv(sls, names(sls))

my_dataset <- data.table::data.table(
   sex = 1L,
   area_1 = 10L,
   area_2 = 11L
)

basicepistats::stat_count(
  x = my_dataset,
  by = sls
)
basicepistats::stat_count(
  x = my_dataset,
  by = list(sex = 1:2, area_1 = c(10L, 20L))
)
basicepistats::stat_count(
  x = my_dataset,
  by = list(sex = 1:2, area = area_sls)
)

## melt
observed <- basicepistats::stat_count(
  x = my_dataset,
  by = sls,
  melt = list(area = c("area_1", "area_2"))
)
stopifnot(
  observed[["N"]][observed[["area"]] == 1 & observed[["area"]] == 10L] == 1,
  observed[["N"]][observed[["area"]] == 1 & observed[["area"]] == 11L] == 1
)


# basicepistats::stat_unique_count
dt <- data.table::data.table(id = c(1, 1, 2), grp = 1:3)
observed <- basicepistats::stat_unique_count(
  x = dt,
  unique_by = "id",
  by = "grp"
)
expected <- data.table::data.table(grp = 1:3, N = 1L, key = "grp")
stopifnot(all.equal(observed, expected, check.attributes = FALSE))

}
