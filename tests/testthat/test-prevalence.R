testthat::test_that("can compute record prevalence", {
  my_dataset <- data.table::data.table(
    id = c(1,1,2,2),
    sex = c(1,1,2,2),
    cancer_type = c(1,1,1,2),
    follow_up_time = c(2.5,1.5, 4.5,3.0),
    time_of_entry = c(2001.5,2002.5, 2000.5,2002.0)
  )
  fut_window_widths <- c(1, 5, Inf)
  obs_time_points <- 2001:2003 - 0.001
  observed <- stat_prevalence_count(
    x = my_dataset,
    follow_up_time_col_nm = "follow_up_time",
    follow_up_time_window_widths = fut_window_widths,
    observation_time_points = obs_time_points,
    entry_time_col_nm = "time_of_entry"
  )

  expected <- data.table::CJ(
    time_of_entry = obs_time_points,
    time_since_entry = unique(observed[["time_since_entry"]])
  )
  expected[, "N" := c(1L,1L,1L, 1L,2L,2L, 2L,4L,4L)]


  testthat::expect_equal(
    object = observed[["N"]],
    expected = expected[["N"]]
  )
})



testthat::test_that("can compute record prevalence using Dates", {
  my_dataset <- data.table::data.table(
    id = c(1,1,2,2),
    sex = c(1,1,2,2),
    cancer_type = c(1,1,1,2),
    follow_up_time = c(2.5,1.5, 4.5,3.0) * 365.242199,
    # time_of_entry = c(2001.5,2002.5, 2000.5,2002.0)
    time_of_entry = as.Date(c("2001-06-15", "2002-06-15", "2000-06-15", "2002-01-01"))
  )
  fut_window_widths <- c(1, 5, Inf) * 365.242199
  obs_time_points <- as.Date(paste0(2000:2002, "-12-31"))
  observed <- stat_prevalence_count(
    x = my_dataset,
    follow_up_time_col_nm = "follow_up_time",
    follow_up_time_window_widths = fut_window_widths,
    observation_time_points = obs_time_points,
    entry_time_col_nm = "time_of_entry"
  )

  expected <- data.table::CJ(
    time_of_entry = obs_time_points,
    time_since_entry = unique(observed[["time_since_entry"]])
  )
  expected[, "N" := c(1L,1L,1L, 1L,2L,2L, 2L,4L,4L)]


  testthat::expect_equal(
    object = observed[["N"]],
    expected = expected[["N"]]
  )
})



testthat::test_that("year-based prevalence works as intended", {


  dt <- data.table::CJ(entry_year = 2001:2003, full_year_fut = 0:5)
  dt[, "id" := 1:.N]
  dt[, "exit_year" := entry_year + full_year_fut]
  dt[, "stratum" := rep(1:2, length.out = .N)]

  obs_y_set <- sort(union(dt[["entry_year"]], dt[["exit_year"]]))
  obs_y_set <- c(obs_y_set[1L] - 1L, obs_y_set, obs_y_set[length(obs_y_set)] + 1L)
  max_fut_year_set <- c(1L, 2L, 3L, 4L, 5L)
  prev_dt <- basicepistats::stat_year_based_prevalent_record_count(
    x = dt,
    entry_year_col_nm = "entry_year",
    exit_year_col_nm = "exit_year",
    observation_years = obs_y_set,
    maximum_follow_up_years = max_fut_year_set
  )

  exp_prev_dt <- data.table::CJ(
    exp_obs_y = obs_y_set, exp_max_fut_year = max_fut_year_set
  )
  for (obs_y in obs_y_set) {
    for (max_fut_year in max_fut_year_set) {
      data.table::set(dt, j = "fut_to_obs_y", value = obs_y - dt[["entry_year"]])
      exp_prev_dt[
        i = exp_obs_y == obs_y & exp_max_fut_year == max_fut_year,
        j = "N" := dt[entry_year <= obs_y & obs_y < exit_year & fut_to_obs_y <= (max_fut_year - 1L), .N]
      ]
      data.table::set(dt, j = "fut_to_obs_y", value = NULL)
    }
  }

  testthat::expect_equal(
    prev_dt[["N"]],
    exp_prev_dt[["N"]]
  )

})


testthat::test_that("year-based prevalence works as intended, vol II", {

  RNGversion("4.0.0")
  set.seed(1337)
  dt <- data.table::data.table(
    entry_year = sample(2001:2010, size = 1000L, replace = TRUE),
    full_year_fut = sample(0:10, size = 1000L, replace = TRUE)
  )
  dt[, "id" := sort(sample(1:900, .N, replace = TRUE))]
  dt[, "exit_year" := entry_year + full_year_fut]
  dt[, "exit_year" := max(exit_year), by = "id"]
  dt[, "full_year_fut" := exit_year - entry_year]
  dt[, "stratum" := rep(1:2, length.out = .N)]
  data.table::setkeyv(dt, c("id", "entry_year"))

  # record-level ---------------------------------------------------------------
  prev_dt <- basicepistats::stat_year_based_prevalent_record_count(
    x = dt,
    entry_year_col_nm = "entry_year",
    exit_year_col_nm = "exit_year",
    observation_years = 2010L,
    maximum_follow_up_years = c(1L, 1e3L)
  )

  exp_oneyear_prev <- dt[entry_year == 2010 & exit_year > 2010, .N]
  exp_total_prev   <- dt[entry_year <= 2010 & exit_year > 2010, .N]

  testthat::expect_equal(
    prev_dt[["N"]],
    c(exp_oneyear_prev, exp_total_prev)
  )

  # subject-level --------------------------------------------------------------
  prev_dt <- basicepistats::stat_year_based_prevalent_subject_count(
    x = dt,
    entry_year_col_nm = "entry_year",
    exit_year_col_nm = "exit_year",
    subject_id_col_nm = "id",
    observation_years = 2010L,
    maximum_follow_up_years = c(1L, 999L)
  )

  exp_oneyear_prev <- dt[!duplicated(id), ][
    entry_year == 2010 & exit_year > 2010, .N
  ]
  exp_total_prev   <- dt[!duplicated(id), ][
    entry_year <= 2010 & exit_year > 2010, .N
  ]

  testthat::expect_equal(
    prev_dt[["N"]],
    c(exp_oneyear_prev, exp_total_prev)
  )


})


