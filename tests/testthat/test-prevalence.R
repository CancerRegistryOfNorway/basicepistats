testthat::test_that("can compute record prevalence", {
  my_dataset <- data.table::data.table(
    id = c(1,1,2,2),
    sex = c(1,1,2,2),
    cancer_type = c(1,1,1,2),
    follow_up_time = c(2.5,1.5, 4.5,3.0),
    calendar_time = c(2001.5,2002.5, 2000.5,2002.0)
  )
  fut_window_widths <- c(1, 5, Inf)
  obs_time_points <- 2001:2003 - 0.001
  observed <- stat_prevalence_count(
    x = my_dataset,
    follow_up_time_col_nm = "follow_up_time",
    follow_up_time_window_widths = fut_window_widths,
    observation_time_points = obs_time_points,
    observation_time_col_nm = "calendar_time"
  )

  expected <- data.table::CJ(
    calendar_time = obs_time_points,
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
    # calendar_time = c(2001.5,2002.5, 2000.5,2002.0)
    calendar_time = as.Date(c("2001-06-15", "2002-06-15", "2000-06-15", "2002-01-01"))
  )
  fut_window_widths <- c(1, 5, Inf) * 365.242199
  obs_time_points <- as.Date(paste0(2000:2002, "-12-31"))
  observed <- stat_prevalence_count(
    x = my_dataset,
    follow_up_time_col_nm = "follow_up_time",
    follow_up_time_window_widths = fut_window_widths,
    observation_time_points = obs_time_points,
    observation_time_col_nm = "calendar_time"
  )

  expected <- data.table::CJ(
    calendar_time = obs_time_points,
    time_since_entry = unique(observed[["time_since_entry"]])
  )
  expected[, "N" := c(1L,1L,1L, 1L,2L,2L, 2L,4L,4L)]


  testthat::expect_equal(
    object = observed[["N"]],
    expected = expected[["N"]]
  )
})
