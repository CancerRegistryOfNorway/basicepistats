
library("data.table")



test_that("stat_counts works", {
  sls <- data.table::CJ(sex = 1:2, area_2 = 1:5)
  area_sls <- data.table::data.table(
    area_1 = c(1L, 1L, 1L, 2L, 2L), area_2 = 1:5
  )
  sls <- merge(sls, area_sls, by = "area_2")
  data.table::setcolorder(sls, c("sex", "area_1", "area_2"))

  my_dataset <- data.table::data.table(
    sex = 1L,
    area_1 = 1L,
    area_2 = 1L
  )

  count_dt <- stat_count(
    x = my_dataset,
    by = sls
  )

  testthat::expect_equivalent(
    count_dt[, .SD, .SDcols = names(sls)], sls
  )
})




