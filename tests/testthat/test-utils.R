

testthat::test_that("level_space_list_to_level_space_data_table works", {
  input <- list(a = NULL, b = data.table::data.table(a = 1:5, b = 1:5), c = 1:3)
  expected_output <- data.table::CJ(b = 1:5, c = 1:3)
  expected_output[, "a" := b]
  data.table::setcolorder(expected_output, c("a", "b", "c"))
  data.table::setkeyv(expected_output, c("a", "b", "c"))
  actual_output <- level_space_list_to_level_space_data_table(input)

  testthat::expect_equal(expected_output, actual_output)
})
