testthat::context("assertions")

testthat::test_that("assertion functions work as expected", {

  assert_user_input_by(NULL)
  assert_user_input_by("my_var")
  assert_user_input_by(data.table::data.table(a = 1:5))

  testthat::expect_error(
    assert_user_input_by(1L)
  )


  assert_user_input_subset(NULL, 2L)
  assert_user_input_subset(c(TRUE, FALSE), 2L)

  testthat::expect_error(
    assert_user_input_subset(c(TRUE, FALSE, TRUE), 4L)
  )

  lapply(subset_style_options(), function(style) {
    assert_user_input_subset_style(style)
  })

})


