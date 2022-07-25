testthat::context("assertions")

testthat::test_that("assertion functions work as expected", {

  assert_is_arg_by(NULL)
  assert_is_arg_by("my_var")
  assert_is_arg_by(data.table::data.table(a = 1:5))

  testthat::expect_error(
    assert_is_arg_by(1L)
  )


  assert_is_arg_subset(NULL, 2L)
  assert_is_arg_subset(c(TRUE, FALSE), 2L)

  testthat::expect_error(
    assert_is_arg_subset(c(TRUE, FALSE, TRUE), 4L)
  )

  lapply(subset_style_options(), function(style) {
    assert_is_arg_subset_style(style)
  })

})


