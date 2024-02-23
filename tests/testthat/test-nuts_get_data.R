# Run error tests
#---------------------
test_that("data input not valid", {
  expect_error(
    nuts_get_data(
      data = 1
    ),
    "Input `data` must be a nuts.classified-object, not a number."
  )
})
