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

# Run positive tests
test_that("Classify returns nuts.classified", {
  expect_equal(attr(
    nuts_classify(data = manure_2_indic_DE_2003(),
                  nuts_code = "geo") %>%
      nuts_get_data(),
    "class"
  ),
  c("tbl_df", "tbl", "data.frame"))
})

test_that("Dimensions of ouput", {
  expect_equal(dim(
    nuts_classify(data = manure_2_indic_DE_2003(),
                  nuts_code = "geo") %>%
      nuts_get_data()
  ),
  c(36, 6))
})
