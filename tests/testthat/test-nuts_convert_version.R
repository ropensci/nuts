# Run error tests
#---------------------
test_that("data input not valid", {
  expect_error(
    nuts_convert_version(
      data = 1,
      to_version = "2021",
      variables = c("values" = "absolute")
    ),
    "Input `data` must be a nuts.classified-object."
  )
})

test_that("variables missing", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(data = .,
                           to_version = "2021")
  )
})

test_that("variable not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = "2021",
        variables = c("valuess" = "absolute")
      ),
    "Input `variables` not found in the provided data frame."
  )
})

test_that("variable type not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolutee")
      ),
    "Variable type\\(s\\) not found. Use one of the following: 'absolute' or 'relative'."
  )
})

test_that("to_version invalid", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = "2020",
        variables = c("values" = "absolute")
      ),
    "Input `to_version` invalid. Make sure it is a string and only one of the version years 2006, 2010, 2013, 2016 or 2021."
  )
})

test_that("multiple to_versions provided", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = c("2006", "2020"),
        variables = c("values" = "absolute")
      ),
    "Input `to_version` invalid. Make sure it is a string and only one of the version years 2006, 2010, 2013, 2016 or 2021."
  )
})

test_that("weight invalid", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolute"),
        weight = "pop19"
      ),
    "Input `weight` invalid. Must be either 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
  )
})


test_that("multiple weights provided", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolute"),
        weight = c("pop19", "areaKm")
      ),
    "Input `weight` invalid. Must be either 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
  )
})


test_that("Mixing NUTS codes of different levels", {
  expect_equal(
    expect_error(
      manure %>%
        filter(indic_ag == "I07A_EQ_Y") %>%
        select(-indic_ag) %>%
        filter(grepl("^DE", geo)) %>%
        filter(geo != "DE") %>%
        filter(time == 2003) %>%
        nuts_classify(nuts_code = "geo") %>%
        nuts_convert_version(
          data = .,
          to_version = "2021",
          variables = c("values" = "absolute")
        )
    ) %>%
      grepl("Data contains NUTS codes from multiple levels", .),
    TRUE
  )
})

# Run positive tests
#---------------------
test_that("Converter output spits out correct names", {
  expect_equal(
    manure_2_indic_DE_2003() %>%
      filter(!grepl("ZZ", geo)) %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_convert_version(
        to_version = "2021",
        variables = c("values" = "absolute")
      ) %>%
      names(.)
    ,
    c("to_code", "to_version", "country", "values")
  )
})

test_that("Grouped output equal to non-grouped output", {
  expect_equal({
    manure_2_indic() %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000, 2010)) %>%
      nuts_classify(nuts_code = "geo",
                    data = .,
                    group_vars = "time") %>%
      nuts_convert_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      filter(time == 2000) %>% select(-time)
  },
  manure_2_indic() %>%
    filter(grepl("DE", geo)) %>%
    filter(!grepl("ZZ", geo)) %>%
    filter(time %in% c(2000)) %>%
    nuts_classify(nuts_code = "geo",
                  data = .) %>%
    nuts_convert_version(
      data = .,
      to_version = "2021",
      variables = c("values" = "absolute",
                    "pct" = "relative")
    ))
})

test_that("Multiple groups", {
  expect_equal({
    manure %>%
      filter(nchar(geo) == 5) %>%
      nuts_classify(
        nuts_code = "geo",
        data = .,
        group_vars = c("time", "indic_ag")
      ) %>%
      nuts_convert_version(
        to_version = "2021",
        variables = c("values" = "absolute")
      ) %>%
      dim(.)
  }
  ,
  c(13140, 6))
})


test_that("Missing NUTS codes", {
  expect_equal({
    manure_2_indic_DE_2003()  %>%
      nuts_classify(nuts_code = "geo",
                    data = .) %>%
      nuts_convert_version(
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      filter(!is.na(values)) %>%
      dim(.)
  }
  ,
  c(34, 5))
})


test_that("Missing NUTS codes, reporting share of missing weights per variable", {
  expect_equal({
    manure_2_indic_DE_2003()  %>%
      filter(!geo %in% c("DE93", "DED3")) %>%
      nuts_classify(nuts_code = "geo",
                    data = .) %>%
      nuts_convert_version(
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative"),
        missing_weights_pct = TRUE
      ) %>%
      names(.)
  }
  ,
  c("to_code", "to_version","country", "values", "pct", "values_na_w", "pct_na_w"))
})


test_that("Ignoring missing NUTS codes", {
  expect_equal({
    manure_2_indic_DE_2003()  %>%
      nuts_classify(nuts_code = "geo",
                    data = .) %>%
      nuts_convert_version(
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative"),
        missing_rm = T
      ) %>%
      filter(!is.na(values)) %>%
      dim(.)
  }
  ,
  c(38, 5))
})


test_that("Feeding multiple NUTS versions within groups", {
  expect_equal(
    expect_error(
      manure %>%
        filter(nchar(geo) == 5) %>%
        select(geo, indic_ag, values) %>%
        distinct(geo,  .keep_all = T) %>%
        nuts_classify(
          nuts_code = "geo",
          group_vars = "indic_ag",
          data = .
        ) %>%
        nuts_convert_version(
          to_version = 2021,
          variables = c("values" = "absolute"),
          missing_rm = T
        )
    ) %>%
      grepl("Please make sure...", .),
    TRUE
  )
})


test_that("Feeding multiple NUTS versions within groups. Option most frequent.",
          {
            expect_equal(
              manure %>%
                filter(nchar(geo) == 5) %>%
                select(geo, indic_ag, values) %>%
                distinct(geo,  .keep_all = T) %>%
                nuts_classify(
                  nuts_code = "geo",
                  group_vars = "indic_ag",
                  data = .
                ) %>%
                nuts_convert_version(
                  to_version = 2021,
                  variables = c("values" = "absolute"),
                  multiple_versions = "most_frequent"
                ) %>%
                filter(!is.na(values)) %>%
                dim(),
              c(1005, 5)
            )
          })
