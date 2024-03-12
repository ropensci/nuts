# Run error tests
#---------------------
test_that("data input not valid", {
  expect_error(
    nuts_aggregate(
      data = 1,
      to_level = 1,
      variables = c("values" = "absolute")
    ),
    "Input `data` must be a nuts.classified-object, not a number."
  )
})

test_that("variables missing", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(data = .,
                         to_level = 1)
  )
})

test_that("variable not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = 1,
        variables = c("valuess" = "absolute")
      ),
    "Input `variables` not found in the provided data frame."
  )
})

test_that("variable type not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = 1,
        variables = c("values" = "absolutee")
      ),
    "Variable type\\(s\\) not found. Use one of the following: 'absolute' or 'relative'."
  )
})


test_that("invalid to_level 1", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = 4,
        variables = c("values" = "absolute")
      ),
    "Input `to_level` invalid. Must be 1 or 2."
  )
})


test_that("invalid to_level 2", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = TRUE,
        variables = c("values" = "absolute")
      ),
    "Input `to_level` invalid. Must be 1 or 2."
  )
})

test_that("multiple to_levels", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = c(2, 3),
        variables = c("values" = "absolute")
      ),
    "Input `to_level` invalid. Must be 1 or 2."
  )
})


test_that("weight invalid", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = 1,
        variables = c("values" = "absolute"),
        weight = "pop19"
      ),
    "Input `weight` invalid. Must be either 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
  )
})

test_that("multiple weights supplied", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        data = .,
        to_level = 1,
        variables = c("values" = "absolute"),
        weight = c("pop19", "areaKm")
      ),
    "Input `weight` invalid. Must be either 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
  )
})



test_that("NUTS codes already at level 2", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        to_level = 2,
        variables = c("values" = "absolute")
      ),
    "NUTS codes already at level 2."
  )
})


# Run positive tests
#---------------------
test_that("Converter output spits out correct names", {
  expect_equal(
    manure %>%
      filter(nchar(geo) == 5) %>%
      filter(!grepl("EU|ME|ZZ", geo)) %>%
      nuts_classify(
        nuts_code = "geo",
        group_vars = c("indic_ag", "time")
      ) %>%
      nuts_aggregate(
        to_level = 2,
        variables = c("values" = "absolute")
      ) %>%
      names(.),
    c("to_code", "country", "indic_ag", "time", "values")
  )
})


test_that("See if all codes are aggregated from level 3 to level 2", {
  expect_equal({
    manure %>%
      filter(nchar(geo) == 5) %>%
      filter(!grepl("EU|ME|ZZ", geo)) %>%
      nuts_classify(nuts_code = "geo",
                    group_vars = c("indic_ag", "time")) %>%
      nuts_aggregate(
        to_level = 2,
        variables = c("values" = "absolute")
      ) %>%
      pull(to_code) %>%
      nchar(.) %>%
      unique()
  },
  4)
})

test_that("See if all codes are aggregated from level 3 to level 1", {
  expect_equal({
    manure %>%
      filter(nchar(geo) == 5) %>%
      filter(!grepl("EU|ME|ZZ", geo)) %>%
      nuts_classify(nuts_code = "geo",
                    group_vars = c("indic_ag", "time")) %>%
      nuts_aggregate(
        to_level = 1,
        variables = c("values" = "absolute")
      ) %>%
      pull(to_code) %>%
      nchar(.) %>%
      unique()
  },
  3)
})


test_that("Grouped output equal to non-grouped output", {
  expect_equal({
    manure_2_indic() %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000, 2010)) %>%
      nuts_classify(nuts_code = "geo",
                    group_vars = "time") %>%
      nuts_aggregate(
        to_level = 1,
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      filter(time == 2000) %>% select(-time) %>%
      as.data.frame()
  },
  manure_2_indic() %>%
    filter(grepl("DE", geo)) %>%
    filter(!grepl("ZZ", geo)) %>%
    filter(time %in% c(2000)) %>%
    nuts_classify(nuts_code = "geo") %>%
    nuts_aggregate(
      to_level = 1,
      variables = c("values" = "absolute",
                    "pct" = "relative")
    ) %>%
    as.data.frame())
})


test_that("Grouped output equal to non-grouped output", {
  expect_equal({
    manure_2_indic() %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000, 2010)) %>%
      nuts_classify(nuts_code = "geo",
                    group_vars = "time") %>%
      nuts_aggregate(
        to_level = 1,
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      filter(time == 2000) %>% select(-time) %>%
      as.data.frame()
  },
  manure_2_indic() %>%
    filter(grepl("DE", geo)) %>%
    filter(!grepl("ZZ", geo)) %>%
    filter(time %in% c(2000)) %>%
    nuts_classify(nuts_code = "geo") %>%
    nuts_aggregate(
      to_level = 1,
      variables = c("values" = "absolute",
                    "pct" = "relative")
    ) %>%
    as.data.frame())
})

test_that("Additional variables unspecified by the user (here: time)", {
  expect_equal(
    manure_2_indic() %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000)) %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        to_level = 1,
        variables = c("values" = "absolute",
                      "pct" = "relative"),
        missing_rm = TRUE
      ) %>%
      names(.),
    c("to_code", "country", "values", "pct")
  )
})

test_that("Feeding multiple NUTS versions within groups", {
  expect_equal(
    expect_error(
      manure %>%
        filter(nchar(geo) == 5) %>%
        select(geo, indic_ag, values) %>%
        distinct(geo,  .keep_all = TRUE) %>%
        nuts_classify(
          nuts_code = "geo",
          group_vars = "indic_ag",
          data = .
        ) %>%
        nuts_aggregate(
          to_level = 1,
          variables = c("values" = "absolute"),
          missing_rm = TRUE
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
                distinct(geo,  .keep_all = TRUE) %>%
                nuts_classify(
                  nuts_code = "geo",
                  group_vars = "indic_ag",
                  data = .
                ) %>%
                nuts_aggregate(
                  to_level = 1,
                  variables = c("values" = "absolute"),
                  multiple_versions = "most_frequent"
                ) %>%
                filter(!is.na(values)) %>%
                dim(),
              c(52, 4)
            )
          })

test_that("Feeding multiple NUTS versions within groups. Option error.",
          {
            expect_error(
              manure %>%
                filter(nchar(geo) == 5) %>%
                select(geo, indic_ag, values) %>%
                distinct(geo,  .keep_all = TRUE) %>%
                nuts_classify(
                  nuts_code = "geo",
                  group_vars = "indic_ag",
                  data = .
                ) %>%
                nuts_aggregate(
                  to_level = 1,
                  variables = c("values" = "absolute"),
                  multiple_versions = "error"
                ),
              "Mixed NUTS versions within groups!"
            )
          })

test_that("Missing NUTS codes, reporting share of missing weights", {
  expect_equal(
    manure_2_indic() %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000)) %>%
      nuts_classify(nuts_code = "geo") %>%
      nuts_aggregate(
        to_level = 1,
        variables = c("values" = "absolute",
                      "pct" = "relative"),
        missing_weights_pct = TRUE
      ) %>%
      names(.),
    c("to_code", "country", "values", "pct", "values_na_w", "pct_na_w")
  )
})


