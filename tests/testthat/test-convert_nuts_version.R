# Load testing data
#----------------------

# 1. A. EUROSTAT data of manure storage facilities
data("manure")

# 1.B. Add fake random shares
set.seed(1234567)
manure <- runif(nrow(manure) , min = 0 , max = 100) %>%
  bind_cols(manure)

names(manure)[1] <- 'pct'

manure_2_indic_DE_2003 <- manure %>%
  filter(nchar(geo) == 4) %>%
  filter(indic_ag == "I07A_EQ_Y") %>%
  select(-indic_ag) %>%
  filter(grepl("^DE", geo)) %>%
  filter(time == 2003) %>%
  select(-time)

manure_2_indic <- manure %>%
  filter(nchar(geo) == 4) %>%
  filter(indic_ag == "I07A_EQ_Y") %>%
  select(-indic_ag)

manure_3 <- manure %>%
  filter(nchar(geo) == 5)



######################################################### TESTS

# Run error tests
#---------------------
test_that("data input not valid", {
  expect_error(
    convert_nuts_version(
      data = 1,
      to_version = "2021",
      variables = c("values" = "absolute")
    ),
    "Input 'data' must be a nuts.classified-object."
  )
})

test_that("variables missing", {
  expect_error(
    manure_2_indic_DE_2003 %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(data = .,
                           to_version = "2021")
  )
})

test_that("variable not found", {
  expect_error(
    manure_2_indic_DE_2003 %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(
        data = .,
        to_version = "2021",
        variables = c("valuess" = "absolute")
      ),
    "Input 'variables' not found in data."
  )
})

test_that("variable type not found", {
  expect_error(
    manure_2_indic_DE_2003 %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolutee")
      ),
    "Variable type\\(s\\) not found. Use one of the following: 'absolute' or 'relative'."
  )
})

test_that("to_version invalid", {
  expect_error(
    manure_2_indic_DE_2003 %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(
        data = .,
        to_version = "2020",
        variables = c("values" = "absolute")
      ),
    "Input 'to_version' invalid. Make sure it is a string and one of the version years 2006, 2010, 2013, 2016 or 2021."
  )
})

test_that("weight invalid", {
  expect_error(
    manure_2_indic_DE_2003 %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolute"),
        weight = "pop19"
      ),
    "Input 'weight' invalid. Must be 'areaKm', 'pop11', 'pop18', 'artif_surf12' or 'artif_surf18'."
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
        classify_nuts(nuts_code = "geo") %>%
        convert_nuts_version(
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
    manure_2_indic_DE_2003 %>%
      filter(!grepl("ZZ", geo)) %>%
      classify_nuts(nuts_code = "geo") %>%
      convert_nuts_version(
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
    manure_2_indic %>%
      filter(grepl("DE", geo)) %>%
      filter(!grepl("ZZ", geo)) %>%
      filter(time %in% c(2000, 2010)) %>%
      classify_nuts(nuts_code = "geo",
                    data = .,
                    group_vars = "time") %>%
      convert_nuts_version(
        data = .,
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      filter(time == 2000) %>% select(-time)
  },
  manure_2_indic %>%
    filter(grepl("DE", geo)) %>%
    filter(!grepl("ZZ", geo)) %>%
    filter(time %in% c(2000)) %>%
    classify_nuts(nuts_code = "geo",
                  data = .) %>%
    convert_nuts_version(
      data = .,
      to_version = "2021",
      variables = c("values" = "absolute",
                    "pct" = "relative")
    ))
})

test_that("Multiple groups", {
  expect_equal({
    manure_3 %>%
      classify_nuts(
        nuts_code = "geo",
        data = .,
        group_vars = c("time", "indic_ag")
      ) %>%
      convert_nuts_version(
        to_version = "2021",
        variables = c("values" = "absolute",
                      "pct" = "relative")
      ) %>%
      dim(.)
  }
  ,
  c(13140, 7))
})


test_that("Missing NUTS codes", {
  expect_equal({
    manure_2_indic_DE_2003  %>%
      classify_nuts(nuts_code = "geo",
                    data = .) %>%
      convert_nuts_version(
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


test_that("Ignoring missing NUTS codes", {
  expect_equal({
    cross_walks %>%
      filter(from_code %in% "DE50")
    manure_2_indic_DE_2003  %>%
      classify_nuts(nuts_code = "geo",
                    data = .) %>%
      convert_nuts_version(
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
        classify_nuts(
          nuts_code = "geo",
          group_vars = "indic_ag",
          data = .
        ) %>%
        convert_nuts_version(
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
                classify_nuts(
                  nuts_code = "geo",
                  group_vars = "indic_ag",
                  data = .
                ) %>%
                convert_nuts_version(
                  to_version = 2021,
                  variables = c("values" = "absolute"),
                  multiple_versions = "most_frequent"
                ) %>%
                filter(!is.na(values)) %>%
                dim(),
              c(1005, 5)
            )
          })
