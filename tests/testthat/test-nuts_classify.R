# Run error tests
test_that("data not valid", {
  expect_error(
    nuts_classify(data = 1 , nuts_code = "geo"),
    "Input `data` must be a data frame or tibble, not a number."
  )
})

test_that("Needs geo var 1", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = NULL),
    "Input `nuts_code` must be provided as a string, not NULL."
  )
})

test_that("Needs geo var 2", {
  expect_error(manure_2_indic_DE_2003() %>%
                 nuts_classify())
})


test_that("nuts_code not valid", {
  expect_equal(
  expect_error(manure_2_indic_DE_2003() %>%
                 nuts_classify(nuts_code = 1)) %>%
    .[["message"]] %>%
    as.character() %>%
    grepl("must be provided as a string", .),
  TRUE
  )
})


test_that("nuts_code name not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geoo"),
    "Input `nuts_code` not found in the provided data frame."
  )
})

test_that("ties not valid", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo", ties = "most_recentt"),
    "Input `ties` must be 'most_recent' or 'oldest'."
  )
})

test_that("NUTS codes not valid", {
  expect_equal(
    expect_error(
      manure_2_indic_DE_2003() %>%
        mutate(geo = gsub("[A-Z]", "", geo)) %>%
        nuts_classify(nuts_code = "geo")
    ) %>%
      grepl("Variable `geo` contains invalid NUTS codes.", .),
    TRUE
  )
})

test_that("Multiple strings as variable name", {
  expect_equal(
    expect_error(
      manure_2_indic_DE_2003() %>%
        nuts_classify(nuts_code = c("geo", "geo_false"))
      )%>%
      grepl("only be a single string", .),
    TRUE
  )
})

test_that("Multiple levels", {
  expect_equal(
    expect_error(
      patents %>%
        filter(nchar(geo) > 2) %>%
        distinct(geo) %>%
        nuts_classify(data = ., nuts_code = "geo")
    ) %>%
      grepl("Data contains NUTS codes from multiple levels", .),
    TRUE
  )
})

test_that("grouping variable not found", {
  expect_error(
    manure_2_indic_DE_2003() %>%
      nuts_classify(nuts_code = "geo", group_vars = "group"),
    "Input `group_vars` not found in the provided data frame."
  )
})

test_that("no grouping variable was used or NUTS codes are not unique", {
  expect_equal(expect_error(nuts_classify(
    data = manure_2_indic(),
    nuts_code = "geo"
  )) %>%
    grepl("Duplicate NUTS codes found", .),
  TRUE)
})


# Run positive tests
test_that("Classify returns nuts.classified", {
  expect_equal(attr(
    nuts_classify(data = manure_2_indic_DE_2003(),
                  nuts_code = "geo"),
    "class"
  ),
  c("nuts.classified", "list"))
})

test_that("Length of three", {
  expect_equal(length(
    nuts_classify(data = manure_2_indic_DE_2003(),
                  nuts_code = "geo")
  ),
  3)
})

test_that("Dimensions of ouput when using group", {
  expect_equal(dim(
    nuts_classify(
      data = manure_2_indic(),
      nuts_code = "geo",
      group_vars = "time"
    )[[1]]
  ),
  c(704, 7))
})

test_that("Dimensions of within group overlap classification output when using groups",
          {
            expect_equal(dim(
              nuts_classify(
                data = manure_2_indic(),
                nuts_code = "geo",
                group_vars = "time"
              )[[2]]
            ),
            c(346, 4))
          })

test_that("Names of NUTS version classified output when using groups", {
  expect_equal(
    names(
      nuts_classify(
        data = manure_2_indic(),
        nuts_code = "geo",
        group_vars = "time"
      )[[1]]
    ),
    c(
      "from_code",
      "from_version",
      "from_level",
      "country",
      "pct",
      "time",
      "values"
    )
  )
})

test_that("Pass unidentifiable NUTS codes with version = NA", {
  expect_equal(
    nuts_classify(
      data = manure_2_indic(),
      nuts_code = "geo",
      group_vars = "time"
    )[[1]] %>%
      filter(is.na(from_version)) %>%
      dim(.)
    ,
    c(4,7)
  )
})

test_that("Report custom warning that there are multiple versions within groups",
          {
            expect_equal(length(
              manure_2_indic() %>%
                distinct(geo, .keep_all = T) %>%
                nuts_classify(data = ., nuts_code = "geo")
            ),
            3)
          })

test_that("No missing NUTS codes", {
  expect_equal(
    patents %>%
      filter(unit == "NR", nchar(geo) == 4, time == 2012) %>%
      filter(grepl("^DE", geo)) %>%
      nuts_classify(data = ., nuts_code = "geo") %>%
      .[[3]] %>%
      dim(.),
    c(0, 4)
  )
})

test_that("One missing NUTS code", {
  expect_equal(
    patents %>%
      filter(unit == "NR", nchar(geo) == 4, time == 2012) %>%
      filter(grepl("^DE", geo)) %>%
      filter(geo != "DE11") %>%
      nuts_classify(data = ., nuts_code = "geo") %>%
      .[[3]] %>%
      dim(.),
    c(1, 4)
  )
})
