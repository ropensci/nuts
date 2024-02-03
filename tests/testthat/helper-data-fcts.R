# Load testing data
manure_2_indic_DE_2003 <- function() {
  df <- withr::with_preserve_seed(
      runif(nrow(manure) , min = 0 , max = 100)) %>%
    bind_cols(manure)

  names(df)[1] <- 'pct'

  df <- df %>%
    filter(nchar(geo) == 4) %>%
    filter(indic_ag == "I07A_EQ_Y") %>%
    select(-indic_ag) %>%
    filter(grepl("^DE", geo)) %>%
    filter(time == 2003) %>%
    select(-time)

  return(df)
}

manure_2_indic <- function() {
  df <- withr::with_preserve_seed(
    runif(nrow(manure) , min = 0 , max = 100)) %>%
    bind_cols(manure)

  names(df)[1] <- 'pct'

  df <- df %>%
    filter(nchar(geo) == 4) %>%
    filter(indic_ag == "I07A_EQ_Y") %>%
    select(-indic_ag)

  return(df)
}
