*net install github, from("https://haghish.github.io/github/")
*github install haghish/rcall


// Run R code directly
rcall: print("Hello from R")

// Import data
rcall: library(nuts)
rcall: data(manure)
rcall: manure
rcall: summary(manure)

rcall: write_dta(manure, "manure.dta")

rcall: library(dplyr)
rcall: library(haven)
rcall: setwd("/home/moritz/dev/nuts/data-raw/")




library(nuts)
library(dplyr)
library(haven)
setwd("/home/moritz/dev/nuts/data-raw/");
data(manure)




cd /home/moritz/dev/nuts/data-raw/
sysuse auto, clear
describe, full
save myauto.dta, replace
rsource, terminator(END_OF_R)
library(haven);
setwd("/home/moritz/dev/nuts/data-raw/");
rauto<-read_dta("myauto.dta", convert.f=TRUE);
rauto;
attributes(rauto);
q();
END_OF_R



# Load packages
library(nuts)
library(dplyr)
library(stringr)

# Loading and subsetting Eurostat data
data(patents, package = "nuts")

pat_n2 <- patents %>% filter(nchar(geo) == 4) # NUTS-2 values

pat_n2_mhab_12_no <- pat_n2 %>%
  filter(unit == "P_MHAB") %>% # Patents per one million inhabitants
  filter(time == 2012) %>% # 2012
  filter(str_detect(geo, "^NO")) %>%  # Norway
  dplyr::select(-unit)

# Classifying the Data
pat_classified <- nuts_classify(
  data = pat_n2_mhab_12_no,
  nuts_code = "geo"
  )
  
  
pat_classified <- nuts_classify(
  data = pat_n2,
  nuts_code = "geo"
  )
