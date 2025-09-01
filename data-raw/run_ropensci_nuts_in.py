# NUTS R PACKAGE IN PYTHON
#============================
# Use the rOpenSci R package nuts for the conversion of European NUTS codes in Python with the help of this gist.
# More information on the package can be found here: https://docs.ropensci.org/nuts/
#
# [Remark] It may be easier to run this in a conda environment created with:  
# conda create -n r-env r-base r-essentials
# conda activate r-env
# pip install rpy2

# Import modules
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

# Load nuts package
nuts = importr('nuts')

# Load required packages 
ro.r('''
library(nuts)
library(dplyr)
''')

# Load some Eurostat data that ships with the nuts package
ro.r('''
data(manure)
''')
manure_df = ro.r('manure')

# Convert to pandas 
with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
    manure_df = ro.conversion.rpy2py(manure_df)

# Let's only look at NUTS 3 codes (when number of digits in NUTS codes is 5)
manure_df = manure_df[manure_df['geo'].str.len() == 5]

# The indicator I07A_EQ_Y measures the number of manure storage holdings
manure_df = manure_df[manure_df['indic_ag'] == 'I07A_EQ_Y']
manure_df = manure_df.drop(['indic_ag'], axis=1)

# Focus on the year 2010
manure_df = manure_df[manure_df['time'] == 2010]
print(manure_df)


# 1. DETECT NUTS VERSIONS
#--------------------------
# Let's explore how NUTS versions can be found in this Eurostat dataset

# Pass your pandas data frame to R for the conversion
with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
    ro.globalenv['manure_df'] = manure_df

# Convert manure holdings in dataset to level of NUTS codes 2021 version
results = ro.r('''
nuts_classify(nuts_code = "geo", data = manure_df) 
''')

# Convert result back to pandas 
with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
    manure_classified_df = ro.conversion.rpy2py(results)

# 1. This is the original data set augmented with three variables: 
# from_version = detected NUTS version(s)
# from_level = detected NUTS level
# country = detected country
print(manure_classified_df[0])

# 2. Breakdown of the overlap with each NUTS version
print(manure_classified_df[1])

# 3. Missing NUTS codes 
print(manure_classified_df[2])


# 2. CONVERT NUTS VERSIONS
#--------------------------
# Let's convert manure holdings in dataset to level of NUTS codes 2021 version
results = ro.r('''
manure_df %>%
  # We first classify the NUTS codes 
  nuts_classify(nuts_code = "geo") %>%
  # And then convert
  nuts_convert_version(to_version = '2021',
                       weight = 'pop11',
                       variables = c('values' = 'absolute'))
''')

# Convert result back to pandas 
with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
    manure_converted_df = ro.conversion.rpy2py(results)

# Voila, the converted data
print(manure_converted_df)


# 3. AGGREGATE NUTS LEVELS
#--------------------------
# Let's aggregate NUTS 3 to NUTS 2
results = ro.r('''
manure_df %>%
  # We first classify the NUTS codes 
  nuts_classify(nuts_code = "geo") %>%
  # And aggregate
  nuts_aggregate(to_level = 2, 
                 variables = c('values' = 'absolute'))
''')

# Convert result back to pandas 
with ro.conversion.localconverter(ro.default_converter + pandas2ri.converter):
    manure_aggregated_df = ro.conversion.rpy2py(results)

# The data aggregated to nuts level 2
print(manure_aggregated_df)
