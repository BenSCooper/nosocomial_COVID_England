# Prepare data

source("create_sitrep_weekly.R")
source("merge_nvdata.R")  #add in new variant data to sitrep_weekly
source("merge_vaccine_data.R") # add in vaccine coverage (by region from SIREN study)
source("mergeERICdata.r") #  merge in additional data on trusts such as number of side rooms 
source("merge_hcw_numbers.R")  # merge in HCW numbers 