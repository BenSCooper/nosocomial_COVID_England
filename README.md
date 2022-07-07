# nosocomial\_COVID\_England
 Analysis of nosocomial COVID data in England

This repository contains code needed to reproduce the analysis in the manuscript "The burden and dynamics of hospital-acquired SARS-CoV-2 in England"
For access to data sources please see documentation within the manuscript.

It also contains semi-synthetic data, where fields in data frames that we do not yet have permission to share have been replaced by random draws 
from a saturated Poisson model (i.e. with the same expected value for each point as the observation). These are stored in thie files:

i) synthetic_sitreps_eng_expanded.rds , which contains infection data with hospital-associated infections replaced with model-generated synthesised data; and ii)  synthetic_vacc_cov_by_region.csv, which contains vaccine coverage in HCWs replaced with model-generated synthesised data.

Code to generate synthetic data (given real data) is contained in the file "Create synthetic data.R"

To replicate the analysis copy all files to a directory, make this the current working directory in R, and run the code in the file

"runall.R"

By default this is set to run code using synthetic data, though this can be changed by setting the flag runwithsyntheticdata to FALSE. 





