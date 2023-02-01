# nosocomial\_COVID\_England
Analysis of nosocomial COVID data in England


This repository contains code needed to reproduce the analysis in the manuscript "The burden and dynamics of hospital-acquired SARS-CoV-2 in England"
For access to data sources please see documentation within the manuscript.

It also contains semi-synthetic data, where fields in data frames that we do not yet have permission to share have been replaced by random draws 
from a saturated Poisson model (i.e. with the same expected value for each point as the observation). These are stored in the files:

i) synthetic_sitreps_eng_expanded.rds , which contains infection data with hospital-associated infections replaced with model-generated synthesised data; and ii)  synthetic_vacc_cov_by_region.csv, which contains vaccine coverage in HCWs replaced with model-generated synthesised data. See "Note on generation of synthetic data" below for details of how this synthetic data was generated.

Code to generate synthetic data (given real data) is contained in the file "Create synthetic data.R", though note that for this to work the real data is needed, and for this reason the synthetic data is included in repository.

To replicate the analysis copy all files to a directory, make this the current working directory in R, and run the code in the file

"runall.R"

By default this is set to run code using synthetic data, though this can be changed by setting the flag runwithsyntheticdata to FALSE. 

# Note on generation of synthetic data

The analyses makes use of several types of data from multiple sources. While most of these are publicly available for immediate download, there are two types of data for which formal data access requests are required: vaccine coverage data and data on hospital-onset COVID-19 infections.  To ensure transparency we have therefore created a synthetic versions of both data sets as outlined below. 

1. Vaccine coverage data were available from the SIREN study which we used to derive estimates of the proportion of healthcare workers who had been immunised in each NHS region on each day. As individual is considered to be “immunised”, for this work,  if they had ben given one or more doses of any recognised COVID-19 vaccine at least three weeks previously. We then used staffing data from the NHS to derive estimates of the number of staff who were immunised on each day in each region. Let imm_ij represent the number estimated to be immunised in region i on day j. Synthetic immunisation data syn_imm_ij, were then obtained  by sampling from a saturated Poisson model, i.e. syn_imm_ij, ~ Poisson(imm_ij), and these synthetic immunisation data were in turn used to derive the proportion immunised by date and region used in the model. 

We used a similar procedure to create synthetic data for infections classified as indeterminate healthcare-
associated,  probable healthcare-associated, and definite healthcare-associated. This again required 
sampling from a saturated Poisson model. i.e. the number of healthcare-associated SARS-CoV-2 infections in hospital i on day j was obtained by sampling from a Poisson distribution with a mean given by the observed  number of healthcare-associated  SARS-CoV-2 infections in hospital i on day j.

