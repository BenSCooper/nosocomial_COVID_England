# runs all code using real data if available otherwise using synthetic data

runwithsyntheticdata<-TRUE # if set to true runs all code with synthetic data

if(runwithsyntheticdata) {
  file1 <- "sitreps_eng_expanded.rds"
  file2<- "synthetic_sitreps_eng_expanded.rds"
  file3<- "vacc_cov_by_region.csv"
  file4<- "synthetic_vacc_cov_by_region.csv"
  
  if (file.exists(file1)) {
    file.rename(file1, "realdata_sitreps_eng_expanded.rds")
  } else {
    #  do nothing
  }
  
  if (file.exists(file2)) {
    file.rename(file2, "sitreps_eng_expanded.rds")
  } else {
    cat("Synthetic data file synthetic_sitreps_eng_expanded.rds not found")
  }
  
  if (file.exists(file3)) {
    file.rename(file3, "realdata_vacc_cov_by_region.csv")
  } else {
    #  do nothing
  }
  
  if (file.exists(file4)) {
    file.rename(file4, "vacc_cov_by_region.csv")
  } else {
    cat("Synthetic data file synthetic_vacc_cov_by_region.csv not found")
  }
  
}

# Commands to run all analysis here 
source("preparedata.R")
ssource("fig1.r")
source("fig2.r")
source("run regression models.r")
source("fig3.r")
source("fig4.r")
source("figS1.r")
source("fig S4.r")
source("fig S5.r")


# Cleanup afterwards by renaming files back
if(runwithsyntheticdata) {
  file.rename("vacc_cov_by_region.csv","synthetic_vacc_cov_by_region.csv" )
  file.rename("sitreps_eng_expanded.rds","synthetic_sitreps_eng_expanded.rds")
  file.rename("realdata_vacc_cov_by_region.csv", "vacc_cov_by_region.csv" )
  file.rename("realdata_sitreps_eng_expanded.rds","sitreps_eng_expanded.rds")
}

