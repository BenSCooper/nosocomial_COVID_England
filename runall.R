# runs all code using real data if available otherwise using synthetic data
library(lubridate)
library(readr)
library(readxl)
library(PostcodesioR)
library(remotes)
library(ggplot2)
library(vioplot)
library(sm)
library(zoo)
library(ggvoronoi)
library(rgdal)
library(maps)
library(tidyverse)
library(rgeos)
library(maptools)
library(sp)
library(gtable)
library(gridExtra)
library(inlmisc)
library(geogrid)
library(sf)
library(tmap)
library(broom)
library(cartogram)
library(rstan)
library(splines)
library(gtable)
library(gridExtra)
library(bayesplot)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)
library(deSolve)
library(lhs)
library(dplyr)
library(cowplot)
library(GGally)
require(akima)
library(MASS)


runwithsyntheticdata<-TRUE # if set to true runs all code with synthetic data

if(runwithsyntheticdata) {
  file1 <- "sitreps_eng_expanded.rds"
  file2<- "synthetic_sitreps_eng_expanded.rds"
  file3<- "vacc_cov_by_region.csv"
  file4<- "synthetic_vacc_cov_by_region.csv"
  
  if (file.exists(file1) & !file.exists("realdata_sitreps_eng_expanded.rds")) {
    file.rename(file1, "realdata_sitreps_eng_expanded.rds")
  } else {
    #  do nothing
  }
  
  if (file.exists(file2)) {
    file.copy(file2, "sitreps_eng_expanded.rds")
  } else {
    cat("Synthetic data file synthetic_sitreps_eng_expanded.rds not found")
  }
  
  if (file.exists(file3) & !file.exists("realdata_vacc_cov_by_region.csv")) {
    file.rename(file3, "realdata_vacc_cov_by_region.csv")
  } else {
    #  do nothing
  }
  
  if (file.exists(file4)) {
    file.copy(file4, "vacc_cov_by_region.csv")
  } else {
    cat("Synthetic data file synthetic_vacc_cov_by_region.csv not found")
  }
  
}

# Commands to run all analysis here 
source("preparedata.R")
source("fig1.r")
source("fig2.r")
source("run regression models.r")
source("fig3.r")
source("fig4.r")
source("figS1.r")
source("fig S4.r")
# source("fig S5.r")  # code for fig 5 is within "fig S4.r"


# Cleanup afterwards by renaming files back
if(runwithsyntheticdata) {
  # file.rename("vacc_cov_by_region.csv","synthetic_vacc_cov_by_region.csv" )
  # file.rename("sitreps_eng_expanded.rds","synthetic_sitreps_eng_expanded.rds")
  file.copy("realdata_vacc_cov_by_region.csv", "vacc_cov_by_region.csv" )
  file.copy("realdata_sitreps_eng_expanded.rds","sitreps_eng_expanded.rds")
}

