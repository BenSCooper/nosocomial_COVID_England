# Create synthetic data 


temp<-readRDS("sitreps_eng_expanded.rds")


# Fields to replace with synthetic data which we do with a saturated model
# For figure 1
# n_inpatients_diagnosed_0_2
# n_inpatients_diagnosed_3_7
# n_inpatients_diagnosed_8_14
# n_inpatients_diagnosed_15_
# Also need dates and organisation codes


n<-dim(temp)[1]
temp$n_inpatients_diagnosed_0_2  <- rpois(n, temp$n_inpatients_diagnosed_0_2)
temp$n_inpatients_diagnosed_3_7  <- rpois(n, temp$n_inpatients_diagnosed_3_7)
temp$n_inpatients_diagnosed_8_14 <- rpois(n, temp$n_inpatients_diagnosed_8_14)
temp$n_inpatients_diagnosed_15_  <- rpois(n, temp$n_inpatients_diagnosed_15_)


fieldstoinclude<-match(c("date", "region", "org_code", "org_name", "organisation_type","n_inpatients_diagnosed",
                         "n_inpatients_diagnosed_0_2","n_inpatients_diagnosed_3_7","n_inpatients_diagnosed_8_14",
                         "n_inpatients_diagnosed_15_","n_patients_admitted","day","hcwcovidisolated_cleaned",
                         "hcwcovid_daily_isolated1","hcwcovid_daily_isolated2","hcwcovid_daily_isolated3",
                         "hcwcovid_daily_isolated4", "hcwcovid_daily_isolated5","hcwcovid_daily_isolated6",
                         "hcwcovid_daily_isolated7", "hcwcovid_daily_isolated8",
                         "hcwcovid_daily_isolated9", "hcwcovid_daily_isolated10",
                         "smoothed_hcwcovid_daily_isolated"), names(temp))

synthetic_sitreps_eng_expanded <- temp[, fieldstoinclude]

saveRDS(synthetic_sitreps_eng_expanded, "synthetic_sitreps_eng_expanded.rds")



#  Now also create synthetic data for vaccine coverage. 
# Do this in a similar way, converting existing proportions into counts by multiplying by staff numbers in region
# Then sampling from a saturated poisson model to get simulated counts, then converting back into a proportion

# To do this we want to create a synthetic version of vacc_cov_by_region.csv, so first read this in
vax.data<-read.csv(file = "../create_dfs/vacc_cov_by_region.csv",  na.strings = "*")

# then read in staff data and get staff numbers by region
staffdata<-read.csv("..//../additional_data/HCHS staff in NHS Trusts and CCGs in England, Organisation and Job Type CSV.csv")

regionalstaff<-tapply(staffdata$FTE,staffdata$NHSE.region.name, FUN = "sum")
# then create version of vax.data with number by multiplying by staff in each region 
# (to do this split Midelands staff into East and West 50:50, similarly for NE and Yorks)

vax.data2<-vax.data
#East midlands
n<-dim(vax.data)[2]
pos<-match("EM", vax.data$region)
staffFTES<-regionalstaff[3] /2
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#East of England
n<-dim(vax.data)[2]
pos<-match("EE", vax.data$region)
staffFTES<-regionalstaff[1] 
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#London
n<-dim(vax.data)[2]
pos<-match("LN", vax.data$region)
staffFTES<-regionalstaff[2] 
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#NE
n<-dim(vax.data)[2]
pos<-match("NE", vax.data$region)
staffFTES<-regionalstaff[4]/2
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#NW
n<-dim(vax.data)[2]
pos<-match("NW", vax.data$region)
staffFTES<-regionalstaff[5]
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#SE
n<-dim(vax.data)[2]
pos<-match("SE", vax.data$region)
staffFTES<-regionalstaff[6]
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#SW
n<-dim(vax.data)[2]
pos<-match("SW", vax.data$region)
staffFTES<-regionalstaff[7]
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#WM
n<-dim(vax.data)[2]
pos<-match("WM", vax.data$region)
staffFTES<-regionalstaff[3]/2
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])
#Yorkshire
n<-dim(vax.data)[2]
m<-dim(vax.data)[1]
pos<-match("YH", vax.data$region)
staffFTES<-regionalstaff[4]/2
vax.data2[pos,2:n] <- round(staffFTES*vax.data[pos,2:n])

# now create a df with mean new vaccinations on each day in each region baesd on above

newvax.data<-vax.data2
for(j in 2:n){
  for(i in 1:m){
      if(j ==2 ) newinfections<-vax.data2[i,j ] else newinfections<-vax.data2[i,j ] - vax.data2[i,j-1 ]
      if (is.na(newinfections)) newinfections<-0
      sample<-rpois(1,newinfections ) # sample from saturated model
      if(j ==2 ) newvax.data[i,j] <-sample else newvax.data[i,j] <- sample+newvax.data[i,j-1] 
  }
}

# now convert newvax.data back to proportions  to be held in newvax.data2

newvax.data2<-newvax.data
#East midlands
n<-dim(vax.data)[2]
pos<-match("EM", vax.data$region)
staffFTES<-regionalstaff[3] /2
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#East of England
n<-dim(vax.data)[2]
pos<-match("EE", vax.data$region)
staffFTES<-regionalstaff[1] 
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#London
n<-dim(vax.data)[2]
pos<-match("LN", vax.data$region)
staffFTES<-regionalstaff[2] 
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#NE
n<-dim(vax.data)[2]
pos<-match("NE", vax.data$region)
staffFTES<-regionalstaff[4]/2
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#NW
n<-dim(vax.data)[2]
pos<-match("NW", vax.data$region)
staffFTES<-regionalstaff[5]
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#SE
n<-dim(vax.data)[2]
pos<-match("SE", vax.data$region)
staffFTES<-regionalstaff[6]
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#SW
n<-dim(vax.data)[2]
pos<-match("SW", vax.data$region)
staffFTES<-regionalstaff[7]
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#WM
n<-dim(vax.data)[2]
pos<-match("WM", vax.data$region)
staffFTES<-regionalstaff[3]/2
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES
#Yorkshire
n<-dim(vax.data)[2]
m<-dim(vax.data)[1]
pos<-match("YH", vax.data$region)
staffFTES<-regionalstaff[4]/2
newvax.data2[pos,2:n] <- newvax.data[pos,2:n]/staffFTES

write.csv(newvax.data2,file = "synthetic_vacc_cov_by_region.csv" ,row.names=F )










