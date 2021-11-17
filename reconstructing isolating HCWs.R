# test area to calculate number of HCWs going into isolation each day from available data which 
# only gives the number currently in isolation
#——reconstructing number of HCWs testing positive each day ———————————




#sitrep<-readRDS("sitreps_eng.rds")
sitrep<-readRDS("sitreps_eng_current.rds")

sitrep<-subset(sitrep, organisation_type=="Acute Trust", select=c(date,region,org_code,org_name, organisation_type, n_inpatients_diagnosed,
                                                                  n_inpatients_diagnosed_0_2, n_inpatients_diagnosed_3_7,  n_inpatients_diagnosed_8_14, 
                                                                  n_inpatients_diagnosed_15_ , n_inpatients_diagnosed_asymptomatic, n_patients_admitted_first, n_patients_admitted_readmitted,  
                                                                  n_care_home_diagnosed, n_care_home_admitted, n_patients_admitted, n_beds_mechanical, n_beds_noninvasive, n_beds_oxygenation, 
                                                                  n_nhs_staff_bed_covid19, n_beds_other_3, n_staff_absent_covid_1, n_staff_absent_1, n_staff_absent_2, hospital_prev, icu_prev,non_cov_ns_prev,n_staff_absent_tt))  
require(lubridate)                                                           
sitrep<-as.data.frame(sitrep)
days_since_Jan12020<-as.integer(sitrep$date - date("2020-01-01"))
sitrep$day<-days_since_Jan12020
  
# now reorder sitrep so that ordered by trust and day
ord<-order(sitrep$org_code, sitrep$day)
sitrep<-sitrep[ord,]


trusts<-unique(sitrep$org_code)
# subtract from n_staff_absent_covid_1 those who we absent due to test and trace
sitrep$n_staff_absent_covid_1<- sitrep$n_staff_absent_covid_1 - sitrep$n_staff_absent_tt
sitrep$hcwcovidisolated_cleaned <- NA
sel.records <- sitrep$day>250

sitrep$hcwcovidisolated_cleaned[sel.records] <-  sitrep$n_staff_absent_covid_1[sel.records]


# next 12 lines are just for plotting. No changes are made to the data
par(mfrow=c(4,4), mar=c(1,1,1,1))

for(i in 33:48){
  t<-trusts[i]
  # now that data is ordered above don't need ordering bit below (but it shouldn't hurt)
  hcwcovidisolated <- sitrep$n_staff_absent_covid_1[sitrep$org_code==t]
  if(sum(!is.na(hcwcovidisolated))>0){
   hcwcovidisolated.days <- sitrep$day[sitrep$org_code==t] 
   days<-  hcwcovidisolated.days[order(hcwcovidisolated.days)]
   hcwcovidisolated<-hcwcovidisolated[order(hcwcovidisolated.days)]
   rm(hcwcovidisolated.days)
   plot(days,hcwcovidisolated,type='l')
   lines(days, sitrep$n_staff_absent_covid_1[sitrep$org_code==t],col='green')
   lines(days, sitrep$hcwcovidisolated_cleaned[sitrep$org_code==t],col='pink')
  }  
}
# Now go through by trust replacing the zeros with last number carried forward. 
# Then go through by hand to identify trusts to exclude based on speaks that go up and odown in  10 days by >100

# eyeballing the daily data (above) suggests data may not be recorded consistently over the first 20 weeks or so
# There are also some sudden spikes which might be artefacts (i.e. coding errors ).
# We also see a big sudden fall everywhere at about 220 days 
# Probably therefore makes sense to dischard the first 250 calendar days of data, and also to work with smoothed data 
# And also where there are sudden daily jumps of over 100, followed by daily falls of at least 100 within 10 days, 
# then assume those are artefacts - and either replace cleaned data with NAs (or consider some imputation)
# in case of spikes to zero...use last number carried forward. 

# First thing then...create new variable, hcwcovidisolated_cleaned
# that is NA for first 250 days, NA, for spikes, and replaces zeros with last number carried forward. 



# for(t in trusts){
  #sel.records<-sitrep$org_code==t
  #sel.days<-sitrep$day[sel.records]
  # select recoreds where day>250 from the same trust
  #sel.records<-sitrep$org_code==t  &sitrep$day>250
  #sitrep$hcwcovidisolated_cleaned[sel.records] <-  sitrep$n_staff_absent_covid_1[sel.records]
# }

# Next thing is to remove spurious zeros. Do this while replacing with last number carried forward 
# if from same trust and if a number

wherethezerosare<- which(sitrep$hcwcovidisolated_cleaned==0)
for(i in wherethezerosare){
  trust<-sitrep$org_code[i]
  if(sitrep$org_code[i-1]==trust & sitrep$hcwcovidisolated_cleaned[i-1]>10 ){ 
    #i.e. if record before is the same trust and at least 10 hcws covid isolated previous day we assume 0 means missing 
    sitrep$hcwcovidisolated_cleaned[i]<-sitrep$hcwcovidisolated_cleaned[i-1] #and do a last number carried forward
  }  else {
    # for the moment do nothing as a true 0 is not completely implausible
  }
}

# now plot cleaned data for covid isolated HCWs for all trusts and visually identify spikes that must represent data coding errors
# we have 150 trusts - so do five plots each with 30 graphs in a 6 x 5 grid 
for(plotnum in 1:5){
  firsttrustinplot<-30*(plotnum-1)+1
  lasttrustinplot<-firsttrustinplot +29
  filename<-paste("covid_isolated_hws_plot_", plotnum, ".pdf")
  pdf(filename, pointsize=10,height=7, width=7)
  par(mfrow=c(5,6), mar=c(4,3,1,1))
  for(i in firsttrustinplot:lasttrustinplot){
    t<-trusts[i]
    # now that data is ordered above don't need ordering bit below (but it shouldn't hurt)
    hcwcovidisolated <- sitrep$n_staff_absent_covid_1[sitrep$org_code==t]
    if(sum(!is.na(hcwcovidisolated))>0){
      hcwcovidisolated.days <- sitrep$day[sitrep$org_code==t] 
      days<-  hcwcovidisolated.days[order(hcwcovidisolated.days)]
      hcwcovidisolated<-hcwcovidisolated[order(hcwcovidisolated.days)]
      rm(hcwcovidisolated.days)
      plot(days,hcwcovidisolated,type='l',main=t,col='grey')
      lines(days, sitrep$hcwcovidisolated_cleaned[sitrep$org_code==t],col='purple')
    }
  }
  dev.off()
}
# Exclude trusts based where there are clear coding errors or inconsistencies in reporting
# R0A90 has no data - exclude 
# R0B has a huge spike ( 126 434 195 covid isolated patients  in consecutive days )  - exclude or replace 434 with last number carried forwward
# R0D has a huge spike (271 431 262) and also some big one day dips (eg 418 309 454  )  - exclude
# RAJ has a huge one day dip 726 532 881 - exclude? or interpolate 
# RBT has a huge one day dip 95   3  97   exclude or interpolate 
# RC1 has no data
# RCU has huge one day drops   172  11 142    and 83   5  91      exclude or interpolate 
#  RD1 has a huge one day spike   142 273 134   - exclude or interpolate
# REM has spikes  and falls that look v odd eg 910 765 844  - exclude
# RGT   - sudden dipp -207   9 185  -  exclude or interpolate
# RHM - sudden one day fall is not credible  332 157 413  - exclude or interpolate
# RHQ - sudden one day fall is not credible  512 243 496   - exclude or interpolate  
# RJE sudden one day spike is not credible  698 1186  713 - exclude or interpolate  
# RJR sudden fall is not plasuible  148 157 159  47 - exclude?
# RKB - one day spike is not plausible 126 271 131   -  exclude or interpolate
# RM3 - sudden one day fall is not credible  359 134 353  -  exclude or interpolate
#  RN5 - sudden one day changes are not credible  235  83 281 293 299 308 308  85 308  -  exclude or interpolate
#  RNA - one day spike is not credible 102 102 221  87   -  exclude or interpolate
# RNS - more than one one day spike that is not credible 48 153  53    -  exclude or interpolate
# RP5 - one day change that is not credible  158  14 148  -  exclude or interpolate
#  RQ3 - one day changes that are not credible  151  68 166   -  exclude or interpolate
#  RRF - one day spikes 302 623 302  -  exclude or interpolate
# RRV - one days spikes 103 206 103  not credible -  exclude or interpolate
#  RTE one days spike exclude or interpolate
#  RTF one days spike exclude or interpolate
#  RTG one day dips exclude or interpolate
#  RTX  one days spike  306 640 357 exclude or interpolate
#  RVW one days spikes exclude or interpolate
#  RWE one day dips  688 346 703 exclude or interpolate
#  RWG - one day spike 233 321 229 exclude or interpolate
# 
#  exclude trusts below from analysis of hcw covid absence days due to unreliable data (can consider interpolating later)
trusts.to.exclude<-c("R0A90",  "R0B","R0D","RAE","RAJ","RA3", "RA7","RBA","RBT","RBV","RC1","RCU","RD1","REM", "RGT", "RHM","RH5","RHQ", "RJE","RJR","RKB","RL1","RM3","RNS","RNA","RN3","RN5","RP5","RQ3","RRF","RRV","RTE","RTF","RTG","RTH","RTX","RVW","RWE","RWF","RWG","RWW")

trusts.to.include<- trusts[!trusts %in% trusts.to.exclude] # ilel these are the trusts where we seem to have reasonably good date. 

sitrep$hcwcovidisolated_cleaned[sitrep$org_code %in% trusts.to.exclude] <- NA

#  Next step is to reconstruct numbers who are being isolated each day. Since we know HCWs have to isolate
#  For 10 full days from the day first symptoms (and can return to work 11 days after the first day of symptoms)
# then if we know when we start the analysis how many have been isolating for 1 day, 2 days et 
# We do this at random...i.e if we know there are x patients isolating, we rendomly divide this up between those who are on the 1,2nd, 3rd etc
# days of isolation. This way we can generate n data-sets of new hcw infections and check results are not sensitivite.
# if we think the probabilities that patients have been isolated for 1,2,3..etc days are not equal we can specify this when sampling.

# Assigning equal probabiligties to day 1,2,3....of isolation seems reasonable here as when we  start using obseravations of isolating covid+
# HCWS is in the trough and numbers in isolation are neither going up nor down. 
# therefore if we make the reasonable assumption that number going into isolation in a trust is con stant over the 10 days before day 251
#  Therefore, we assume that the number coming out of isolation one tenth of those currently in isolation.   
# More formally, 
#Suppose, on day d, we are steady state initially and the same number of HCWs have been testing positive 
#for at least the last 10 days. Then if we want to choose one patient in isolation there is an equal numbers this patient has been in isolation for 1 day, 2 days,…..,10 days,
#so on day d 1/10 of those currently in isolation will come out of isolation. 

#So if on day d we have x(d) patients in isolation, and on day d+1 we have x(d+1) then the number of new 
#patients going into isolation on day d+1 is y(d+1)=  x(d+1) - (x(d)  - x(d)/10)

#Can then repeat calculation for the next nine days, but for day d+11 (when those who went into isolation
#on day d+1 come out of isolation we can replace x(d)/10 with y(d+1), so number of patients going into isolation on day d+11 is y(d+11)= x(d+11) - x(d) + y(d+1).
                                                                      
# to do this, loop over included trusts, and llop over reocrds (can assume they are in ascending date order within a ward)

# Note - a few things to fix here: i) steady state assumption for initial conditions which must be wrong; ii) code assumes, I think, that there is one record
# for each patient day, but in facts there are some days with no records , iii) probably also some issues with time isolation
n<-10 # number of reconstructions of newly isolating hcws to make
for(i in 1:n) eval(str2lang(paste0("sitrep$hcwcovid_daily_isolated",i,"<-NA"))) # create n new dataframe columns to hold reconstructions of daily newly isolated hcw
# sitrep$hcwcovid_daily_isolated<-NA    # this is the reconstructed number of newly isolated hcws per day
sitrep$smoothed_hcwcovid_daily_isolated<-NA

for(t in trusts.to.include){
  daystoinclude<-  sitrep$day[sitrep$org_code==t] 
  daystoinclude<- daystoinclude[daystoinclude>250]
  if(length(daystoinclude)>11){
    # print(daystoinclude)
    selected.records<-sitrep$day %in% daystoinclude & sitrep$org_code==t  #i.e records selected for one trust
    selected.records.numbers<-c(1:length(sitrep$day))[selected.records]
    #  print(sitrep$hcwcovidisolated_cleaned[selected.records])
    firstrecord<-selected.records.numbers[1]
    numberinitiallyisolated<-sitrep$hcwcovidisolated_cleaned[firstrecord]  
    # now sample number isolated for 1...10 days (i.e. sample unknown duration of times they have been isolated)
    # By default we assume that those in isolation when data series starts are equally likely to have been in isolation for
    # 1,2,3,...10 days. Subsequently we will need to consider sensitivity analysis where we consider extreme assumptions
    # that a) all those in isolation at start have been isolation for 10 days, and b) all have been in isolation for 0 days
    numberisolatedfor1to10days<-rmultinom(10,numberinitiallyisolated,rep(1/11,11)) # returns matrix where row indexes isolation days and col indexes sample
    
    for(sim in 1:n){
      newlyisolated <- rep(NA,length(sitrep$day))

      numberleavingisolationinfirst11days<-numberisolatedfor1to10days[,sim]
      for(d in selected.records.numbers){
        if(sitrep$day[d] - sitrep$day[selected.records.numbers[1]]<11) {  # 
          i<-1+sitrep$day[d] - sitrep$day[selected.records.numbers[1]]
          numberleavingisolation<-numberleavingisolationinfirst11days[i]
          numberenteringisolation<-sitrep$hcwcovidisolated_cleaned[d]- (sitrep$hcwcovidisolated_cleaned[d-1] -numberleavingisolation)
          newlyisolated[d]<-numberenteringisolation
          eval(str2lang(paste0("sitrep$hcwcovid_daily_isolated",sim,"[d]<-numberenteringisolation"))) 
        } else {
          numberleavingisolation<-newlyisolated[d-10]  
          numberenteringisolation<-sitrep$hcwcovidisolated_cleaned[d]- (sitrep$hcwcovidisolated_cleaned[d-1] -numberleavingisolation)
          #   sitrep$hcwcovid_daily_isolated[d]<-numberenteringisolation
          newlyisolated[d]<-numberenteringisolation
          eval(str2lang(paste0("sitrep$hcwcovid_daily_isolated",sim,"[d]<-numberenteringisolation"))) 
          
        }
      }
    }
    
    #  numberleavingisolationinfirst11days<-rep(NA,11)
    # numberlefttoleave<-numberinitiallyisolated
    # for(i in 1:11){
    #   if(i<11) {
    #     numberleavingonthisday<-round(numberlefttoleave/11)  
    #   } else {
    #     numberleavingonthisday<-numberlefttoleave
    #   }  
    #   numberlefttoleave<-numberlefttoleave-numberleavingonthisday
    #   numberleavingisolationinfirst11days[i] <-numberleavingonthisday
    # }

    # also add one smoothed line (for the first reconstruction) for comparison
    # but leave out first observation
    selected.records.numbers<-selected.records.numbers[-1]
    daystoinclude<-daystoinclude[-1]
    if(sum(is.na(sitrep$hcwcovid_daily_isolated1[selected.records.numbers]))==0  & length(daystoinclude)>0)
    {  
      print(t)
      smoothed<-loess(sitrep$hcwcovid_daily_isolated1[selected.records.numbers] ~ sitrep$day[selected.records.numbers])
      print(length(smoothed$fitted))
      print(length(selected.records.numbers))
      print(sitrep$hcwcovid_daily_isolated1[selected.records.numbers])
      sitrep$smoothed_hcwcovid_daily_isolated[selected.records.numbers] <- pmax(0,smoothed$fitted)
      #rm(smoothed)
    }  else  {
      # NA exist in  hcwcovid_daily_isolated and dealing with these in smoothing results is more complicates
      # just don't do smoothing in these cases for now, but can add in later 
    }
  }
}



saveRDS(sitrep,"sitreps_eng_expanded.rds")

#  plot calculated numbers entering isolation
# 
# for(plotnum in 1:5){
#   firsttrustinplot<-30*(plotnum-1)+1
#   lasttrustinplot<-firsttrustinplot +29
#   filename<-paste("covid_isolated_hws_plot_(newly isolated in red)", plotnum, ".pdf")
#   pdf(filename, pointsize=10,height=7, width=7)
#   par(mfrow=c(5,6), mar=c(4,3,1,1))
#   for(i in firsttrustinplot:lasttrustinplot){
#     t<-trusts[i]
#     # now that data is ordered above don't need ordering bit below (but it shouldn't hurt)
#     hcwcovidisolated <- sitrep$n_staff_absent_covid_1[sitrep$org_code==t]
#     hcwcovidisolated.days <- sitrep$day[sitrep$org_code==t] 
#     days<-  hcwcovidisolated.days[order(hcwcovidisolated.days)]
#     hcwcovidisolated<-hcwcovidisolated[order(hcwcovidisolated.days)]
#     rm(hcwcovidisolated.days)
#     plot(days,hcwcovidisolated,type='l',main=t,col='grey')
#     lines(days, sitrep$hcwcovidisolated_cleaned[sitrep$org_code==t],col='purple')
#     hcw_covid_newly_isolated<-sitrep$hcwcovid_daily_isolated[sitrep$org_code==t]
#     smoothed_hcwcovid_daily_isolated<-sitrep$smoothed_hcwcovid_daily_isolated[sitrep$org_code==t]
#     par(new=T)
#     plot(days,hcw_covid_newly_isolated,ylim=c(-300,300), col="red" ,type='l',yaxt='n')
#     lines(days,smoothed_hcwcovid_daily_isolated, col="orange")
#     axis(side=4)
#     abline(h=0,col="grey")
#   }
#   dev.off()
# }
# 
