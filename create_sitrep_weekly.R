#create sitrep_weekly data


sitrep<-readRDS("sitreps_eng_expanded.rds")   
# the above rds object is created by "reconstructing isolating HCWs.R" and already includes imputed HCW infections


#First step, filter by acute trusts and get weekly totals for the following
# n_inpatients_diagnosed n_inpatients_diagnosed_0_2   n_inpatients_diagnosed_3_7  n_inpatients_diagnosed_8_14   n_inpatients_diagnosed_15_ 
#n_inpatients_diagnosed_asymptomatic  n_patients_admitted_first  n_patients_admitted_readmitted  n_care_home_diagnosed
# n_care_home_admitted  n_patients_admitted   n_beds_mechanical  n_beds_noninvasive n_beds_oxygenation n_nhs_staff_bed_covid19 
#n_beds_other_3  n_staff_absent_covid_1 n_staff_absent_1 n_staff_absent_2 hospital_prev icu_prev  

# #sitrep<-subset(sitrep, organisation_type=="Acute Trust", select=c(date,region,org_code,org_name, organisation_type, n_inpatients_diagnosed,
#                                                                   n_inpatients_diagnosed_0_2, n_inpatients_diagnosed_3_7,  n_inpatients_diagnosed_8_14, 
#                                                                   n_inpatients_diagnosed_15_ , n_inpatients_diagnosed_asymptomatic, n_patients_admitted_first, n_patients_admitted_readmitted,  
#                                                                   n_care_home_diagnosed, n_care_home_admitted, n_patients_admitted, n_beds_mechanical, n_beds_noninvasive, n_beds_oxygenation, 
#                                                                   n_nhs_staff_bed_covid19, n_beds_other_3, n_staff_absent_covid_1, n_staff_absent_1, n_staff_absent_2, hospital_prev, icu_prev))  
sitrep<-subset(sitrep, organisation_type=="Acute Trust", select=c(date,region,org_code,org_name, organisation_type, n_inpatients_diagnosed,
                                                                  n_inpatients_diagnosed_0_2, n_inpatients_diagnosed_3_7,  n_inpatients_diagnosed_8_14, 
                                                                  n_inpatients_diagnosed_15_ , n_inpatients_diagnosed_asymptomatic, n_patients_admitted_first, n_patients_admitted_readmitted,  
                                                                  n_care_home_diagnosed, n_care_home_admitted, n_patients_admitted, n_beds_mechanical, n_beds_noninvasive, n_beds_oxygenation, 
                                                                  n_nhs_staff_bed_covid19, n_beds_other_3, n_staff_absent_covid_1, n_staff_absent_1, n_staff_absent_2, hospital_prev, icu_prev,non_cov_ns_prev, 
                                                                  hcwcovid_daily_isolated1,hcwcovid_daily_isolated2,hcwcovid_daily_isolated3,hcwcovid_daily_isolated4,
                                                                  hcwcovid_daily_isolated5,hcwcovid_daily_isolated6,hcwcovid_daily_isolated7,hcwcovid_daily_isolated8,
                                                                  hcwcovid_daily_isolated9,hcwcovid_daily_isolated10,smoothed_hcwcovid_daily_isolated))  

include<-!(sitrep$org_code %in% c("RP4" , "RBS", "RCU"))
sitrep<-sitrep[include,]
# These excludes these children-only hospitals 
# "GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST" 
# "ALDER HEY CHILDREN'S NHS FOUNDATION TRUST" 
# “SHEFFIELD CHILDREN'S NHS FOUNDATION TRUST" 

require(lubridate)                                                           
sitrep<-as.data.frame(sitrep)

days_since_Jan12020<-as.integer(sitrep$date - date("2020-01-01"))

# now calc week number so days 1to7 are week 1, days 8-14 are week 2 etc
sitrep$week<-days_since_Jan12020 %/% 7

temp<-aggregate(sitrep$non_cov_ns_prev, by=list(org=sitrep$org_code), FUN="max",na.rm=T) # take max of non_cov_ns_prev (number beds occuplied by patients not known or suspected to have covid)
temp$x[temp$x==-Inf] <-NA

sel<-match(sitrep$org_code, temp$org)
sitrep$non_cov_ns_prev2<-temp$x[sel]
rm(sel, temp)


#sitrep_weekly<-aggregate(sitrep[,c(6:26)], by=list(wk=sitrep$week, org=sitrep$org_code), FUN="sum")
sitrep_weekly<-aggregate(sitrep[,c(6:38)], by=list(wk=sitrep$week, org=sitrep$org_code), FUN="sum")

sel<-match("non_cov_ns_prev2",names(sitrep))
temp<-aggregate(sitrep[,c(sel)], by=list(wk=sitrep$week, org=sitrep$org_code), FUN="mean",na.rm=T) # take mean of non_cov_ns_prev (number beds occuplied by patients not known or suspected to have covid)

sitrep_weekly$non_cov_ns_prev <-temp$x

temp<-match(sitrep_weekly$org, sitrep$org_code) 
region<-sitrep$region[temp]
orgname<-sitrep$org_name[temp]
sitrep_weekly$region<-region
sitrep_weekly$org_name<-orgname
rm(region,temp,orgname)

sitrep_weekly$adm_cases<-sitrep_weekly$n_patients_admitted
sitrep_weekly$ca_cases<-sitrep_weekly$n_inpatients_diagnosed_0_2 + sitrep_weekly$n_patients_admitted
sitrep_weekly$ha_cases1<-sitrep_weekly$n_inpatients_diagnosed_3_7 + sitrep_weekly$n_inpatients_diagnosed_8_14 + sitrep_weekly$n_inpatients_diagnosed_15_ 
sitrep_weekly$ha_cases2<-sitrep_weekly$n_inpatients_diagnosed_8_14 + sitrep_weekly$n_inpatients_diagnosed_15_ 
sitrep_weekly$ha_cases3<- sitrep_weekly$n_inpatients_diagnosed_15_ 

sitrep_weekly$mean_staff_absent_covid_1 <- sitrep_weekly$n_staff_absent_covid_1/7  #  weekly mean of staff off with covid
sitrep_weekly$mean_staff_covid_isolated<-round((sitrep_weekly$hcwcovid_daily_isolated1+ 
                                            sitrep_weekly$hcwcovid_daily_isolated2+ 
                                            sitrep_weekly$hcwcovid_daily_isolated3+ 
                                            sitrep_weekly$hcwcovid_daily_isolated4+ 
                                            sitrep_weekly$hcwcovid_daily_isolated5+ 
                                            sitrep_weekly$hcwcovid_daily_isolated6+ 
                                            sitrep_weekly$hcwcovid_daily_isolated7+ 
                                            sitrep_weekly$hcwcovid_daily_isolated8+ 
                                            sitrep_weekly$hcwcovid_daily_isolated9+ 
                                            sitrep_weekly$hcwcovid_daily_isolated10)/10)



# Approach to dealing with negative numbers in HCW infections

# i) replace with NA
# ii) replace with 0
# iii) drop entire trust
# 

approach<- 2
if(approach==1){
  #  approach i) first  replace with NA
  temp<-sitrep_weekly$mean_staff_covid_isolated<0
  sitrep_weekly$mean_staff_covid_isolated[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated1<0
  sitrep_weekly$hcwcovid_daily_isolated1[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated2<0
  sitrep_weekly$hcwcovid_daily_isolated2[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated3<0
  sitrep_weekly$hcwcovid_daily_isolated3[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated4<0
  sitrep_weekly$hcwcovid_daily_isolated4[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated5<0
  sitrep_weekly$hcwcovid_daily_isolated5[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated6<0
  sitrep_weekly$hcwcovid_daily_isolated6[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated7<0
  sitrep_weekly$hcwcovid_daily_isolated7[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated8<0
  sitrep_weekly$hcwcovid_daily_isolated8[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated9<0
  sitrep_weekly$hcwcovid_daily_isolated9[temp] <-NA
  
  temp<-sitrep_weekly$hcwcovid_daily_isolated10<0
  sitrep_weekly$hcwcovid_daily_isolated10[temp] <-NA
} else if(approach==2){
  temp<-sitrep_weekly$mean_staff_covid_isolated<0
  sitrep_weekly$mean_staff_covid_isolated[temp] <-0
  
} else {  #approach 3
  
  # not implemented yet -  but this will remove whole trust from sitrep_weekly if negative numbers,

}



# hcwcovid_daily_isolated1sitrep_weekly$mean_staff_covid_isolated<-sitrep_weekly$smoothed_hcwcovid_daily_isolated/7 # mean new covid isolated based on smoothed absence data
# Now create lagged variables - for  ca_cases by trust ha_cases1 and ha_cases2
trusts<-unique(sitrep_weekly$org)
sitrep_weekly$adm_cases_lag1<-NA
sitrep_weekly$ca_cases_lag1<-NA
sitrep_weekly$ha1_cases_lag1<-NA
sitrep_weekly$ha2_cases_lag1<-NA
sitrep_weekly$ha3_cases_lag1<-NA


sitrep_weekly$hcw_cases_lag1<-NA
for(t in trusts) {
  wksintrust<-sort(sitrep_weekly$wk[sitrep_weekly$org==t] ) # listed in ascending order 
  # then loop through wksintrust updating where relevant
  if(length(wksintrust)>1){
    firstweekfortrust<-T
    secondweekfortrust<-F
    for(w in wksintrust){
      if(firstweekfortrust) { 
        #do nothing 
      }  else {
        sitrep_weekly$adm_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- adm_last_week
        sitrep_weekly$ca_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ca_last_week
        sitrep_weekly$ha1_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha1_last_week
        sitrep_weekly$ha2_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha2_last_week
        sitrep_weekly$ha3_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha3_last_week
        
        sitrep_weekly$hcw_cases_lag1[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- hcw_last_week #i.e. estimated numbers of staff who first isolated due to covid last week.
        
        if(!secondweekfortrust){
          #then give lag2 values
          sitrep_weekly$adm_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- adm_week_before_last_week
          sitrep_weekly$ca_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ca_week_before_last_week
          sitrep_weekly$ha1_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha1_week_before_last_week
          sitrep_weekly$ha2_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha2_week_before_last_week
          sitrep_weekly$ha3_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- ha3_week_before_last_week
          sitrep_weekly$hcw_cases_lag2[sitrep_weekly$org==t & sitrep_weekly$wk ==w] <- hcw_week_before_last_week #i.e. estimated numbers of staff who first isolated due to covid last week.
          
        }
        adm_week_before_last_week<-adm_last_week
        ca_week_before_last_week<-ca_last_week
        ha1_week_before_last_week <- ha1_last_week
        ha2_week_before_last_week <- ha2_last_week
        ha3_week_before_last_week <- ha3_last_week
        
        hcw_week_before_last_week <- hcw_last_week
      }
      adm_last_week<-sitrep_weekly$adm_cases[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      ca_last_week<-sitrep_weekly$ca_cases[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      ha1_last_week<-sitrep_weekly$ha_cases1[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      ha2_last_week<-sitrep_weekly$ha_cases2[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      ha3_last_week<-sitrep_weekly$ha_cases3[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      
      hcw_last_week<-sitrep_weekly$mean_staff_covid_isolated[sitrep_weekly$org==t & sitrep_weekly$wk ==w]
      if(firstweekfortrust){
        firstweekfortrust<-F
        secondweekfortrust<-T
      } else {
        secondweekfortrust<-F
      }
    }
  }
}
# 
# # plot of non covid non suspected covid bed occupancy (non_cov_ns_prev)
# par(mfrow=c(5,5),oma=c(1,0,0,2),mar=c(3,3,0,0))
# firsttrustinplot<-1
# lasttrustinplot<-33
# trusts<-unique(sitrep$org_code)
# for(t in trusts[firsttrustinplot:lasttrustinplot]){
# 
#   temp<-sitrep_weekly$org==t 
#   if(sum(!is.na( sitrep_weekly$mean_staff_covid_isolated[temp])) >3 ){
#      plot(sitrep_weekly$wk[temp], sitrep_weekly$mean_staff_covid_isolated[temp], type='l',col="blue",main=t)
#     abline(h=0,col='red')
#   }
# }
# #       plot(sitrep_weekly$wk[temp], sitrep_weekly$non_cov_ns_prev[temp], ylim=c(0,maxval), type='p',col="red",title=t)
# 
# 
# 
# 
# # trusts<-unique(sitrep$org_code)
# firsttrustinplot<-61
# lasttrustinplot<-80
# par(mfrow=c(4,5),oma=c(0,0,0,0))
# for(t in trusts[firsttrustinplot:lasttrustinplot]){
#   temp<-sitrep_weekly$org==t 
#   if(sum(!is.na( sitrep_weekly$non_cov_ns_prev[temp]))>0){
#       print("yes")
#       print(t)
#       maxval<-max(sitrep_weekly$non_cov_ns_prev[temp],na.rm = T)
#       plot(sitrep_weekly$wk[temp], sitrep_weekly$non_cov_ns_prev[temp], ylim=c(0,maxval), type='p',col="red",title=t)
#   }
# }

# above plot shows very little data on non_cov_ns_prev and not changing substantially over time 
# for now work with the max value - but bear in mind that it may not  be that useful as we don't have individual 
# hospital (site) data - only trust level 



# so non_cov_ns_prev2 can be considered as a denominator (i.e. number exposed in trust - but because trusts 
# are composed of a number of hospitals and we don't have data at hospital level it may not make complete sense)
