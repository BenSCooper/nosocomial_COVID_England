# fig S4 results of fitting regression models to simulated data 
# 
require(MASS)
draw_generation_interval<-function(n){
  # n is the number of generation intervals to draw
  # Generation time estimates from Ferretti  (Science 2020)
  # Weibull shape = 2.826
  w_shape<-2.826
  # Weibull scale = 5.665
  w_scale<- 5.665
  gen_int<-rweibull(n,shape = w_shape,scale=w_scale)
  #  gen_int<-rep(7,n)
  return(gen_int)
}

sim_data_for_one_trust<-function(community_onset_admissions, hcws_infected_in_community){
  # community_onset_admissions is a vector with length equal to the number of days to simulate
  # ith element contains number of patients with community onset covid (still infectious) admitted on day i
  # hcws_infected_in_community is similar for infections in HCWs acquired in the community
  #function simulates number of days equal to length(community_onset_admissions)
  # and returns a a dataframe holding number of different of infections in the different groups on each day
  MAXGI<-20  #max permitted generation interval
  days<-length(community_onset_admissions)
  ha_infections_P <-rep(0,days + MAXGI ) #hospital acquired infections in patients
  ha_infections_H <-rep(0,days + MAXGI ) #hospital acquired infections in HCWs
  
  for(i in 1:days){
    # total secondary cases from community onset admissions
    if(community_onset_admissions[i]>0){
      y_C <-rnbinom(community_onset_admissions[i],mu=R_CH + R_CP, size=k_C)
      if(sum(y_C)>0){
        gis<-draw_generation_interval(sum(y_C))
        patient_infections<-rbinom(sum(y_C),1,R_CP/(R_CH+R_CP))
        HCW_infections<-1-patient_infections
        for(j in 1:length(gis)){
          if(patient_infections[j]) ha_infections_P[i+gis[j]]<- ha_infections_P[i+gis[j]] +1
          if(HCW_infections[j]) ha_infections_H[i+gis[j]]<- ha_infections_H[i+gis[j]] +1
        }  # end for
      }# end if(sum_ycC) 
    }# end if(community_onset_admissions)
    
    # total secondary cases from community transmission to HCWs
    if(hcws_infected_in_community[i]>0){
      y_C <-rnbinom(hcws_infected_in_community[i],mu=R_HH + R_HP, size=k_H)
      if(sum(y_C)>0){
        gis<-draw_generation_interval(sum(y_C))
        patient_infections<-rbinom(sum(y_C),1,R_HP/(R_HH+R_HP))
        HCW_infections<-1-patient_infections
        for(j in 1:length(gis)){
          if(patient_infections[j]) ha_infections_P[i+gis[j]]<- ha_infections_P[i+gis[j]] +1
          if(HCW_infections[j]) ha_infections_H[i+gis[j]]<- ha_infections_H[i+gis[j]] +1
        }  # end for
      }# end if(sum_ycC) 
    }# end if(community_onset_admissions)
    
    # total secondary cases from  patients who acquired infection in hospital
    if(ha_infections_P[i]>0){
      y_C <-rnbinom(ha_infections_P[i],mu=R_PH + R_PP, size=k_P)
      if(sum(y_C)>0){
        gis<-draw_generation_interval(sum(y_C))
        patient_infections<-rbinom(sum(y_C),1,R_PP/(R_PH+R_PP))
        HCW_infections<-1-patient_infections
        for(j in 1:length(gis)){
          if(patient_infections[j]) ha_infections_P[i+gis[j]]<- ha_infections_P[i+gis[j]] +1
          if(HCW_infections[j]) ha_infections_H[i+gis[j]]<- ha_infections_H[i+gis[j]] +1
        }  # end for
      }# end if(sum_ycC) 
    }# end if(community_onset_admissions)
    
    # total secondary cases from HCWs who acquired infection in this hospital
    if(ha_infections_H[i]>0){
      y_C <-rnbinom(ha_infections_H[i],mu=R_HH + R_HP, size=k_H)
      if(sum(y_C)>0){
        gis<-draw_generation_interval(sum(y_C))
        patient_infections<-rbinom(sum(y_C),1,R_HP/(R_HH+R_HP))
        HCW_infections<-1-patient_infections
        for(j in 1:length(gis)){
          if(patient_infections[j]) ha_infections_P[i+gis[j]]<- ha_infections_P[i+gis[j]] +1
          if(HCW_infections[j]) ha_infections_H[i+gis[j]]<- ha_infections_H[i+gis[j]] +1
        }  # end for
      }# end if(sum_ycC) 
    }# end if(community_onset_admissions)
  }
  
  l<-length(hcws_infected_in_community)
  total_infections_H<-ha_infections_H[1:l]
  total_infections_H<- total_infections_H +hcws_infected_in_community
  results<-data.frame(ca_P=community_onset_admissions[1:l],ha_P=ha_infections_P[1:l], ha_H=ha_infections_H[1:l], total_H=total_infections_H)  
  return(results)
}

gen_observed_data<-function(rawdata, prob_observing_patient_infection, prob_observing_hcw_infection){
  #rawdata is a a dataframe generated by sim_data_for_one_trust
  #prob_observing_patient_infection and prob_observing_hcw_infection correspond to p_P and p_H
  n<-dim(rawdata)[1]
  obs_ha_P<-rbinom(rep(1,n), rawdata$ha_P,prob_observing_patient_infection) 
  obs_ha_H<-rbinom(rep(1,n), rawdata$ha_H,prob_observing_hcw_infection)
  obs_total_H<-rbinom(rep(1,n), rawdata$total_H,prob_observing_hcw_infection)
  obs_ca_P<-rawdata$ca_P   #i.e. for now assume that all those patients admitted with covid are detected 
  # (not unreasonable for periods where admission screening was in place)
  results<-data.frame(o_ca_P= obs_ca_P,o_ha_P=obs_ha_P, o_ha_H=obs_ha_H, o_total_H=obs_total_H)  
  return(results)
}

gen_wkly_total<-function(daily.data, daysinweek=7){
  #function takes a data frame containing daily data,daily.data assumed to be output from gen_observed_data
  #and returns weekly aggregates (i.e. total for each variable for each "week")
  # by default  assumes 7 days in a week, but allows for other possibilities so we can, for example, generate 
  # aggregates at 5 day intervals
  l<- dim(daily.data)[1]
  m<-dim(daily.data)[2]
  daily.data$wk<- 1+(0:(l-1))%/%daysinweek   # adds a week number field, wk
  result<-aggregate(daily.data[,1:m], by=list(week=daily.data$wk), sum, simplify=TRUE)
  return(result)
}

genfakesitrepdata<-function(daystoaggregate=7){
  # returns a data frame holding fake sitrep data but using admissions in real data 
  #daystoaggregate is number of days to do aggregation over, defaults to 7 for weekly aggregates,
  #but might also be a good idea to set this to be equal to the generation interval
  sitrep<-sitreps_eng_expanded <- readRDS("sitreps_eng_expanded.rds")
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
  sitrep$ca_cases<-sitrep$n_inpatients_diagnosed_0_2 + sitrep$n_patients_admitted # community-acquired cases
  trust_codes<-unique(sitrep$org_code)
  
  # initially do some test plots for the first trust 
  sel<-sitrep$org_code==trust_codes[1] & !is.na(sitrep$ca_cases)
  ca_admissions<-sitrep$ca_cases[sel] 
  days<-sitrep$date[sel]
  hcws_infected_in_community<-rep(0,length(days))
  
  #now, starting at day 200, use  ca_admissions as input for simulating hospital data up to time 400
  # then use 100 days data starting from 251 for simulations, generating weekly partially observed data  
  # Do this for both 7 day and 5 days "weeks" 
  raw_sim1<-sim_data_for_one_trust(ca_admissions,hcws_infected_in_community)
  # raw_sim1 now holds information on number of patients and HCWs infected on each day
  obs_sim1<-gen_observed_data(raw_sim1,prob_observing_patient_infection=p_P,prob_observing_hcw_infection=p_H)
  obs_sim1_weekly<-gen_wkly_total(obs_sim1)
  
  # now create data from holding simulations for the trusts 2:100  
  obs_sim1_weekly$trust<-trust_codes[1]
  for(i in 2:100){
    sel<-sitrep$org_code==trust_codes[i] & !is.na(sitrep$ca_cases)
    if(sum(sel)>50){
      ca_admissions<-sitrep$ca_cases[sel] 
      #  ca_admissions<-rpois(length(ca_admissions),2) # for now try Poisson admissions, mean 2
      days<-sitrep$date[sel]
      hcws_infected_in_community<-rep(0,length(days))
      raw_sim1<-sim_data_for_one_trust(ca_admissions,hcws_infected_in_community)
      # raw_sim1 now holds information on number of patients and HCWs infected on each day
      obs_sim1<-gen_observed_data(raw_sim1,prob_observing_patient_infection=p_P,prob_observing_hcw_infection=p_H)
      new_obs_sim1_weekly<-gen_wkly_total(obs_sim1, daysinweek = daystoaggregate)
      new_obs_sim1_weekly$trust<-trust_codes[i]
      obs_sim1_weekly<-rbind(obs_sim1_weekly,new_obs_sim1_weekly)
    }
  }
  standata1<-obs_sim1_weekly
  standata1$ha_cases<-standata1$o_ha_P
  standata1$ca_cases<-standata1$o_ca_P
  standata1$hcw_cases<-standata1$o_total_H
  standata1$hcw_cases_lag1<-NA
  standata1$hcw_cases_lag2<-NA
  standata1$ha_cases_lag1<-NA
  standata1$ha_cases_lag2 <-NA
  standata1$ca_cases_lag1<-NA
  standata1$ca_cases_lag2<-NA
  
  trusts<-unique(standata1$trust)
  for(i in 1:length(trusts)){
    sel<-standata1$trust==trusts[i]
    sel.ind<-c(1:dim(standata1)[1])[sel]
    l<-length(sel.ind)
    standata1$hcw_cases_lag1[sel.ind[2:l]]<-standata1$o_total_H[sel.ind[1:(l-1)]]
    standata1$hcw_cases_lag2[sel.ind[3:l]]<-standata1$o_total_H[sel.ind[1:(l-2)]]
    standata1$ha_cases_lag1[sel.ind[2:l]]<-standata1$o_ha_P[sel.ind[1:(l-1)]]
    standata1$ha_cases_lag2[sel.ind[3:l]]<-standata1$o_ha_P[sel.ind[1:(l-2)]]
    standata1$ca_cases_lag1[sel.ind[2:l]]<-standata1$o_ca_P[sel.ind[1:(l-1)]]
    standata1$ca_cases_lag2[sel.ind[3:l]]<-standata1$o_ca_P[sel.ind[1:(l-2)]]
    
  }
  
  # strip out NAs (i.e. remove rows with NAs in lagged )
  countNAs<-function(x){
    sum(is.na(x))
  }
  
  rowswithNAs<-apply(standata1,MARGIN = 1,FUN = countNAs)>0
  standata1<-standata1[!rowswithNAs,]
  # restrict to week 20 onwards
  standata1<-standata1[standata1$week>19,]
  return(standata1)
}


# First do a plot keeping regression parameters constant but varying detection probabilities 
# Parameters for regression model

R_HH <- 0.5 #mean secondary cases amongst HCWs per case in a HCW
R_HP <- 0.2 #mean secondary cases amongst in-patients per case in a HCW
R_HC <-  NA #not currently used. mean secondary cases in community  per case in a HCW

R_PH <- 0.2 #mean secondary cases amongst HCWs per nosocomial case in a patient
R_PP <- 0.6 #mean secondary cases amongst in-patients per nosocomial case in a patient
R_PC <-  NA #not currently used. mean secondary cases in community  per nosocomial case in a patient

R_CH <- 0.05 #mean secondary cases amongst HCWs via hospital spread per admitted community acquired case in a patient
R_CP <- 0.05 #mean secondary cases amongst in-patients per admitted community acquired case
R_CC <-  NA #not currently used. mean secondary cases in community  per admitted community acquired case



# then  need detection probabilities

p_P<- 1# probability that a patient infected in hospital is detected and recorded as a nosocomial case
p_H <-1  # probability that an infected HCW is detected
p_C <- 1 # probablity that a patient admitted with community onset COVID-19 is detected and recorded in the data

# Negin k values
#  in general this varies over time and space but Max Lau 2020 PNAS paper (Lau, Grenfell, ...Lopman)
# suggests values of 0.5 are reasonable https://www.pnas.org/content/117/36/22430.short

k_C <- 0.5 # clustering parameter for negbin distribution for  secondary cases from CA patients
k_P <- 0.5 # clustering parameter for negbin distribution for  secondary cases from HA patients
k_H <- 0.5 # clustering parameter for negbin distribution for  secondary cases from HCWs



require(gtable)
require(ggplot2)
require(gridExtra)
require(hrbrthemes)
require(viridis)
require(RColorBrewer)


#  sample from a grid to create lookup table for R component values
# samplesA varies R_PP 
p_P.values<-seq(0.1,1,.1)
R_PP.values<-seq(0.05,1.0, 0.05)
nsims<- 20 # number of simulations per combination of values
nrows<-length(p_P.values) * length(R_PP.values) * nsims

ha_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows),R_PP=rep(NA, nrows),  ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
ha_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows),   ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))

for(p in 1:length(p_P.values)){
  for(r in 1:length(R_PP.values)){
    p_P<-p_P.values[p]
    R_PP<-R_PP.values[r]
    print(p); print(r) 
    print("*************")
    simcount=1
    # Model for hospital acquisition in patients with 5-day aggregated data
    while(simcount<(nsims+1)){
      data5<-genfakesitrepdata(daystoaggregate=5) 
      m5<-try(glm.nb(formula = ha_cases ~ ha_cases_lag1  + hcw_cases_lag1 +  ca_cases_lag1 ,data = data5, link = "identity",start=c(0.02,0.7,0.1,0.1)))
      if(class(m5)[1]=="negbin"){
        if(m5$converged){
          row<-nsims*length(R_PP.values)*(p-1)  + nsims*(r-1) +simcount
          print(paste("row=", row))
          print(paste("simcount=", simcount))
          ha_predictors_lookup_5day_agg$p_P[row]<-p_P
          ha_predictors_lookup_5day_agg$R_PP[row] <- R_PP
          ha_predictors_lookup_5day_agg$ha_cases_lag1[row]<-m5$coefficients[2]
          ha_predictors_lookup_5day_agg$hcw_cases_lag1[row]<-m5$coefficients[3]
          ha_predictors_lookup_5day_agg$ca_cases_lag1[row]<-m5$coefficients[4]      
          simcount<-simcount+1
        }
      }
    }
    
  }
}
samplesA<-ha_predictors_lookup_5day_agg
# First to Plot for Hospital-acquired patient to Patient

m<-aggregate(samplesA$ha_cases_lag1, list(p_P=samplesA$p_P,R_PP=samplesA$R_PP),mean)   
# geom_contour will fail unless we interpolate first. 
# Use the akima package for this.
require(akima)
require(MASS)
m2<-interp(x=m$p_P, y=m$x,  z=m$R_PP)
filled.contour(m2,xlab="Probability of detecting hospital acquired infection", ylab="coefficient",key.title = title(main="R"))
# ggplot version below...
m3<-data.frame(x=rep(m2$x, length(m2$y)), y=rep(m2$y, each=length(m2$x)), z=as.vector(m2$z))

p_P2P <- ggplot(m3, aes( x, y, z=z))
p_P2P<-p_P2P +
  geom_contour_filled() +
  theme_ipsum() +
  scale_fill_discrete(name = "Reproduction number") +
  scale_fill_brewer(palette = "OrRd") +
  xlab("Proportion of patient infections acquired in hospital detected") +
  ylab("coefficient estimate") +
  labs(fill="R",title="Hospital-acquired patient to patient") 
# scale_fill_brewer(palette = "Oranges")
#  scale_fill_brewer()
p_P2P
#  Next for HCW to Patient
# samplesB varies R_HP.values
R_PP<-0.6
p_P.values<-seq(0.1,1,.1)
R_HP.values<-seq(0.05,1.0, 0.05)
nsims<- 5 # number of simulations per combination of values
nrows<-length(p_P.values) * length(R_PP.values) * nsims

ha_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows),R_PP=rep(NA, nrows),  ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
ha_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows),   ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))

for(p in 1:length(p_P.values)){
  for(r in 1:length(R_HP.values)){
    p_P<-p_P.values[p]
    R_HP<-R_HP.values[r]
    print(p); print(r) 
    print("*************")
    simcount=1
    # Model for hospital acquisition in patients with 5-day aggregated data
    while(simcount<(nsims+1)){
      data5<-genfakesitrepdata(daystoaggregate=5) 
      m5<-try(glm.nb(formula = ha_cases ~ ha_cases_lag1  + hcw_cases_lag1 +  ca_cases_lag1 ,data = data5, link = "identity",start=c(0.02,0.7,0.1,0.1)))
      if(class(m5)[1]=="negbin"){
        if(m5$converged){
          row<-nsims*length(R_HP.values)*(p-1)  + nsims*(r-1) +simcount
          print(paste("row=", row))
          print(paste("simcount=", simcount))
          ha_predictors_lookup_5day_agg$p_P[row]<-p_P
          ha_predictors_lookup_5day_agg$R_HP[row] <- R_HP
          ha_predictors_lookup_5day_agg$ha_cases_lag1[row]<-m5$coefficients[2]
          ha_predictors_lookup_5day_agg$hcw_cases_lag1[row]<-m5$coefficients[3]
          ha_predictors_lookup_5day_agg$ca_cases_lag1[row]<-m5$coefficients[4]      
          simcount<-simcount+1
        }
      }
    }
    
  }
}

samplesB<-ha_predictors_lookup_5day_agg

m<-aggregate(samplesB$hcw_cases_lag1, list(p_P=samplesB$p_P,R_HP=samplesB$R_HP),mean)   
# geom_contour will fail unless we interpolate first. 
# Use the akima package for this.
require(akima)
m2<-interp(x=m$p_P, y=m$x,  z=m$R_HP)
filled.contour(m2,xlab="Probability of detecting hospital acquired infection", ylab="coefficient",key.title = title(main="R"))
# ggplot version below...
m3<-data.frame(x=rep(m2$x, length(m2$y)), y=rep(m2$y, each=length(m2$x)), z=as.vector(m2$z))

p_H2P <- ggplot(m3, aes( x, y, z=z))
p_H2P<-p_H2P +
  geom_contour_filled() +
  theme_ipsum() +
  scale_fill_discrete(name = "Reproduction number") +
  scale_fill_brewer(palette = "OrRd") +
  xlab("Proportion of patient infections acquired in hospital detected") +
  ylab("coefficient estimate") +
  labs(fill="R",title="HCW to patient") 
# scale_fill_brewer(palette = "Oranges")
#  scale_fill_brewer()
p_H2P
# Next  plot for community acquired  to Patient

# samplesC varies R_CP.values 

ha_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_5day_agg<-data.frame(p_P=rep(NA,nrows),R_PP=rep(NA, nrows),  ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
ha_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows),   ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
hcw_predictors_lookup_7day_agg<-data.frame(p_P=rep(NA,nrows), R_PP=rep(NA, nrows), ha_cases_lag1=rep(NA,nrows), hcw_cases_lag1=rep(NA,nrows), ca_cases_lag1=rep(NA,nrows))
R_PP<-0.6
R_HP <- 0.2 
R_CP.values<-seq(0.05,1.0, 0.05)
nsims<-5
for(p in 1:length(p_P.values)){
  for(r in 1:length(R_CP.values)){
    p_P<-p_P.values[p]
    R_CP<-R_CP.values[r]
    print(p); print(r) 
    print("*************")
    simcount=1
    # Model for hospital acquisition in patients with 5-day aggregated data
    while(simcount<(nsims+1)){
      data5<-genfakesitrepdata(daystoaggregate=5) 
      m5<-try(glm.nb(formula = ha_cases ~ ha_cases_lag1  + hcw_cases_lag1 +  ca_cases_lag1 ,data = data5, link = "identity",start=c(0.02,0.7,0.1,0.1)))
      if(class(m5)[1]=="negbin"){
        if(m5$converged){
          row<-nsims*length(R_CP.values)*(p-1)  + nsims*(r-1) +simcount
          print(paste("row=", row))
          print(paste("simcount=", simcount))
          ha_predictors_lookup_5day_agg$p_P[row]<-p_P
          ha_predictors_lookup_5day_agg$R_CP[row] <- R_CP
          ha_predictors_lookup_5day_agg$ha_cases_lag1[row]<-m5$coefficients[2]
          ha_predictors_lookup_5day_agg$hcw_cases_lag1[row]<-m5$coefficients[3]
          ha_predictors_lookup_5day_agg$ca_cases_lag1[row]<-m5$coefficients[4]      
          simcount<-simcount+1
        }
      }
    }
    
  }
}
samplesC<-ha_predictors_lookup_5day_agg

m<-aggregate(ha_predictors_lookup_5day_agg$ca_cases_lag1, list(p_P=ha_predictors_lookup_5day_agg$p_P,R_CP=ha_predictors_lookup_5day_agg$R_CP),mean)   
#m<-aggregate(ha_predictors_lookup_5day_agg$ca_cases_lag1, list(p_P=ha_predictors_lookup_5day_agg$p_P,R_HP=ha_predictors_lookup_5day_agg$R_HP),mean)   

# geom_contour will fail unless we interpolate first. 
# Use the akima package for this.
require(akima)
m2<-interp(x=m$p_P, y=m$x,  z=m$R_CP)
filled.contour(m2,xlab="Probability of detecting hospital acquired infection", ylab="coefficient",key.title = title(main="R"))
# ggplot version below...
m3<-data.frame(x=rep(m2$x, length(m2$y)), y=rep(m2$y, each=length(m2$x)), z=as.vector(m2$z))

p_C2P <- ggplot(m3, aes( x, y, z=z))
p_C2P +
  geom_contour_filled() +
  theme_ipsum() +
  scale_fill_discrete(name = "Reproduction number") +
  scale_fill_brewer(palette = "OrRd") +
  xlab("Proportion of patient infections acquired in hospital detected") +
  ylab("Coefficient estimate") +
  labs(fill="R component",title="Community onset admission to patient") +
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size = rel(1.5)))

# scale_fill_brewer(palette = "Oranges")
#  scale_fill_brewer()
R_CP <- 0.05 
#save.image()

