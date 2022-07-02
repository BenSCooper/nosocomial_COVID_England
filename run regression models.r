#  Run all pmodels to be reported in paper/report of nosocomial transmission using sitrep
#  where dependent variable is patient acquisition. 
#  These are named ModelP1...Modelp8

# Run this after setting up the data using "preparedata.R" and "fig2.R"

source("plot_functions.R") # loads functions for plotting aspects of fitted models

# Then select fields that we need for Stan models and remove rows from data frame where there are NAs
# Do this  for i) data where we have new variant data (stan1data)
# ii) data where ignore the new variant data  (stan2data) iii) data ignoring new variant data but with denominator data (non_cov_ns.prev)
fields.for.stan.v1<-c("org","wk", "ha_cases1","ha_cases2","ha_cases3", "mean_staff_covid_isolated","ca_cases_lag1", 
                      "ca_cases_lag2","ha1_cases_lag1","ha1_cases_lag2", "ha2_cases_lag1", "ha2_cases_lag2","ha3_cases_lag1", 
                      "ha3_cases_lag2", "hcw_cases_lag1","hcw_cases_lag2", "prop_nv",
                      "prop_nv_permuted","hcw_vax_plus2","hcw_vax_plus3")
fields.for.stan.v2<-c("org","wk", "ha_cases1","ha_cases2", "ha_cases3","mean_staff_covid_isolated","ca_cases_lag1", 
                      "ca_cases_lag2","ha1_cases_lag1","ha1_cases_lag2", "ha2_cases_lag1", "ha2_cases_lag2",
                      "ha3_cases_lag1", "ha3_cases_lag2",  "hcw_cases_lag1","hcw_cases_lag2")
fields.for.stan.v3<-c("org","wk", "ha_cases1","ha_cases2","ha_cases3", "mean_staff_covid_isolated","ca_cases_lag1", 
                      "ca_cases_lag2","ha1_cases_lag1","ha1_cases_lag2", "ha2_cases_lag1", "ha2_cases_lag2",
                      "ha3_cases_lag1", "ha3_cases_lag2",  "hcw_cases_lag1","hcw_cases_lag2","non_cov_ns_prev")
fields.for.stan.v4<-c("org","wk", "ha_cases1","ha_cases2","ha_cases3", "mean_staff_covid_isolated","adm_cases","adm_cases_lag1","adm_cases_lag2","ca_cases","ca_cases_lag1", 
                      "ca_cases_lag2","ha1_cases_lag1","ha1_cases_lag2", "ha2_cases_lag1", "ha2_cases_lag2","ha3_cases_lag1", 
                      "ha3_cases_lag2", "hcw_cases_lag1","hcw_cases_lag2", "prop_nv",
                      "prop_nv_permuted","hcw_vax_plus2","hcw_vax_plus3","proportion_single_rooms_std","occupancy_std" ,"percent_pre1965.std", "trustsize.std","trustvolperbed.std","num_hcws")



sel.fields<-match(fields.for.stan.v1,names(sitrep_weekly))
stan1data<-sitrep_weekly[,sel.fields]
countNAs<-function(x){
  sum(is.na(x))
}

# if week is before week 42 if prop_nv==NA assume it is zero 

sel<- is.na(stan1data$prop_nv) & stan1data$wk< 42
stan1data$prop_nv[sel]<- 0
sel<- is.na(stan1data$prop_nv_permuted)
stan1data$prop_nv_permuted[sel]<- 0

rowswithNAs<-apply(stan1data,MARGIN = 1,FUN = countNAs)>0
# this has 6407 rows. 5387  of these have an NA 
stan1data<-stan1data[!rowswithNAs,]
stan1data$hcw_cases_lag1<-round(stan1data$hcw_cases_lag1)
stan1data$hcw_cases_lag2<-round(stan1data$hcw_cases_lag2)
stan1data$mean_staff_covid_isolated<-round(stan1data$mean_staff_covid_isolated)


sel.fields<-match(fields.for.stan.v2,names(sitrep_weekly))
stan2data<-sitrep_weekly[,sel.fields]
rowswithNAs<-apply(stan2data,MARGIN = 1,FUN = countNAs)>0
stan2data<-stan2data[!rowswithNAs,]
stan2data$hcw_cases_lag1<-round(stan2data$hcw_cases_lag1)
stan2data$hcw_cases_lag2<-round(stan2data$hcw_cases_lag2)
stan2data$mean_staff_covid_isolated<-round(stan2data$mean_staff_covid_isolated)

sel.fields<-match(fields.for.stan.v3,names(sitrep_weekly))
stan3data<-sitrep_weekly[,sel.fields]
rowswithNAs<-apply(stan3data,MARGIN = 1,FUN = countNAs)>0
# this has 6968 rows. 4724  of these have an NA 
stan3data<-stan3data[!rowswithNAs,]
stan3data$hcw_cases_lag1<-round(stan3data$hcw_cases_lag1)
stan3data$hcw_cases_lag2<-round(stan3data$hcw_cases_lag2)
stan3data$mean_staff_covid_isolated<-round(stan3data$mean_staff_covid_isolated)

sel.fields<-match(fields.for.stan.v4,names(sitrep_weekly))
stan4data<-sitrep_weekly[,sel.fields]
sel<- is.na(stan4data$prop_nv) & stan4data$wk< 42
stan4data$prop_nv[sel]<- 0
sel<- is.na(stan4data$prop_nv_permuted)
stan4data$prop_nv_permuted[sel]<- 0
rowswithNAs<-apply(stan4data,MARGIN = 1,FUN = countNAs)>0
# this has 6968 rows. 4724  of these have an NA 
stan4data<-stan4data[!rowswithNAs,]
stan4data$hcw_cases_lag1<-round(stan4data$hcw_cases_lag1)
stan4data$hcw_cases_lag2<-round(stan4data$hcw_cases_lag2)
stan4data$mean_staff_covid_isolated<-round(stan4data$mean_staff_covid_isolated)

# first the data with the nv 
sitrep_noso_covid_data1 <- list(
  N = dim(stan1data)[1], #number of observations
  H = length(unique(stan1data$org)),  #number of hospitals
  W = length(unique(stan1data$wk)),   #number of weeks
  #  now get trustindex for each record, starting from 1..
  
  trustindex = match(stan1data$org, unique(stan1data$org)),
  weekindex = match(stan1data$wk, unique(stan1data$wk)),
  ha1 = stan1data$ha_cases1,    #hospital acquired using 3 day definition (i.e hospital onset after >=3 days)
  ha2 = stan1data$ha_cases2,    #hospital acquired using 8 day definition (i.e hospital onset after >=8 days)
  ha3 = stan1data$ha_cases3,    #hospital acquired using 15 day definition (i.e hospital onset after >=15 days)
  
  lag1ha1 = stan1data$ha1_cases_lag1 ,
  lag1ha2 = stan1data$ha2_cases_lag1 ,
  lag1ha3 = stan1data$ha3_cases_lag1 ,
  lag2ha1 = stan1data$ha1_cases_lag2 ,
  lag2ha2 = stan1data$ha2_cases_lag2 ,
  lag2ha3 = stan1data$ha3_cases_lag2 ,
  lag1_ca = stan1data$ca_cases_lag1,
  lag2_ca =stan1data$ca_cases_lag2,
  hcw = stan1data$mean_staff_covid_isolated,
  lag1hcw=stan1data$hcw_cases_lag1,
  lag2hcw=stan1data$hcw_cases_lag2,
  prop_nv=stan1data$prop_nv,
  prop_nv_permuted=stan1data$prop_nv_permuted,
  hcw_vax_plus2=stan1data$hcw_vax_plus2,
  hcw_vax_plus3=stan1data$hcw_vax_plus3
)
temp<-sitrep_noso_covid_data1$weekindex^2 
sitrep_noso_covid_data1$weekindexsquared_std<- (temp-mean(temp))/sd(temp)  # standardised square of week number
temp<-sitrep_noso_covid_data1$weekindex 
sitrep_noso_covid_data1$weekindex_std<- (temp-mean(temp))/sd(temp)  # standardised square of week number



temp<-sitrep_noso_covid_data1$weekindex^2 
sitrep_noso_covid_data1$weekindexsquared_std<- (temp-mean(temp))/sd(temp)  # standardised square of week number
temp<-sitrep_noso_covid_data1$weekindex 
sitrep_noso_covid_data1$weekindex_std<- (temp-mean(temp))/sd(temp)  # standardised square of week number

# then the data without the nv 

sitrep_noso_covid_data2 <- list(
  N = dim(stan2data)[1], #number of observations
  H = length(unique(stan2data$org)),  #number of hospitals
  W = length(unique(stan2data$wk)),   #number of weeks
  #  now get trustindex for each record, starting from 1..
  
  trustindex = match(stan2data$org, unique(stan2data$org)),
  weekindex = match(stan2data$wk, unique(stan2data$wk)),
  ha1 = stan2data$ha_cases1,    #hospital acquired using 3 day definition (i.e hospital onset after >=3 days)
  ha2 = stan2data$ha_cases2,    #hospital acquired using 8 day definition (i.e hospital onset after >=3 days)
  lag1ha1 = stan2data$ha1_cases_lag1 ,
  lag1ha2 = stan2data$ha2_cases_lag1 ,
  lag2ha1 = stan2data$ha1_cases_lag2 ,
  lag2ha2 = stan2data$ha2_cases_lag2 ,
  lag1_ca = stan2data$ca_cases_lag1,
  lag2_ca =stan2data$ca_cases_lag2,
  hcw = stan2data$mean_staff_covid_isolated,
  lag1hcw=stan2data$hcw_cases_lag1,
  lag2hcw=stan2data$hcw_cases_lag2
)


#  Then with added in covariates (and nv data)
sitrep_noso_covid_data4 <- list(
  N = dim(stan4data)[1], #number of observations
  H = length(unique(stan4data$org)),  #number of hospitals
  W = length(unique(stan4data$wk)),   #number of weeks
  #  now get trustindex for each record, starting from 1..
  
  trustindex = match(stan4data$org, unique(stan4data$org)),
  weekindex = match(stan4data$wk, unique(stan4data$wk)),
  ha1 = stan4data$ha_cases1,    #hospital acquired using 3 day definition (i.e hospital onset after >=3 days)
  ha2 = stan4data$ha_cases2,    #hospital acquired using 8 day definition (i.e hospital onset after >=8 days)
  ha3 = stan4data$ha_cases3,    #hospital acquired using 8 day definition (i.e hospital onset after >=15 days)
  adm = stan4data$adm_cases, #community acquired cases 
  ca = stan4data$ca_cases, #community acquired cases 
  lag1ha1 = stan4data$ha1_cases_lag1 ,
  lag1ha2 = stan4data$ha2_cases_lag1 ,
  lag1ha3 = stan4data$ha3_cases_lag1 ,
  lag2ha1 = stan4data$ha1_cases_lag2 ,
  lag2ha2 = stan4data$ha2_cases_lag2 ,
  lag2ha3 = stan4data$ha3_cases_lag2 ,
  lag1_adm = stan4data$adm_cases_lag1,
  lag1_ca = stan4data$ca_cases_lag1,
  lag2_ca =stan4data$ca_cases_lag2,
  hcw = stan4data$mean_staff_covid_isolated,
  lag1hcw=stan4data$hcw_cases_lag1,
  lag2hcw=stan4data$hcw_cases_lag2,
  prop_nv=stan4data$prop_nv,
  prop_nv_permuted=stan4data$prop_nv_permuted,
  hcw_vax_plus2=stan4data$hcw_vax_plus2,
  hcw_vax_plus3=stan4data$hcw_vax_plus3,
  proportion_single_rooms_std=stan4data$proportion_single_rooms_std,
  occupancy_std=stan4data$occupancy_std,
  percent_pre1965_std = stan4data$percent_pre1965.std,
  trustsize_std=stan4data$trustsize.std,
  trustvolperbed_std=stan4data$trustvolperbed.std,
  num_hcws = stan4data$num_hcws
)

sitrep_noso_covid_data4$num_hcws_std<-sitrep_noso_covid_data4$num_hcws/sd(sitrep_noso_covid_data4$num_hcws)
  
trusts<-unique(sitrep_noso_covid_data4$trustindex)
sitrep_noso_covid_data4$lag1hcw_s <- NA # smoothed hcw data - as reporting artifcacts in raw data
for(i in 1:length(trusts)){
  sel<-sitrep_noso_covid_data4$trustindex==trusts[i]
  smoothedlag1hcw<-loess(sitrep_noso_covid_data4$lag1hcw[sel] ~ sitrep_noso_covid_data4$weekindex[sel])
  sitrep_noso_covid_data4$lag1hcw_s[sel] <- pmax(0,round(smoothedlag1hcw$fitted))
  #  plot(sitrep_noso_covid_data4$weekindex[sel],sitrep_noso_covid_data4$lag1hcw[sel],type='l', ylim=c(0,1000)) 
  #  lines(sitrep_noso_covid_data4$weekindex[sel],sitrep_noso_covid_data4$lag1hcw_s[sel],col="red")
}


# The models
# 
# * Model P1: Basic model with patient and HCW covariates but no new variant or vaccine or hospital covariates  
# * Model P2: as P1 but adding hospital covariates from ERIC data (occupancy, single rooms, age, size,vol)
# * Model P3:  adding vaccine  and new variants to P2 (vaccine for HCW only able to alter the rate they transmit)
# * Model P4: as model P3 but with spline on the intercept.
# * Models P5: as P4 but allowing allowing transmission from patients to change over time with a spline, 
# For each of the above due three versions P1_ha1, P1_ha2 and P_ha3 corresponding to acquitions with 4, 8 and 14 day cutoff

# * Models H1: as P1 but for HCW acquisitions
# * Models H2: as P2 but for HCW acquisitions
# * Models H3: as P3 but for HCW acquisitions
# * Models H4: as P4 but for HCW acquisitions
# * Models H5: as P5 but for HCW acquisitions

# ha_cases1 represents cases with onset at least 3 days after admission  n_inpatients_diagnosed_3_7  and above
# ha_cases2 represents cases with onset at least 8 days after admission  n_inpatients_diagnosed_8_14  and above
# ha_cases3 represents cases with onset at least 15 days after admission  n_inpatients_diagnosed_15  
# 
# These are named modelP1.ha1 (using ha_cases1), ModelP1.ha2 (using ha_cases2), ModelP1.ha3 (using ha_cases3),  etc
# 
# Then do corresponding models for HCW infections. 

require("rstan")

# Model 1 : P1_ha1

W <- length(unique(sitrep_noso_covid_data4$weekindex))   #number of weeks
week<- 1:W
sitrep_noso_covid_data4$week <- week

num_trusts<-sitrep_noso_covid_data4$H
initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0),
                     list(a=rep(0.09,num_trusts), b0=0.1, c0=0.1, d0=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1)
)

#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))
modelP1.ha1 <- stan(
  file = "ModelP1.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=TRUE
)

check_hmc_diagnostics(modelP1.ha1)

summary(modelP1.ha1 ,pars=c("a0","b0","c0","d0","sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP1.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","phi0", "k0"))
plot(modelP1.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_k","sigmasq_a","phi0", "k0"))
plot(modelP1.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","phi0", "k0"))
loo(modelP1.ha1)
print(modelP1.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))

modelP2.ha1 <- stan(
  file = "ModelP2.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
summary(modelP2.ha1 ,pars=c("a0","b0","c0","d0","sr0", "vol","occ0","sizetrust0","agetrust0", "sigmasq_a","sigmasq_k","phi0", "k0"))
check_hmc_diagnostics(modelP2.ha1)
plot(modelP2.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","phi0", "k0"))
print(modelP2.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP2.ha1)


initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.5, d0=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1))

    
# P3.ha1

modelP3.ha1 <- stan(
  file = "ModelP3.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
summary(modelP3.ha1 ,pars=c("a0","b0","c0","d0","sr0", "vol","occ0","sizetrust0","agetrust0", "sigmasq_a","sigmasq_k","phi0", "k0"))
check_hmc_diagnostics(modelP3.ha1)
plot(modelP3.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","phi0", "k0"))
print(modelP3.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP3.ha1)

save.image()

# P1.ha2

#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))
modelP1.ha2 <- stan(
  file = "ModelP1.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelP1.ha2)

summary(modelP1.ha2 ,pars=c("a0","b0","c0","d0","sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP1.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","phi0", "k0"))
plot(modelP1.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","phi0", "k0"))
plot(modelP1.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","phi0", "k0"))
print(modelP1.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP1.ha2)

# P2.ha2

modelP2.ha2 <- stan(
  file = "ModelP2.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelP2.ha2)

summary(modelP2.ha2 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP2.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","phi0", "k0"))

plot(modelP2.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","phi0", "k0"))
print(modelP2.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP2.ha2)

#P3.ha2 
initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.5, d0=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1))


modelP3.ha2 <- stan(
  file = "ModelP3.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelP3.ha2)

summary(modelP3.ha2 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol","v0","nv0", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP3.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","v0", "nv0","phi0", "k0"))
plot(modelP3.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","v0", "nv0","phi0", "k0"))
print(modelP3.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP3.ha2)
saveRDS(modelP3.ha2,"modelP3.ha2")
# save.image()



#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))

#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0),
#                     list(a=rep(0.11,num_trusts),a0=0.09, b0=0.15, c0=0.09, d0=0 ))

initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.5, d0=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1))


modelP1.ha3 <- stan(
  file = "ModelP1.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=TRUE
)

check_hmc_diagnostics(modelP1.ha3)
check_n_eff(fit)
summary(modelP1.ha3 ,pars=c("a0","b0","c0","d0","sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP1.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","phi0", "k0"))
plot(modelP1.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","phi0", "k0"))
plot(modelP1.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","phi0", "k0"))
print(modelP1.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP1.ha3)


# P2.ha3

modelP2.ha3 <- stan(
  file = "ModelP2.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelP2.ha3)

summary(modelP2.ha3 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP2.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","phi0", "k0"))

plot(modelP2.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","phi0", "k0"))
print(modelP2.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP2.ha3)




#P3.ha3 
modelP3.ha3 <- stan(
  file = "ModelP3.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelP3.ha3)

summary(modelP3.ha3 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol","v0","nv0", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelP3.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","v0", "nv0","phi0", "k0"))
plot(modelP3.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","nv0","phi0", "k0"))

plot(modelP3.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","v0", "nv0","phi0", "k0"))
print(modelP3.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelP3.ha3)
saveRDS(modelP3.ha3,"modelP3.ha3")

#save.image()

# **************************************
# now models for HCW acquisitions 
# **************************************

#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))
modelH1.ha1 <- stan(
  file = "ModelH1.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH1.ha1)

summary(modelH1.ha1 ,pars=c("a0","b0","c0","d0","sigmasq_a","phi0", "k0"))
summary(modelH1.ha1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))
plot(modelH1.ha1 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))
print(modelH1.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH1.ha1)
saveRDS(modelH1.ha1,"modelH1.ha1")


modelH2.ha1 <- stan(
  file = "ModelH2.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH2.ha1)
summary(modelH2.ha1 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
plot(modelH2.ha1 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","sigmasq_k","phi0", "k0"))
print(modelH2.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH2.ha1)
saveRDS(modelH2.ha1,"modelH2.ha1")



modelH3.ha1 <- stan(
  file = "ModelH3.ha1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH3.ha1)
summary(modelH3.ha1 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelH3.ha1 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
plot(modelH3.ha1 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
print(modelH3.ha1,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH3.ha1)

#save.image()
saveRDS(modelH3.ha1,"modelH3.ha1")


# initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))
modelH1.ha2 <- stan(
  file = "ModelH1.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH1.ha2)

summary(modelH1.ha2 ,pars=c("a0","b0","c0","d0","sigmasq_a","phi0", "k0"))
summary(modelH1.ha2 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))
plot(modelH1.ha2 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))
print(modelH1.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH1.ha2)
saveRDS(modelH1.ha2,"modelH1.ha2")


modelH2.ha2 <- stan(
  file = "ModelH2.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH2.ha2)
summary(modelH2.ha2 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
plot(modelH2.ha2 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0"))
print(modelH2.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH2.ha2)
saveRDS(modelH2.ha2,"modelH2.ha2")


modelH3.ha2 <- stan(
  file = "ModelH3.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH3.ha2)
summary(modelH3.ha2 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelH3.ha2 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
plot(modelH3.ha2 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
print(modelH3.ha2,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH3.ha2)

saveRDS(modelH3.ha2,"modelH3.ha2")


# initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))
modelH1.ha3 <- stan(
  file = "ModelH1.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH1.ha3)

summary(modelH1.ha3 ,pars=c("a0","b0","c0","d0","sigmasq_a","phi0", "k0"))
summary(modelH1.ha3 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))
plot(modelH1.ha3 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))
print(modelH1.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))
loo(modelH1.ha3)
saveRDS(modelH1.ha3,"modelH1.ha3")


modelH2.ha3 <- stan(
  file = "ModelH2.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH2.ha3)
summary(modelH2.ha3 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelH2.ha3 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0"))
plot(modelH2.ha3 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0"))
print(modelH2.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","sigmasq_a","sigmasq_k","phi0", "k0"))
loo(modelH2.ha3)
saveRDS(modelH2.ha3,"modelH2.ha3")



modelH3.ha3 <- stan(
  file = "ModelH3.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelH3.ha3)
summary(modelH3.ha3 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelH3.ha3 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
plot(modelH3.ha3 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
print(modelH3.ha3,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "vol","occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_a","sigmasq_k","phi0", "k0"))

loo(modelH3.ha3)

saveRDS(modelH3.ha3,"modelH3.ha3")

# now model with adm_cases as a negative control 

# Here adm_cases is the number of patients admitted with confirmed COVID19 
# In contrast, ca_cases is "community acquired" cases - those either admitted with COVID19 OR confirmed PCR+ within 48 hours of admission
#initial.values<-list(list(a=rep(0.1,num_trusts),a0=0.1, b0=0.1, c0=0.1, d0=0))

initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.5, d0=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1))



modelADM1.ha2 <- stan(
  file = "ModelADM1.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelADM1.ha2)
summary(modelADM1.ha2 ,pars=c("a0","b0","c0","d0", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelADM1.ha2 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))
print(modelADM1.ha2,pars=c("a0","adm_coeff","ha_coeff","hcw_coeff","sigmasq_a","sigmasq_k","phi0", "k0"))
loo(modelADM1.ha2)

modelADM1.ha3 <- stan(
  file = "ModelADM1.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelADM1.ha3)
summary(modelADM1.ha3 ,pars=c("a0","b0","c0","d0", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelADM1.ha3 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))



modelADM2.ha2 <- stan(
  file = "ModelADM2.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelADM2.ha2)
summary(modelADM2.ha2 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sr0","vol", "occ0","sizetrust0","agetrust0","sigmasq_k","phi0", "k0"))
print(modelADM2.ha2 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sr0","vol", "occ0","sizetrust0","agetrust0","sigmasq_k","phi0", "k0"))
loo(modelADM2.ha2)


modelADM3.ha2 <- stan(
  file = "ModelADM3.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelADM3.ha2)
summary(modelADM3.ha2 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))
print(modelADM3.ha2 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sr0","vol", "occ0","sizetrust0","agetrust0","v0","nv0","sigmasq_k","phi0", "k0"))
loo(modelADM3.ha2)


summary(modelADM3.ha2 ,pars=c("a0","b0","c0","d0", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelADM1.ha3 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sigmasq_k","phi0", "k0"))

modelADM3.ha3 <- stan(
  file = "ModelADM3.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)
check_hmc_diagnostics(modelADM3.ha3)
summary(modelADM3.ha3 ,pars=c("adm_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))


# now model without adjusting for hospital-infected  patients and HCWs

modelADM4.ha3 <- stan(
  file = "ModelADM4.ha3.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelADM4.ha3)
summary(modelADM4.ha3 ,pars=c("adm_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))

# now control with community-acquired cases (either admitted with confirmed infection, or  confirmed in first 48 hours of stay)

modelCA3.ha2 <- stan(
  file = "ModelCA3.ha2.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelCA3.ha2)
summary(modelCA3.ha2 ,pars=c("a0","b0","c0","d0","sr0", "occ0","sizetrust0","agetrust0","vol", "sigmasq_a","sigmasq_k","phi0", "k0"))
summary(modelCA3.ha2 ,pars=c("ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","sigmasq_k","phi0", "k0","v0","nv0"))

# 
# Additional sensitivity analysis using a model with time varying intercepts

require("splines")
require("rstan")
spline_degree <- 3
W<-sitrep_noso_covid_data4$W
week<- 1:W
#  knots 
num_knots <- 6
num_basis <- num_knots + spline_degree - 1
knots <- unname(quantile(1:W,probs=seq(from=0, to=1, length.out = num_knots)))
# then add spline info ot the data 
sitrep_noso_covid_data4$num_knots <- num_knots
sitrep_noso_covid_data4$spline_degree <- spline_degree
sitrep_noso_covid_data4$num_basis <- num_basis
sitrep_noso_covid_data4$knots <- knots
sitrep_noso_covid_data4$week<-week
#P3.ha2 

# four chains
initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1, a_spl_raw=rep(0,num_basis),tau=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.5, d0=0.1, a_spl_raw=rep(0,num_basis),tau=0.1),
                     list(a=rep(0.1,num_trusts), b0=0.1, c0=0.3, d0=0.1, a_spl_raw=rep(0,num_basis),tau=0.1),
                     list(a=rep(0.09,num_trusts), b0=0.05, c0=0.7, d0=0.1, a_spl_raw=rep(0,num_basis),tau=0.1))

# one chain
#initial.values<-list(list(a=rep(0.1,num_trusts), b0=0.1, c0=0.1, d0=0.1, a_spl_raw=rep(0,num_basis),tau=0.1))

modelP3.ha2.tv1 <- stan(
  file = "ModelP3.ha2.tv1.stan",  
  data = sitrep_noso_covid_data4,    # named list of data
  chains = 4,             # number of Markov chains
  init=initial.values,
  warmup = 1000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4,              # number of cores ( one per chain)
  thin=1,
  refresh = 100,
  seed= 42,
  #  control = list(max_treedepth = 15,adapt_delta=0.99),
  control = list(max_treedepth = 15,adapt_delta=0.99),
  verbose=FALSE
)

check_hmc_diagnostics(modelP3.ha2.tv1)
summary(modelP3.ha2.tv1)
summary(modelP3.ha2.tv1 ,pars=c("a0","ca_coeff","ha_coeff","hcw_coeff","sr0", "occ0","sizetrust0","agetrust0","vol","v0", "nv0","phi0", "k0"))
loo(modelP3.ha2.tv1)
saveRDS(modelP3.ha2.tv1,"modelP3.ha2.tv1")

#  Now plot spline function 

require(gtable)
require(ggplot2)
require(gridExtra)
require(bayesplot)
require(hrbrthemes)
require(viridis)

pos<-match(c("spline_component[1]"), names(modelP3.ha2.tv1))

spline_samples<-extract(modelP3.ha2.tv1, pars= names(modelP3.ha2.tv1)[pos:(pos+W-1)], permuted=FALSE)
spline_samples1<-as.matrix(spline_samples[,1,])
spline_means<-apply(spline_samples1,MARGIN = 2,"mean")
ppc_ribbon(spline_means, spline_samples1, y_draw="line") + ggplot2::xlab("Week number") + legend_none()


