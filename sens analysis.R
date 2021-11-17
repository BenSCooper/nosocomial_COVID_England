# Sensitivity analysis for estimation of number of cases 

# need to run fig1.R before running this
 
LoS_by_Quarter <- readRDS("~/Dropbox/studies/covid19/nosocomial transmission stuff/sitrep/final_analysis/los data/LoS_by_Quarter.rds")
LoS_by_Quarter<-as.data.frame(LoS_by_Quarter)

LoS_by_Trust <- readRDS("~/Dropbox/studies/covid19/nosocomial transmission stuff/sitrep/final_analysis/los data/LoS_by_Trust.rds")
LoS_by_Trust<-as.data.frame(LoS_by_Trust)
trustIDS<- unique(LoS_by_Trust[,1]) #  note that this includes only 134 - we don't have LoS data from all trusts in the sitrep analysis
#  First take LoS_by_Quarter adn get four distributions applying to all trusts combined 
LoSQ1<-LoS_by_Quarter[LoS_by_Quarter[,2]=="Q1:mar-may" ,] 
LoSQ2<-LoS_by_Quarter[LoS_by_Quarter[,2]=="Q2:jun-aug" ,]
LoSQ3<-LoS_by_Quarter[LoS_by_Quarter[,2]=="Q3:sep-nov" ,] 
LoSQ4<-LoS_by_Quarter[LoS_by_Quarter[,2]=="Q4: dec-feb" ,] 

agg_los_distQ1<-tapply(LoSQ1$count, LoSQ1$LoS, "sum")
agg_los_distQ2<-tapply(LoSQ2$count, LoSQ2$LoS, "sum")
agg_los_distQ3<-tapply(LoSQ3$count, LoSQ3$LoS, "sum")
agg_los_distQ4<-tapply(LoSQ4$count, LoSQ4$LoS, "sum")

# exclude episodes with a los of 0 days and normalise
agg_los_distQ1<-agg_los_distQ1[2:length(agg_los_distQ1)]
agg_los_distQ1<-agg_los_distQ1/sum(agg_los_distQ1)

agg_los_distQ2<-agg_los_distQ2[2:length(agg_los_distQ2)]
agg_los_distQ2<-agg_los_distQ2/sum(agg_los_distQ2)

agg_los_distQ3<-agg_los_distQ3[2:length(agg_los_distQ3)]
agg_los_distQ3<-agg_los_distQ3/sum(agg_los_distQ3)

agg_los_distQ4<-agg_los_distQ4[2:length(agg_los_distQ4)]
agg_los_distQ4<-agg_los_distQ4/sum(agg_los_distQ4)


# plot(as.numeric(names(agg_los_dist)), log(agg_los_dist))
# over 99.5% of episodes are less than 100 days  sum(agg_los_distQ1[1:100]) is 0.997
# so we truncate the los distribution at 100 days 
agg_los_distQ1<-agg_los_distQ1[1:100]/sum(agg_los_distQ1[1:100])  
agg_los_distQ2<-agg_los_distQ2[1:100]/sum(agg_los_distQ2[1:100])  
agg_los_distQ3<-agg_los_distQ3[1:100]/sum(agg_los_distQ3[1:100])  
agg_los_distQ4<-agg_los_distQ4[1:100]/sum(agg_los_distQ4[1:100])  

plot(agg_los_distQ1, xlab="LoS", ylab="Proportion of patient admissions", type='l')
lines(agg_los_distQ2, col="pink")
lines(agg_los_distQ3, col="green")
lines(agg_los_distQ4, col="red")

# so visually LoS distributions for patients not admitted with COVID look very similar
# in all four quarters

source("function_defs.R")
num.iterations<-100
maxlos<- 100
prob.i1.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.i2.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.i3.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.i4.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)

prob.j1.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.j2.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.j3.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
prob.j4.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)



set.seed(42); 
include.uncertainty.in.pcr.sensitivity<-TRUE
mean_prob_sympt<-1-.179 # source: https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.10.2000180/
sd_mean_prob_sympt <- (0.202- 0.155)/(2*1.96) 
prob_sympt<-mean_prob_sympt
include.uncertainty.in.prob_sympt.<-TRUE
sampled.pcr.sensitivity <-rep(0, maxlos)

# 1. Breaking down LoS by 4 different periods

for(i in 1:num.iterations){
  # sample incubation period
  print(i)
  lnorm.param1<-rnorm(1, mean.param1, sd.param1)
  lnorm.param2<-rnorm(1, mean.param2, sd.param2)
  temp<-plnorm(incperiod, lnorm.param1, lnorm.param2)
  prob.inc.period.is.x.days<-c(temp[1],diff(temp))
  # Using data from Hellewell 2021 et al. 
  sampled.pcr.sensitivity[1:30] <-sens_post_daily[sample(1:4000,1),]
  if(include.uncertainty.in.prob_sympt.){
    prob_sympt<-rnorm(1,mean_prob_sympt, sd_mean_prob_sympt)
  } 
  
  if(include.uncertainty.in.pcr.sensitivity){
    probs.i1<-CalcProbDetectedOnDay(los=agg_los_distQ1,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.i2<-CalcProbDetectedOnDay(los=agg_los_distQ2,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.i3<-CalcProbDetectedOnDay(los=agg_los_distQ3,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.i4<-CalcProbDetectedOnDay(los=agg_los_distQ3,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    
    probs.j1<-CalcProbDetectedOnDay(los=agg_los_distQ1,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.j2<-CalcProbDetectedOnDay(los=agg_los_distQ2,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.j3<-CalcProbDetectedOnDay(los=agg_los_distQ3,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    probs.j4<-CalcProbDetectedOnDay(los=agg_los_distQ4,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
    
  } else {
    # not currently implemented
  }
  
  prob.i1.matrix[i,] <- cumsum(probs.i1)
  prob.i2.matrix[i,] <- cumsum(probs.i2)
  prob.i3.matrix[i,] <- cumsum(probs.i3)
  prob.i4.matrix[i,] <- cumsum(probs.i4)
  
  prob.j1.matrix[i,] <- cumsum(probs.j1)
  prob.j2.matrix[i,] <- cumsum(probs.j1)
  prob.j3.matrix[i,] <- cumsum(probs.j3)
  prob.j4.matrix[i,] <- cumsum(probs.j4)
  
}


prob.detected.from.day15.i1  <- data.frame(scenario=rep("i",100), probability.detected= prob.i1.matrix[,100] -  prob.i1.matrix[,14])
prob.detected.from.day15.i2  <- data.frame(scenario=rep("i",100), probability.detected= prob.i2.matrix[,100] -  prob.i2.matrix[,14])
prob.detected.from.day15.i3  <- data.frame(scenario=rep("i",100), probability.detected= prob.i3.matrix[,100] -  prob.i3.matrix[,14])
prob.detected.from.day15.i4  <- data.frame(scenario=rep("i",100), probability.detected= prob.i4.matrix[,100] -  prob.i4.matrix[,14])

prob.detected.from.day15.j1  <- data.frame(scenario=rep("j",100), probability.detected= prob.j1.matrix[,100] -  prob.j1.matrix[,14])
prob.detected.from.day15.j2  <- data.frame(scenario=rep("j",100), probability.detected= prob.j2.matrix[,100] -  prob.j2.matrix[,14])
prob.detected.from.day15.j3  <- data.frame(scenario=rep("j",100), probability.detected= prob.j3.matrix[,100] -  prob.j3.matrix[,14])
prob.detected.from.day15.j4  <- data.frame(scenario=rep("j",100), probability.detected= prob.j4.matrix[,100] -  prob.j4.matrix[,14])


numsims<-100 # i..e number of sample probabilities of detection (which depend on uncertainty in inc period, pcr sensitivity, proportion symptomatic)
numdrawsfromposteriorpersim<-100
numsamples<-numsims*numdrawsfromposteriorpersim
maxcases<-20000 #  the maximum conceivable weekly number of nosocomial infections


nosocomial15day.def<-sitrep_time_series$n_inpatients_diagnosed_15_

days.since.31.12.19<-as.integer(sitrep_time_series$date-as.Date("2019-12-31"))

wk.no<-1+(days.since.31.12.19 %/% 7)

nosocomial15day.weeklytotals<-tapply(nosocomial15day.def,wk.no, FUN = "sum",na.rm=T)

estimated.total.nosocomial.m<-matrix(NA,nrow=numsamples, ncol<-length(nosocomial15day.weeklytotals), dimnames=list(sample=1:numsamples,week=names(nosocomial15day.weeklytotals)))
estimated.total.nosocomial.m.oxf<-matrix(NA,nrow=numsamples, ncol<-length(nosocomial15day.weeklytotals), dimnames=list(sample=1:numsamples,week=names(nosocomial15day.weeklytotals)))
for(i in 1:numsims){
  print(i)
  
  p1<-prob.detected.from.day15.i1[i,2]   # this scenario corresponds to PHE guidelines for 1st period
  p2<-prob.detected.from.day15.i2[i,2]   # this scenario corresponds to PHE guidelines for 2nd period
  p3<-prob.detected.from.day15.i3[i,2]   # this scenario corresponds to PHE guidelines for 3rd period
  p4<-prob.detected.from.day15.i4[i,2]   # this scenario corresponds to PHE guidelines for 3rd period
  
  p.oxf1<-prob.detected.from.day15.j1[i,2]   # this scenario corresponds to  testing every 7 days after admission - 1st period
  p.oxf2<-prob.detected.from.day15.j2[i,2]   # this scenario corresponds to  testing every 7 days after admission - 1st period
  p.oxf3<-prob.detected.from.day15.j3[i,2]   # this scenario corresponds to  testing every 7 days after admission - 1st period
  p.oxf4<-prob.detected.from.day15.j4[i,2]   # this scenario corresponds to  testing every 7 days after admission - 1st period
  
  # p<-prob.detected.from.day15.i[i,2]   # this scenario corresponds to PHE guidelines
  
  # p.oxf<-prob.detected.from.day15.j[i,2]   # this scenario corresponds to oxford testing...every 7 days after admission
  
  lastweekoffirstperiod<- 22  #  just a placeholder...will need to check this
  lastweekofsecondperiod<- 35  # just a placeholder...will need to check this
  lastweekofthirdperiod <- 48
  
  
  for(j in 1:length(nosocomial15day.dailytotals)){
    if(j<=lastweekoffirstperiod) {
      p<- p1
      p.oxf<-p.oxf1
    } else if (j>lastweekoffirstperiod & j<=lastweekofsecondperiod) {
      p<- p2
      p.oxf<-p.oxf2
    }  else if (j>lastweekofsecondperiod & j<=lastweekofthirdperiod) {
      p<- p3
      p.oxf<-p.oxf3
    } else {
      p<- p4
      p.oxf<-p.oxf4
    }
    
    x<-nosocomial15day.dailytotals[j]
    if(!is.na(x)){
      # then calculate posterior distribution of total cases (with a uniform prior on 1, maxcases) 
      post<-binom.ntrial.pos(x,p,rep(1,maxcases))
      post.oxf<-binom.ntrial.pos(x,p.oxf,rep(1,maxcases))
      
      samp.tot<-sample(1:maxcases,numdrawsfromposteriorpersim,replace=TRUE, prob=post)
      samp.tot.oxf<-sample(1:maxcases,numdrawsfromposteriorpersim,replace=TRUE, prob=post.oxf)
      col.range<-(i-1)*numdrawsfromposteriorpersim + (1:numdrawsfromposteriorpersim)
      estimated.total.nosocomial.m[col.range, j]<-samp.tot
      estimated.total.nosocomial.m.oxf[col.range, j]<-samp.tot.oxf
      
    }
  }
}  
# total estimated with nataionlly recommended sampling based on post 14 days onset cases
est.tot.sens1<-apply(estimated.total.nosocomial.m,MARGIN = 1, sum)
quantile(est.tot.sens1, c(0.05,.5,.95))
mean(est.tot.sens1)

est.tot.sens2<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 1, sum)
quantile(est.tot.sens2, c(0.05,.5,.95))
mean(est.tot.sens2)

# difference to previous estimates may be due to the fact that we are no longer
# basing LoS of patients who were never covid...so getting more covid 

# 2. Breaking down LoS by TRUST
# Using separate LoS dist for each trust
# In this analysis we ignore variation in LoS by time as previous analysis indicated that 
#  there was little variation  

# plan - create X  independent samples  of number of cases per trust  (x could be 100 or 1000)
# create X sums over trusts of these samples and calc intervals of the sums

#trusts.to.include<- XXX create vector of trusts
trusts.to.include<-sitreporgcodes147  #data from the 147 trusts in the sitrep data

trusts.to.include<-trustIDS  # the 134 trust sin the LoS data James sent

temp<-match("RQ8", trusts.to.include) 
trusts.to.include<-trusts.to.include[-temp]  #remove RQ8 as only NAs in day15 cases (and all LOS values are 0)


numtrusts<-length(trusts.to.include)

num.iterations<-100  # i.e. number of samples from incubation period, ocr sensitivity etc
numdrawsfromposteriorpersim<-100
numsamples<-num.iterations*numdrawsfromposteriorpersim # i.e number of samples per trust

# matrices - hold estimated number of nosocomial cases by iteration (row) and trust - col
# this holds the key output we need
estimated.total.nosocomial.m<-matrix(NA,nrow=numsamples, ncol<-numtrusts, dimnames=list(sample=1:numsamples,trust=trusts.to.include))
estimated.total.nosocomial.m.oxf<-matrix(NA,nrow=numsamples, ncol<-numtrusts, dimnames=list(sample=1:numsamples,trust=trusts.to.include))

estimated.15day.detection.prob<-matrix(NA,nrow=numsamples, ncol<-numtrusts, dimnames=list(sample=1:numsamples,trust=trusts.to.include))
estimated.15day.detection.prob.oxf<-matrix(NA,nrow=numsamples, ncol<-numtrusts, dimnames=list(sample=1:numsamples,trust=trusts.to.include))


maxcases<-20000 #  the maximum conceivable weekly number of nosocomial infections
maxlos<- 100
prob.i.matrix<-matrix(NA, nrow= length(trusts.to.include), ncol = maxlos)
prob.j.matrix<-matrix(NA, nrow= length(trusts.to.include), ncol = maxlos)

set.seed(42); 
include.uncertainty.in.pcr.sensitivity<-TRUE
mean_prob_sympt<-1-.179 # source: https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.10.2000180/
sd_mean_prob_sympt <- (0.202- 0.155)/(2*1.96) 
prob_sympt<-mean_prob_sympt
include.uncertainty.in.prob_sympt.<-TRUE
sampled.pcr.sensitivity <-rep(0, maxlos)

for(i in 1:num.iterations){
  
  # sample incubation period
  print(i)
  lnorm.param1<-rnorm(1, mean.param1, sd.param1)
  lnorm.param2<-rnorm(1, mean.param2, sd.param2)
  temp<-plnorm(incperiod, lnorm.param1, lnorm.param2)
  prob.inc.period.is.x.days<-c(temp[1],diff(temp))
  # Using data from Hellewell 2021 et al. 
  sampled.pcr.sensitivity[1:30] <-sens_post_daily[sample(1:4000,1),]
  if(include.uncertainty.in.prob_sympt.){
    prob_sympt<-rnorm(1,mean_prob_sympt, sd_mean_prob_sympt)
  } 
  
  # Loop over trusts here
  for(k in 1:length(trusts.to.include)){
    print(k)
    # these LoS calcs are deterministic and don't change as we loop over itations, so could
    # take these  5 lines out of this for loop and store  to speed up 
    LoS<-LoS_by_Trust[LoS_by_Trust[,1]==trusts.to.include[k],] 
    # make sure we have entries at least for day 1 to 100 in LoS
    missing <- !(1:100)  %in% LoS$LoS
    missingdays<-(1:100)[missing]
    newrows<-data.frame(Procode=rep(trusts.to.include[k],length(missingdays)), 
                        LoS=missingdays,
                        count=rep(0,length(missingdays)))
    
    LoS<- rbind(LoS, newrows)    
    LoS<-LoS[order(LoS$LoS),]
    
    agg_los_dist<-tapply(LoS$count, LoS$LoS, "sum")
    
    # exclude episodes with a los of 0 days and normalise
    agg_los_dist<-agg_los_dist[2:length(agg_los_dist)]
    agg_los_dist<-agg_los_dist/sum(agg_los_dist)
    
    #Truncate the los distribution at 100 days 
    agg_los_dist<-agg_los_dist[1:100]/sum(agg_los_dist[1:100])  
    
    # below here needs to be run once per iterations
    if(include.uncertainty.in.pcr.sensitivity){
      probs.i<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      probs.j<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      
    } else {
      # not currently implemented
    }
    cum.prob.i <- cumsum(probs.i)
    cum.prob.j <- cumsum(probs.j)
    
    prob.detected.from.day15.i  <- cum.prob.i[100] -  cum.prob.i[14]
    
    prob.detected.from.day15.j  <- cum.prob.j[100] -  cum.prob.j[14]
    
    
    p<-prob.detected.from.day15.i   # this scenario corresponds to PHE guidelines 
    p.oxf<-prob.detected.from.day15.j  # this scenario corresponds to  testing every 7 days after admission 
    
    #drop this line     for(j in 1:length(estimated.total.nosocomial)){
    
    nosocomial15day.def<-sitrep_time_series$n_inpatients_diagnosed_15_[sitrep_time_series$org_code==trusts.to.include[k]]
    
    days.since.31.12.19<-as.integer(sitrep_time_series$date[sitrep_time_series$org_code==trusts.to.include[k]]-as.Date("2019-12-31"))
    
    wk.no<-1+(days.since.31.12.19 %/% 7)
    
    nosocomial15day.dailytotals<-tapply(nosocomial15day.def,wk.no, FUN = "sum",na.rm=T)
    x<-sum(nosocomial15day.dailytotals)  #  need to set this to sum of day 15 cases in trusts.to.include[k]    nosocomial15day.dailytotals[j]
    if(!is.na(x) & p>0){
      # then calculate posterior distribution of total cases (with a uniform prior on 1, maxcases) 
      post<-binom.ntrial.pos(x,p,rep(1,maxcases))
      post.oxf<-binom.ntrial.pos(x,p.oxf,rep(1,maxcases))
      
      samp.tot<-sample(1:maxcases,numdrawsfromposteriorpersim,replace=TRUE, prob=post)
      samp.tot.oxf<-sample(1:maxcases,numdrawsfromposteriorpersim,replace=TRUE, prob=post.oxf)
      colrange<- ((i-1)*numdrawsfromposteriorpersim+1):(i*numdrawsfromposteriorpersim)
      estimated.total.nosocomial.m[colrange,k] <-samp.tot
      estimated.total.nosocomial.m.oxf[colrange,k] <-samp.tot.oxf
      estimated.15day.detection.prob[colrange,k]<-p
      estimated.15day.detection.prob.oxf[colrange,k]<-p.oxf
        
    }
  }
}
# remove RP6 as no patients there stay for 8 days or more 
rp6col<-match("RP6",colnames(estimated.total.nosocomial.m))

estimated.total.nosocomial.m[,rp6col] <-0 
estimated.total.nosocomial.m.oxf[,rp6col] <-0 


quantile(apply(estimated.total.nosocomial.m,1,sum),c(.05,.5,.95))
#5%      50%      95% 
#123339.9 139893.5 168257.2 

quantile(apply(estimated.total.nosocomial.m.oxf,1,sum),c(.05,.5,.95))
#5%       50%       95% 
#95852.95  99354.50 104263.05 



# 4. Plot graph of detction prob against reported cases per bed

detection.probs<- apply(estimated.15day.detection.prob,2,mean)

trusts<-names(detection.probs)

nosocasesbyrust<-c()
admittedposbytrust<-c()
for(i in trusts){
  temp1<-sum(sitrep_time_series$n_inpatients_diagnosed_15_[sitrep_time_series$org_code==i],na.rm=T)
  temp2<-sum(sitrep_time_series$n_inpatients_diagnosed_0_2[sitrep_time_series$org_code==i],na.rm=T)
  temp2<-temp2+ sum(sitrep_time_series$n_patients_admitted[sitrep_time_series$org_code==i],na.rm=T)
  nosocasesbyrust<-c(nosocasesbyrust,temp1)
  admittedposbytrust<-c(admittedposbytrust,temp2)
}
# adjust for number of beds

detprobs<-detection.probs[-84]
nosocasesbyrust<- nosocasesbyrust[-84]
admittedposbytrust<-admittedposbytrust[-84]

nosoadmittedratio<-nosocasesbyrust/admittedposbytrust

plot(detprobs,nosocasesbyrust )
plot(detprobs,nosoadmittedratio )

outliers<-67  # observation 67 is quite an extreme outlier
plot(detprobs[-outliers],nosocasesbyrust[-outliers])
plot(detprobs[-outliers],nosoadmittedratio[-outliers])

mod1<-lm(nosocasesbyrust ~ detprobs)
mod2<-lm(nosocasesbyrust ~ detprobs -1)
mod3<-lm(nosocasesbyrust ~ detprobs + admittedposbytrust)
mod4<-lm(nosoadmittedratio[-outliers]~ detprobs[-outliers] -1)

newx = seq(min(detprobs),max(detprobs),by = 0.01)
newx = seq(0,max(detprobs),by = 0.01)

#  plot mod1
conf_interval <- predict(mod1, newdata=data.frame(detprobs=newx), interval="confidence",
                         level = 0.95)

plot(detprobs, nosocasesbyrust, xlab="Detection probability", ylab="Number of nosocomial infections", main="",pch=20,col="grey")
abline(mod1, col="blue")
lines(newx, conf_interval[,2], col="grey", lty=2)
lines(newx, conf_interval[,3], col="grey", lty=2)

#  plot mod2 
conf_interval <- predict(mod2, newdata=data.frame(detprobs=newx), interval="confidence",
                         level = 0.95)
plot(detprobs, nosocasesbyrust, xlab="Detection probability", ylab="Number of nosocomial infections", main="")
abline(mod2, col="grey")
lines(newx, conf_interval[,2], col="grey", lty=2)
lines(newx, conf_interval[,3], col="grey", lty=2)

confint(mod2)


#  plot mod4
conf_interval <- predict(mod4, newdata=data.frame(detprobs=newx), interval="confidence",
                         level = 0.95)
plot(detprobs[-outliers], nosoadmittedratio[-outliers], xlab="Detection probability", ylab="Ratio of nosocomial to admitted", main="",xlim=c(0,0.25))
abline(mod4, col="red")
lines(newx, conf_interval[,2], col="grey", lty=2)
lines(newx, conf_interval[,3], col="grey", lty=2)

summary(mod4)

confint(mod4)



