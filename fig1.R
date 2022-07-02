#  Figure 1

# layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,4,4,5,5,5,6,6,6,rep(7,6),rep(8,6)), 3,12,byrow=T) )
layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,4,4,5,5,5,6,6,6,rep(7,12)), 3,12,byrow=T) )

# Panel A: incubation period - plots probability of symptom onset on a given day since infection event
# conditional on a patient being symptomatic

  # Baseline estimates are from Lauer 2020 which gives parameters and associated 95% CIs for common distributions
# Baseline assumption is log-normal with parameters 1.621 (1.504–1.755) 0.418 (0.271–0.542) 

par(mar=c(5,4,1,1),cex=.8)
# Baseline Log-normal parameters 1.621 (1.504–1.755) 0.418 (0.271–0.542) 

mean.param1<-1.621
mean.param2<-0.418
maxday<-30
incperiod<-1:maxday
sd.param1<-(1.755 - 1.504)/(2*1.96)
sd.param2<-(0.542-0.271)/(2*1.96)

set.seed(42)
num.iterations<-1000
incperiod.samples<-matrix(NA, nrow=num.iterations, ncol=length(incperiod))
for(i in 1:num.iterations){
  # sample incubation period
  lnorm.param1<-rnorm(1, mean.param1, sd.param1)
  lnorm.param2<-rnorm(1, mean.param2, sd.param2)
  temp<-plnorm(incperiod, lnorm.param1, lnorm.param2)
  prob.inc.period.is.x.days<-c(temp[1],diff(temp))
  incperiod.samples[i,] <- prob.inc.period.is.x.days
}
inc.period.q<-apply(incperiod.samples,MARGIN = 2,FUN = quantile,c(0.025,.5,.975) )
plot(1:maxday,inc.period.q[2,] , xlab="Day since infection" ,ylab="Probability symptom onset",type='l', ylim=c(0,0.3),bty='n')
polygon(c(1:maxday, maxday:1),c(inc.period.q[1,], rev( inc.period.q[3,])),col="grey" , border=NA)
lines(1:maxday,inc.period.q[2,], )

# Panel B: PCR sensitivity as a function of days since infection

sens_post<-readRDS("Hellewell_fig3bdata.RDS")

# Hellewell_fig3bdata.RDS holds posterior of PCR sensitivity as function of time since infection
# There are 10 time points per day and 30 days in total, so including time 0 there are 301 columns
# This is plotted in Fig 3B of the paper https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-021-01982-x
# Code to produce this is here https://github.com/cmmid/pcr-profile 
# and "Hellewell_fig3bdata.RDS" holds the object res$p obtained by running this code (posteriors from Stan analysis)
sens_post<-sens_post[,2:301] # cut out day 0
day<-rep(1:30, each=10)
daymean<-function(x){ tapply(x, day, mean)}
sens_post_daily<-t(apply(sens_post, 1, daymean))
sensitivity.q<-apply(sens_post_daily,MARGIN = 2,FUN = quantile,c(0.025,.5,.975) )
plot(1:maxday,sensitivity.q[2,] , xlab="Day since infection" ,ylab="PCR sensitivity",type='l', ylim=c(0,1.0),bty='n')
polygon(c(1:maxday, maxday:1),c(sensitivity.q[1,], rev( sensitivity.q[3,])),col="grey" , border=NA)
lines(1:maxday,sensitivity.q[2,], )

# Panel C: Length of stay distribution 
library(readxl)
#  LoS data

# conservatively use los data for those who were not positive on admission, regardless of whether they became positive
los_data <- readRDS("LoS_by_Trust.rds")

# los_data holds frequency of lengths of stays of 0,1,2,3,....days for each provider with >10,000 completed episodes over the period
# by trust and (provider) code and week. 
# Aggregate this across weeks and trusts to calculate probabilities of different los 
# agg_los_dist<-tapply(los_data$n, los_data$los.days, "sum")
agg_los_dist<-tapply(los_data$count, los_data$LoS, "sum")
# exclude episodes with a los of 0 days and normalise
agg_los_dist<-agg_los_dist[2:length(agg_los_dist)]

agg_los_dist<-agg_los_dist/sum(agg_los_dist)
# plot(as.numeric(names(agg_los_dist)), log(agg_los_dist))
# over 99.9% of episodes are less than 100 days  sum(agg_los_dist[1:100]) is 0.999374
# so we don't lose much by truncating the los distribution at 100 days & ignoring those with los>100 days
agg_los_dist<-agg_los_dist[1:100]/sum(agg_los_dist[1:100])
barlabels<-rep("",30)
barlabels[c(1,10,20)] =as.character(c(1,10,20))
barplot(agg_los_dist[1:30],names.arg = barlabels, ylab= "Frequency",xlab="Days",xaxt='none')
abline(v=14,col="grey")  # patients must stay at least this long to be at risk of a "definite" nosocomial infection
abline(v=7,lty=2,col="grey")  # patients must stay at least this long to be at risk of a "probable" nosocomial infection
axis(side=1,at=c(1,5,10,15,20,25,30))

# Panel D: probability of detecting infection as a function of screening policy
# Panel E: probability of detecting infection as a function of screening policy for 14 day cutoff (ECDC definition of definite nosocomial)
# Panel E: probability of detecting infection as a function of screening policy for 14 day cutoff (ECDC definition of definite nosocomial)


# Call another file to do this
repeat_sampling<-TRUE  # set to TRUE if want to repeat sampling for these panels 

source("fig1panelsDtoF.R")

# Now add time series plots of numbers

# read in  sitrep data (which contains only Acute Trusts)
sitrep_time_series <- readRDS("sitreps_eng_expanded.rds")
# exclude children's hospitals (Great Ormond Street Hospital for Children NHS Foundation Trust., Alder Hey Children's NHS Foundation Trust. Sheffield Children's NHS Foundation Trust)
include<-!(sitrep_time_series$org_code %in% c("RP4" , "RBS", "RCU"))
sitrep_time_series<-sitrep_time_series[include,]
sitreporgcodes147<-unique(sitrep_time_series$org_code)
nosocomial15day.def<-sitrep_time_series$n_inpatients_diagnosed_15_
nosocomial8to14day.def<-sitrep_time_series$n_inpatients_diagnosed_8_14
nosocomial3to7day.def<-sitrep_time_series$n_inpatients_diagnosed_3_7
nosocomial0to2day.def<-sitrep_time_series$n_inpatients_diagnosed_0_2

days.since.31.12.19<-as.integer(sitrep_time_series$date-as.Date("2019-12-31"))

wk.no<-1+(days.since.31.12.19 %/% 7)

# now sum cases by week
# first create a week variable 
nosocomial15day.dailytotals<-tapply(nosocomial15day.def,wk.no, FUN = "sum",na.rm=T)
nosocomial8to14day.dailytotals<-tapply(nosocomial8to14day.def,wk.no, FUN = "sum",na.rm=T)
nosocomial3to7day.dailytotals<-tapply(nosocomial3to7day.def,wk.no, FUN = "sum",na.rm=T)
nosocomial0to2day.dailytotals<-tapply(nosocomial0to2day.def,wk.no, FUN = "sum",na.rm=T)

# We have two sources of uncertainty
# i)  uncertainty in the probability of nosocomial infections meeting the 14 day detection cutoff
# ii) binomial uncertainty in the number trials given known probability and number of outcomes

estimated.total.nosocomial.scaling<-mean(1/prob.detected.from.day15.d[,2])
estimated.total.nosocomial<-estimated.total.nosocomial.scaling*nosocomial15day.dailytotals
sum(estimated.total.nosocomial,na.rm=T) # estimated total nosocomial

newsims<-TRUE

if(newsims){
  numsims<-100 # i.e number of sample probabilities of detection (which depend on uncertainty in inc period, pcr sensitivity, proportion symptomatic)
  numdawsfromposteriorpersim<-100
  numsamples<-numsims*numdawsfromposteriorpersim
  maxcases<-20000 #  the maximum conceivable weekly number of nosocomial infections
  estimated.total.nosocomial.m<-matrix(NA,nrow=numsamples, ncol<-length(estimated.total.nosocomial), dimnames=list(sample=1:numsamples,week=names(estimated.total.nosocomial)))
  estimated.total.nosocomial.m.oxf<-matrix(NA,nrow=numsamples, ncol<-length(estimated.total.nosocomial), dimnames=list(sample=1:numsamples,week=names(estimated.total.nosocomial)))
  for(i in 1:numsims){
    print(i)
    p<-prob.detected.from.day15.i[i,2]   # this scenario corresponds to PHE guidelines
    p.oxf<-prob.detected.from.day15.j[i,2]   # this scenario corresponds to oxford testing...every 7 days after admission
    
    for(j in 1:length(estimated.total.nosocomial)){
      x<-nosocomial15day.dailytotals[j]
      if(!is.na(x)){
        # then calculate posterior distribution of total cases (with a uniform prior on 1, maxcases) 
        post<-binom.ntrial.pos(x,p,rep(1,maxcases))
        post.oxf<-binom.ntrial.pos(x,p.oxf,rep(1,maxcases))
        
        samp.tot<-sample(1:maxcases,numdawsfromposteriorpersim,replace=TRUE, prob=post)
        samp.tot.oxf<-sample(1:maxcases,numdawsfromposteriorpersim,replace=TRUE, prob=post.oxf)
        col.range<-(i-1)*numdawsfromposteriorpersim + (1:numdawsfromposteriorpersim)
        estimated.total.nosocomial.m[col.range, j]<-samp.tot
        estimated.total.nosocomial.m.oxf[col.range, j]<-samp.tot.oxf
        
      }
    }
  }  
  # total estimated with nationlly recommended sampling based on post 14 days onset cases
  est.tot.1<-apply(estimated.total.nosocomial.m,MARGIN = 1, sum)
  quantile(est.tot.1, c(0.05,.5,.95))
  mean(est.tot.1)
  
  # total estimated with oxford  sampling based on post 14 days onset cases
  est.tot.2<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 1, sum)
  quantile(est.tot.2, c(0.05,.5,.95))
  mean(est.tot.2)
  
  # so now each column of estimated.total.nosocomial.m reprsents numsims*numdawsfromposteriorpersim draws from the posterior
  
  estimated.total.nosocomial.mean<-apply(estimated.total.nosocomial.m,MARGIN = 2,FUN = mean, na.rm=TRUE)
  estimated.total.nosocomial.intervals<-apply(estimated.total.nosocomial.m,MARGIN = 2,FUN = quantile, c(.05,0.5,0.95), na.rm=TRUE) # 90% credible intervals
  estimated.total.nosocomial.mean.oxf<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 2,FUN = mean, na.rm=TRUE)
  estimated.total.nosocomial.intervals.oxf<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 2,FUN = quantile, c(.05,0.5,0.95), na.rm=TRUE) # 90% credible intervals
  
}



#  Plot estimated numbers based on those with onset after day 14 
par(mar=c(4,20,1,6))
wks<-24:60
sel<-match(as.character(wks), names(estimated.total.nosocomial.mean))
#plot(wks,estimated.total.nosocomial.mean[sel],col="darkseagreen",type="l", xlab="Week number", ylab="Weekly infections",bty='n',ylim=c(0,18000),main="Estimated hospital-acquired infections using numbers detected after day 14")
plot(wks,estimated.total.nosocomial.mean[sel],col="darkseagreen",type="l", xlab="Week number", ylab="Weekly infections",bty='n',ylim=c(0,18000))

polygon(c(wks, rev(wks)),c(estimated.total.nosocomial.intervals.oxf[1,sel], rev(estimated.total.nosocomial.intervals.oxf[3,sel])),col="blue" , border=NA)

polygon(c(wks, rev(wks)),c(estimated.total.nosocomial.intervals[1,sel], rev(estimated.total.nosocomial.intervals[3,sel])),col="seagreen" , border=NA)

lines(wks,estimated.total.nosocomial.mean[sel], lty=1,col="darkseagreen")

lines(wks,estimated.total.nosocomial.mean.oxf[sel], lty=1,col="lightblue")

# Add a line for "definite" nosocomial data (the data we are basing inferences on)
lines(wks,nosocomial15day.dailytotals[sel],col="red3",lwd=2)

nsamples<-dim(estimated.total.nosocomial.m)[1]
prob.detected.from.day8.i <-prob.detected.from.day8.i[ ,2]
prob.detected.from.day8.j <-prob.detected.from.day8.j[ ,2]
p<-prob.detected.from.day8.i   # this scenario corresponds to PHE testing...every 7 days after admission

p.oxf<-prob.detected.from.day8.j   # this scenario corresponds to oxford testing...every 7 days after admission


for(i in 1:length(p)){
  print(i)
  for(j in 1:length(estimated.total.nosocomial)){
    x<-nosocomial8to14day.dailytotals[j] + nosocomial15day.dailytotals[j]
    if(!is.na(x)){
      # then calculate posterior distribution of total cases (with a uniform prior on 1, maxcases) 
      post<-binom.ntrial.pos(x,p,rep(1,maxcases))
      post.oxf<-binom.ntrial.pos(x,p.oxf,rep(1,maxcases))
      
      samp.tot<-sample(1:maxcases,numdawsfromposteriorpersim,replace=TRUE, prob=post)
      samp.tot.oxf<-sample(1:maxcases,numdawsfromposteriorpersim,replace=TRUE, prob=post.oxf)
      col.range<-(i-1)*numdawsfromposteriorpersim + (1:numdawsfromposteriorpersim)
      estimated.total.nosocomial.m[col.range, j]<-samp.tot
      estimated.total.nosocomial.m.oxf[col.range, j]<-samp.tot.oxf
    }
  }
}
# total estimated with nationally recommended sampling based on day8-14  onset cases
est.tot.1b<-apply(estimated.total.nosocomial.m,MARGIN = 1, sum)
quantile(est.tot.1b, c(0.05,.5,.95))
mean(est.tot.1b)

# total estimated with oxford  sampling based on day8-14  onset cases
est.tot.2b<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 1, sum)
quantile(est.tot.2b, c(0.05,.5,.95))
mean(est.tot.2b)

estimated.total.nosocomial.mean<-apply(estimated.total.nosocomial.m,MARGIN = 2,FUN = mean, na.rm=TRUE)
estimated.total.nosocomial.intervals<-apply(estimated.total.nosocomial.m,MARGIN = 2,FUN = quantile, c(.05,0.5,0.95), na.rm=TRUE) # 90% credible intervals
estimated.total.nosocomial.mean.oxf<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 2,FUN = mean, na.rm=TRUE)
estimated.total.nosocomial.intervals.oxf<-apply(estimated.total.nosocomial.m.oxf,MARGIN = 2,FUN = quantile, c(.05,0.5,0.95), na.rm=TRUE) # 90% credible intervals

# 
# #  Plot estimated numbers based on those with onset from day 8 onwards - for figure S1
# 
# par(mar=c(4,4,5,1))
# wks<-unique(wk.no)
# # 
# plot(wks,estimated.total.nosocomial.mean,col="darkseagreen",type="l", xlab="week number", ylab="Weekly infections",bty='n',ylim=c(0,18000),main="Estimated hospital-acquired infections using numbers detected on days 8-14")
# polygon(c(wks, rev(wks)),c(estimated.total.nosocomial.intervals.oxf[1,], rev(estimated.total.nosocomial.intervals.oxf[3,])),col="blue" , border=NA)
# # 
# polygon(c(wks, rev(wks)),c(estimated.total.nosocomial.intervals[1,], rev(estimated.total.nosocomial.intervals[3,])),col="seagreen" , border=NA)
# # 
# lines(wks,estimated.total.nosocomial.mean, lty=2,col="darkseagreen")
# # 
# lines(wks,estimated.total.nosocomial.mean.oxf, lty=2,col="lightblue")
# # 
# # # Add a line for "definite" nosocomial data (the data we are basing inferences on)
# lines(wks,nosocomial8to14day.dailytotals,col="red3",lwd=2,lty=2)
# lines(wks,nosocomial15day.dailytotals,col="red",lwd=1,lty=1)
# # 
# # # Add a dashed line "probable nosocomial infection 
# # # lines(wks,nosocomial8to14day.dailytotals,col="red3",lty=2)
# # # Add a dotted line "indeterminate"
# # #lines(wks,nosocomial3to7day.dailytotals,col="red3",lty=3)
# # 
# # 
# # # polygon(c(wks, rev(wks)),c(estimated.day8to14.nosocomial.intervals[1,], rev(estimated.day8to14.nosocomial.intervals[3,])),col="pink" , border=NA)
# # 
# legend("topleft", legend=c("Estimated  hospital-acquired infections (no weekly screening)","Estimated  hospital-acquired infections (weekly screening)", "Detected infections with onset days 8-14","Detected infections with onset after day 14"), lty=c(1,1,2,1),col=c("seagreen","blue","red3","red"),lwd=c(2,2,2,1),bty='n')
# # 
# 
