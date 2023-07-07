#  Plot fig1 D, E and F  (called from fig1.r)
csize<-0.8
if(repeat_sampling){  #  repeat_sampling is a flag set in the calling file, fig1.r, that specifies whether to repeat sampling for this plot 
  
  source("function_defs.R")
  
  # Now calculate probability of detecting infections by x days post admission (for x=1:100) 
  # averaging over all lengths of stay 
  # When doing this remember that if 10% of patients have a los of 3 days and 10% have a los of 6 days then, providing infections are rare,
  # we would expect to have twice as many infections in patients with a los of 6 days (as twice as many patients days)
  # Also assume (based on analysis of Oxford data) that infections are equally likely to occur on any day of stay for a patient
  # with given length of stay (a reasonable assumption when rate of infection is low)
  
  
  
  num.iterations<-1000
  maxlos<- 100
  prob.a.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.b.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.c.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.d.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.e.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.f.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.g.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.h.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.i.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  prob.j.matrix<-matrix(NA, nrow= num.iterations, ncol = maxlos)
  
  
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
    
    if(include.uncertainty.in.pcr.sensitivity){
      probs.a<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.b<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 3)
      probs.c<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=rep(1,100), inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 3)
      probs.d<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.e<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,1,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.f<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,1,0,1,0),repeatPCRifsymptomaticandneg= 0)
      probs.g<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,1,1,1,1,1,1),repeatPCRifsymptomaticandneg= 0)
      probs.h<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 1)
      probs.i<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      probs.j<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=sampled.pcr.sensitivity, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      
      
    } else {
      probs.a<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.b<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.c<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=rep(1,100), inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 3)
      probs.d<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.e<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,1,0,0,0),repeatPCRifsymptomaticandneg= 0)
      probs.f<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,1,0,1,0),repeatPCRifsymptomaticandneg= 0)
      probs.g<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,1,1,1,1,1,1),repeatPCRifsymptomaticandneg= 0)
      probs.h<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(1,0,0,0,0,0,0),repeatPCRifsymptomaticandneg= 1)
      probs.i<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,1,0,0,1,0),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      probs.j<-CalcProbDetectedOnDay(los=agg_los_dist,  pcr_sens_vec=pcr_sens_vec, inc_vec=prob.inc.period.is.x.days,prob_sympt=prob_sympt, screening_days=c(0,0,0,0,0,0,1),repeatPCRifsymptomaticandneg= 0,wklyscreeningpattern=F)
      
    }
    
    prob.a.matrix[i,] <- cumsum(probs.a)
    prob.b.matrix[i,] <- cumsum(probs.b)
    prob.c.matrix[i,] <- cumsum(probs.c)
    prob.d.matrix[i,] <- cumsum(probs.d)
    prob.e.matrix[i,] <- cumsum(probs.e)
    prob.f.matrix[i,] <- cumsum(probs.f)
    prob.g.matrix[i,] <- cumsum(probs.g)
    prob.h.matrix[i,] <- cumsum(probs.h)
    prob.i.matrix[i,] <- cumsum(probs.i)
    prob.j.matrix[i,] <- cumsum(probs.j)
  }
  
  #  combine day to a single data frame for violin plots 
  prob.ever.detected.a<-data.frame(scenario=rep("a",100), probability.detected=prob.a.matrix[,100])
  prob.ever.detected.b<-data.frame(scenario=rep("b",100), probability.detected=prob.b.matrix[,100])
  prob.ever.detected.c<-data.frame(scenario=rep("c",100), probability.detected=prob.c.matrix[,100])
  prob.ever.detected.d<-data.frame(scenario=rep("d",100), probability.detected=prob.d.matrix[,100])
  prob.ever.detected.e<-data.frame(scenario=rep("e",100), probability.detected=prob.e.matrix[,100])
  prob.ever.detected.f<-data.frame(scenario=rep("f",100), probability.detected=prob.f.matrix[,100])
  prob.ever.detected.g<-data.frame(scenario=rep("g",100), probability.detected=prob.g.matrix[,100])
  prob.ever.detected.h<-data.frame(scenario=rep("h",100), probability.detected=prob.h.matrix[,100])
  prob.ever.detected.i<-data.frame(scenario=rep("i",100), probability.detected=prob.i.matrix[,100])
  prob.ever.detected.j<-data.frame(scenario=rep("j",100), probability.detected=prob.j.matrix[,100])
  
  prob.ever.detected<-rbind(prob.ever.detected.a,prob.ever.detected.b , prob.ever.detected.i, prob.ever.detected.j, prob.ever.detected.d,prob.ever.detected.e,prob.ever.detected.f,prob.ever.detected.g) 
  prob.ever.detected$order<-NA
  prob.ever.detected$order[prob.ever.detected$scenario=="a"] <-1
  prob.ever.detected$order[prob.ever.detected$scenario=="b"] <-2
  prob.ever.detected$order[prob.ever.detected$scenario=="i"] <-3
  prob.ever.detected$order[prob.ever.detected$scenario=="j"] <-4
  prob.ever.detected$order[prob.ever.detected$scenario=="d"] <-5
  prob.ever.detected$order[prob.ever.detected$scenario=="e"] <-6
  prob.ever.detected$order[prob.ever.detected$scenario=="f"] <-7
  prob.ever.detected$order[prob.ever.detected$scenario=="g"] <-8
  
} # end if(repeat_sampling)

# Violin plots
# First plot the overall probabilty of detecting hospital-acquired cases 
require(vioplot)
# vioplot(prob.ever.detected$probability.detected ~ prob.ever.detected$scenario, xlab="Screening scenario", ylab="Probability detected")

# Then plot probabilities of meeting ECDC definitions 

# First for definition of definite nosocomial infection, which is symptom onset after day 14

prob.detected.from.day15.a<-data.frame(scenario=rep("a",100), probability.detected= prob.a.matrix[,100] -  prob.a.matrix[,14])
prob.detected.from.day15.b<-data.frame(scenario=rep("b",100), probability.detected= prob.b.matrix[,100] -  prob.b.matrix[,14])
prob.detected.from.day15.c<-data.frame(scenario=rep("c",100), probability.detected= prob.c.matrix[,100] -  prob.c.matrix[,14])
prob.detected.from.day15.d<-data.frame(scenario=rep("d",100), probability.detected= prob.d.matrix[,100] -  prob.d.matrix[,14])
prob.detected.from.day15.e<-data.frame(scenario=rep("e",100), probability.detected= prob.e.matrix[,100] -  prob.e.matrix[,14])
prob.detected.from.day15.f<-data.frame(scenario=rep("f",100), probability.detected= prob.f.matrix[,100] -  prob.f.matrix[,14])
prob.detected.from.day15.g<-data.frame(scenario=rep("g",100), probability.detected= prob.g.matrix[,100] -  prob.g.matrix[,14])
prob.detected.from.day15.h<-data.frame(scenario=rep("h",100), probability.detected= prob.h.matrix[,100] -  prob.h.matrix[,14])
prob.detected.from.day15.i<-data.frame(scenario=rep("i",100), probability.detected= prob.i.matrix[,100] -  prob.i.matrix[,14])
prob.detected.from.day15.j<-data.frame(scenario=rep("j",100), probability.detected= prob.j.matrix[,100] -  prob.j.matrix[,14])

# Second for definition of probably nosocomial infection, which is symptom onset after day 7
prob.detected.from.day8.a<-data.frame(scenario=rep("a",100), probability.detected= prob.a.matrix[,100] -  prob.a.matrix[,7])
prob.detected.from.day8.b<-data.frame(scenario=rep("b",100), probability.detected= prob.b.matrix[,100] -  prob.b.matrix[,7])
prob.detected.from.day8.c<-data.frame(scenario=rep("c",100), probability.detected= prob.c.matrix[,100] -  prob.c.matrix[,7])
prob.detected.from.day8.d<-data.frame(scenario=rep("d",100), probability.detected= prob.d.matrix[,100] -  prob.d.matrix[,7])
prob.detected.from.day8.e<-data.frame(scenario=rep("e",100), probability.detected= prob.e.matrix[,100] -  prob.e.matrix[,7])
prob.detected.from.day8.f<-data.frame(scenario=rep("f",100), probability.detected= prob.f.matrix[,100] -  prob.f.matrix[,7])
prob.detected.from.day8.g<-data.frame(scenario=rep("g",100), probability.detected= prob.g.matrix[,100] -  prob.g.matrix[,7])
prob.detected.from.day8.h<-data.frame(scenario=rep("h",100), probability.detected= prob.h.matrix[,100] -  prob.h.matrix[,7])
prob.detected.from.day8.i<-data.frame(scenario=rep("i",100), probability.detected= prob.i.matrix[,100] -  prob.i.matrix[,7])
prob.detected.from.day8.j<-data.frame(scenario=rep("j",100), probability.detected= prob.j.matrix[,100] -  prob.j.matrix[,7])


# # Third  symptom onset after day 2
# prob.detected.from.day3.a<-data.frame(scenario=rep("a",100), probability.detected= prob.a.matrix[,100] -  prob.a.matrix[,2])
# prob.detected.from.day3.b<-data.frame(scenario=rep("b",100), probability.detected= prob.b.matrix[,100] -  prob.b.matrix[,2])
# prob.detected.from.day3.c<-data.frame(scenario=rep("c",100), probability.detected= prob.c.matrix[,100] -  prob.c.matrix[,2])
# prob.detected.from.day3.d<-data.frame(scenario=rep("d",100), probability.detected= prob.d.matrix[,100] -  prob.d.matrix[,2])
# prob.detected.from.day3.e<-data.frame(scenario=rep("e",100), probability.detected= prob.e.matrix[,100] -  prob.e.matrix[,2])
# prob.detected.from.day3.f<-data.frame(scenario=rep("f",100), probability.detected= prob.f.matrix[,100] -  prob.f.matrix[,2])
# prob.detected.from.day3.g<-data.frame(scenario=rep("g",100), probability.detected= prob.g.matrix[,100] -  prob.g.matrix[,2])


prob.detected.from.day15<-rbind(prob.detected.from.day15.a, prob.detected.from.day15.b,prob.detected.from.day15.i, prob.detected.from.day15.j, prob.detected.from.day15.d,prob.detected.from.day15.e,prob.detected.from.day15.f,prob.detected.from.day15.g)

prob.detected.from.day8<-rbind(prob.detected.from.day8.a, prob.detected.from.day8.b, prob.detected.from.day8.i, prob.detected.from.day8.j,prob.detected.from.day8.d,prob.detected.from.day8.e,prob.detected.from.day8.f,prob.detected.from.day8.g)
# order
prob.detected.from.day15$order<-NA
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="a"] <-1
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="b"] <-2
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="i"] <-3
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="j"] <-4
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="d"] <-5
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="e"] <-6
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="f"] <-7
prob.detected.from.day15$order[prob.detected.from.day15$scenario=="g"] <-8

prob.detected.from.day8$order<-NA
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="a"] <-1
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="b"] <-2
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="i"] <-3
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="j"] <-4
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="d"] <-5
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="e"] <-6
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="f"] <-7
prob.detected.from.day8$order[prob.detected.from.day8$scenario=="g"] <-8

# prob ever detected if no screening (PCR testing symptomatic only)

quantile(prob.ever.detected$probability.detected[prob.ever.detected$order==1],c(0.05,.5,.95))

# prob ever detected if screening on day 3 and 6 as nationally recommended
quantile(prob.ever.detected$probability.detected[prob.ever.detected$order==3],c(0.05,.5,.95))
# prob ever detected if screening at 7 day intervals
quantile(prob.ever.detected$probability.detected[prob.ever.detected$order==4],c(0.05,.5,.95))


# prob  detected and classed as definite hai if no screening (PCR testing symptomatic only)
quantile(prob.detected.from.day15$probability.detected[prob.ever.detected$order==1],c(0.05,.5,.95))

# prob  detected and classed as definite hai if  screening on day 3 and 6

quantile(prob.detected.from.day15$probability.detected[prob.ever.detected$order==3],c(0.05,.5,.95))
# prob  detected and classed as definite hai if  screening at 7 day intervals
quantile(prob.detected.from.day15$probability.detected[prob.ever.detected$order==4],c(0.05,.5,.95))

# prob.detected.from.day3<-rbind(prob.detected.from.day3.a, prob.detected.from.day3.b,prob.detected.from.day3.c,prob.detected.from.day3.d,prob.detected.from.day3.e,prob.detected.from.day3.f,prob.detected.from.day3.g)
# vioplot(prob.detected.from.day3$probability.detected ~ prob.detected.from.day3$scenario, xlab="Screening scenario", ylab="Probability detected")

# Plots (where y-axis holds different scenarios)
#a) No screening, no retesting if screen negative

#b) No screening, estimated pcr sensitivities
#c) No screening, perfect pcr sensitivities

#d) Screening 1 day per week estimated pcr sensitivities
#e) Screening 2 days per week estimated pcr sensitivities
#f) Screening 3 days per week, estimated pcr sensitivities
#g) Screening 7 days per week, estimated pcr sensitivities

# par(mfrow=c(1,1), mar=c(4,20,2,2), oma=c(1,1,1,1))
violincolours<-rep("grey", 8)
violincolours[3]<-"darkseagreen"
violincolours[4]<-"blue3"
par(mar=c(4,20,3,0))
scenario.names<-c("Symptomatic testing","Symptomatic testing, retesting if negative","Symptomatic testing + day 3 & 6 screening","Symptomatic testing + day 7,14,21,... screening",  
                  "Symptomatic testing + weekly screening" ,"Symptomatic testing + 2 x weekly screening","Symptomatic testing + 3 x weekly screening", "Symptomatic testing + daily screening")

#pdf("fig prob detected.pdf", height=4, width=8, pointsize=10)
vioplot(prob.ever.detected$probability.detected ~ prob.ever.detected$order, ylab="", xlab="Probability detected",horizontal=T,names=scenario.names,las=2, ylim=c(0,1),main="",col=violincolours)
mtext(side=3,"Probability hospital-acquired infection detected",adj = 1,cex=csize, line=1)
grid(ny=NA)

# Note that as we move to more frequent screening uncertainty is reduced as most infections are detected by screening
# asymptomatic patients so incubation period uncertainty becomes irrelevant 
# also repeat testing means uncertainty in sensitivity becomes less relevant as well
#vioplot(prob.detected.from.day8$probability.detected ~ prob.detected.from.day8$scenario, ylab="", xlab="Probability detected",horizontal=T,names=scenario.names,las=2, ylim=c(0,1), main="Probability ha-infection post 7 day onset")
#ppdf("fig prob detected from day 8.pdf", height=4, width=8, pointsize=10)

#vioplot(prob.detected.from.day15$probability.detected ~ prob.detected.from.day15$scenario, ylab="", xlab="Probability detected",horizontal=T,names=scenario.names,las=2, ylim=c(0,0.2),main="")
nonames<-rep("",8)
par(mar=c(4,2,3,1))
vioplot(prob.detected.from.day15$probability.detected ~ prob.detected.from.day15$order, ylab="", xlab="Probability detected",horizontal=T,names=nonames,las=2, ylim=c(0,0.4), main="",col=violincolours)
mtext(side=3,"Probability hospital-acquired infection",adj = 1,cex=csize, line=2)
mtext(side=3,"identified with post day 14 onset",adj = 1,cex=csize, line=1)
grid(ny=NA)
par(mar=c(4,2,3,1))

vioplot(prob.detected.from.day8$probability.detected ~ prob.detected.from.day8$order, ylab="", xlab="Probability detected",horizontal=T,names=nonames,las=2, ylim=c(0,0.4), main="",col=violincolours)
mtext(side=3,"Probability hospital-acquired infection",adj = 1,cex=csize, line=2)
mtext(side=3,"identified with post day 7 onset",adj = 1,cex=csize, line=1) 
grid(ny=NA)

