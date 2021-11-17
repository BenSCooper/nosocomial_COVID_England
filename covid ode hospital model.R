# Covid ODE hospital model
library(deSolve)

HospCovid <- function(t, x, parms) { 

  
  with(as.list(c(parms, x)), {
    # beta_HH is transmission parameter for transmission from infectious hospital patient to susceptible hospital infection
    # mu is hospital death and discharge rate  for hospitalised (not for COVID). In general will be time varying.
    # muprimed is hospital death and discharge rate  for hospitalised COVID patients (i.e. those hospitalised because of COVID)
    #  alpha is admission rate to hospital from community for those not known to be covid 
    # alphaprimed is admission rate to hospital from communith for those with seveer covid (i.e .in the compartment Iprimed_C)
    # gamma1 is progression rate from E1 to E2, 
    # gamma2 is progression rate from E2 to I1,
    # rho1 is progression rate from I1 to I2,
    # rho2 is progression rate from I2 to R or Iprimed (seriously ill),
    # rho3 is progression rate from Iprimed to R
    # pi_H is the proportion of hospitalised infected patients who progress to severe infection, Iprimed (where definition of severe is that hospitalisation would be recommended)
    # pi_C is the proportion of infected patients in the community who progress to severe infection, Iprimed
    
      
    N_H   <- S_H   + E1_H   +  E2_H   +  I1_H   +  I2_H   + Iprimed_H + R_H       # hospital population
    N_HCW <- S_HCW + E1_HCW +  E2_HCW +  I1_HCW +  I2_HCW + Iprimed_HCW + R_HCW   # HCW population
    N_C   <- S_C   + E1_C   +  E2_C   +  I1_C   +  I2_C   + Iprimed_C + R_C       # Community populations
    
    # in the above I_1 and I_2 are for representing erlang distributed infectious periods  for those who are infectious
    # whie Iprimed corresponds to severe costs who will be isolated or hospitalised and can be assumed not to be highly infectious
    # print(c(N_H,N_HCW,N_C))
    
    #  Block for defining time varying parameters (which will include beta in the community and hospital discharge) 
    
    # beta<-time.varying.parameter(beta.max,beta.min,phi,t)
    total.hosp.discharge.rate<-mu*(S_H+ E1_H + E2_H + I1_H +I2_H + R_H) + muprimed*Iprimed_H
    #total.hosp.admission.rate<-alpha*(S_C+ E1_C + E2_C + I1_C +I2_C+ R_C) + alphaprimed*Iprimed_C
    # now choose alpha so admission rate matches dishcarge rate
    # now specify admission rate for severe cases to be limited by overall discharge rate 
    
    limited.alphaprimed<-min(alphaprimed, total.hosp.discharge.rate/Iprimed_C )
    
    # and admission rate for non-covid adjusted to fill capacity, but could be zero 
    alpha<-(total.hosp.discharge.rate -limited.alphaprimed*Iprimed_C)/(S_C+ E1_C + E2_C + I1_C +I2_C+ R_C) 
     
    # scale commmunity transmission to account for interventions (affects beta_CC and beta_CHCW)
    phi<-1
    if(t> changepoint1 & t<= changepoint2) phi<-phi1
    if(t> changepoint2 & t<= changepoint3) phi<-phi2
    if(t> changepoint3 & t<= changepoint4) phi<-phi3
    if(t> changepoint4 ) phi<-phi4
    
    
    
     
    # Derivatives 
    # a) For hospitalised population 
    dS_H <- -beta_HH*S_H*(I1_H + I2_H)/N_H   - betaprimed_HH*S_H*Iprimed_H/N_H  -beta_HCWH*S_H*(I1_HCW)/N_H  - mu * S_H + alpha * S_C 
    dE1_H <-  beta_HH*S_H*(I1_H + I2_H)/N_H  +  betaprimed_HH*S_H*Iprimed_H/N_H  + beta_HCWH*S_H*(I1_HCW)/N_H  - gamma1*E1_H -mu*E1_H +alpha*E1_C
    dE2_H <- gamma1*E1_H - gamma2*E2_H -mu*E2_H +alpha*E2_C
    dI1_H <- gamma2*E2_H - rho1*I1_H  -mu*I1_H +alpha*I1_C
    dI2_H <- rho1*I1_H - rho2*I2_H   -mu*I2_H +alpha*I2_C
    dIprimed_H <- pi_H * rho2*I2_H -rho3*Iprimed_H -muprimed*Iprimed_H +limited.alphaprimed*Iprimed_C
    dR_H  <- (1- pi_H) * rho2*I2_H + rho3*Iprimed_H   -mu*R_H +alpha*R_C
    
    # b) For community population    
    dS_C <- -phi*beta_CC*S_C*(I1_C + I2_C)/N_C   - phi*betaprimed_CC*S_C*Iprimed_C/N_C  -beta_HCWC*S_C*(I1_HCW + I2_HCW)/N_C  + mu * S_H - alpha * S_C 
    dE1_C <-  phi*beta_CC*S_C*(I1_C + I2_C)/N_C  +  phi*betaprimed_CC*S_C*Iprimed_C/N_C  + beta_HCWC*S_C*(I1_HCW + I2_HCW)/N_C  - gamma1*E1_C + mu*E1_H  - alpha*E1_C
    dE2_C <- gamma1*E1_C - gamma2*E2_C + mu*E2_H - alpha*E2_C
    dI1_C <- gamma2*E2_C - rho1*I1_C   + mu*I1_H  - alpha*I1_C
    dI2_C <- rho1*I1_C - rho2*I2_C   +mu*I2_H - alpha*I2_C
    dIprimed_C <- pi_C * rho2*I2_C -rho3*Iprimed_C + muprimed*Iprimed_H - limited.alphaprimed*Iprimed_C
    dR_C  <- (1 - pi_C) * rho2*I2_C  + rho3*Iprimed_C   + mu*R_H - alpha*R_C 
    
    # c) For HCWs  (for simplicity we neglect hospitalisation of HCW for now)
    dS_HCW  <- -beta_HHCW*S_HCW*(I1_H + I2_H)/N_HCW   - betaprimed_HHCW*S_HCW*Iprimed_H/N_HCW  -beta_HCWHCW*S_HCW*(I1_HCW)/N_HCW - phi*beta_CHCW*S_HCW*(I1_C + I2_C)/N_HCW
    dE1_HCW <- beta_HHCW*S_HCW*(I1_H + I2_H)/N_HCW  +  betaprimed_HHCW*S_HCW*Iprimed_H/N_HCW  + beta_HCWHCW*S_HCW*(I1_HCW)/N_HCW  + phi*beta_CHCW*S_HCW*(I1_C + I2_C)/N_HCW- gamma1*E1_HCW 
    dE2_HCW <- gamma1*E1_HCW - gamma2*E2_HCW
    dI1_HCW <- gamma2*E2_HCW - rho1*I1_HCW 
    dI2_HCW <- rho1*I1_HCW - rho2*I2_HCW  
    dIprimed_HCW <- pi_HCW * rho2*I2_HCW -rho3*Iprimed_HCW 
    dR_HCW  <- (1- pi_HCW) * rho2*I2_HCW + rho3*Iprimed_HCW  
    
    dCumNosoInfections <- beta_HH*S_H*(I1_H + I2_H)/N_H  +  betaprimed_HH*S_H*Iprimed_H/N_H  + beta_HCWH*S_H*(I1_HCW + I2_HCW)/N_H 
    dCumDetectedNosoInfections <- rho1*I1_H   # note that this includes community infections with hospital onset 
    dCumCOVIDAdmissions <- limited.alphaprimed*Iprimed_C
    
    
    # Return values 
    list(c(dS_H, dE1_H, dE2_H, dI1_H, dI2_H, dIprimed_H, dR_H,dS_HCW, dE1_HCW, dE2_HCW, dI1_HCW, dI2_HCW, dIprimed_HCW, dR_HCW, dS_C, dE1_C, dE2_C, dI1_C, dI2_C, dIprimed_C, dR_C, 
           dCumNosoInfections,dCumDetectedNosoInfections,dCumCOVIDAdmissions))
    
  })  # end with
} # end HospCovid



# in general components of next generation matrix will be time varying
#R_HH<-   0.5   # R_HH= beta_HH/mean.infectious.period etc
R_HH<-   1.0
R_HCWH<- 0.2
#R_HCWH<- 0
R_HHCW<- 0.5
#R_HHCW<-0
R_CC<-   3.0
R_HCWC<-  0.2 
R_HCWHCW<-0.5
R_CHCW <-0.1


# approx next gen matrix (only approximate as we are not accounting for readmissions etc)
# K is for full community transmission
K<-matrix(0, nrow=3, ncol=3)
K[1,1]<-R_HH
K[1,2]<-R_HCWH
K[1,3]<-0  
K[2,2]<-R_HCWHCW
K[2,1]<-R_HHCW
K[2,3]<-R_CHCW
K[3,3]<-R_CC
K[3,1]<- 0
K[3,2]<- R_HCWC

eigen(K)
# K1 is for 0 community transmission (for baseline parameters still above 1)

K1<-K
K1[2,3]<-0
K1[3,3]<-0
eigen(K1)


# K2 is for 0 community transmission and 25% reduced hospital transmission
K2<-K1
K2[1,1]<-K2[1,1]*0.75
K2[1,2]<-K2[1,2]*0.75
K2[2,2]<-K2[2,2]*0.75
K2[2,1]<-K2[2,1]*0.75
eigen(K2)

# K3 is for 0 community transmission and 50% reduced hospital transmission
K3<-K1
K3[1,1]<-K3[1,1]*0.5
K3[1,2]<-K3[1,2]*0.5
K3[2,2]<-K3[2,2]*0.5
K3[2,1]<-K3[2,1]*0.5
eigen(K3)




gamma1<-0.5
gamma2<-0.5
mu<-0.2
rho1=0.3
rho2 =0.3 
rho3=0.1

mean.infectious.period<-6.66
mean.infectious.period.hcw<-1/rho1 # assumed mean infectious period of a hcw in a hospital before they self-isolate
mean.infectious.period.h<- (gamma1/(gamma1+mu))*(gamma2/(gamma2+mu))*(1/(mu+rho1))*(1+rho1/(mu+rho2))   #assumed mean infectious period of patient infected in a hospital
# (approximate....as for the moment we ignore discharge and readmission effects, which will lead to a small correction)



parameters<-c(
  
  # Note that these R_HH values (components of the NGM) are approximate as in reality need to account for discharges etc 
  # but they are measure of secondary cases while individuals remain in the compartments they start in
  beta_HH=R_HH/mean.infectious.period.h,
  betaprimed_HH=0,    #  initially assume that there is no transmission from known severe cases (i.e. effective isolation)
  beta_HCWH = R_HCWH/mean.infectious.period.hcw ,
  beta_HHCW = R_HHCW/mean.infectious.period.h ,
  beta_CHCW =  R_CHCW/mean.infectious.period ,
  betaprimed_HHCW =0, #  initially assume that there is no transmission from known severe cases (i.e. effective isolation)
  # betaprimed_HHCW = 0.2*R_HHCW/mean.infectious.period, 
  beta_CC =R_CC/mean.infectious.period  ,
  betaprimed_CC=0,
  beta_HCWC=R_HCWC/mean.infectious.period,
  beta_HCWHCW = R_HCWHCW/mean.infectious.period.hcw,
  mu =0.2,
  muprimed =0.05,
  alpha =0.001,
  alphaprimed=0.5,
  gamma1=0.5,
  gamma2=0.5,
  rho1=0.3,
  rho2 =0.3 ,
  rho3=0.1,
  pi_H=0.3,
  pi_C =0.01,
  pi_HCW =0.01,
  changepoint1=40, # day on which there is first change in transmission rate in the community
  changepoint2=100, #etc
  changepoint3=150,
  changepoint4=200,
  
  phi1=0.2,  # scaling factor for community transmission between changepoint1 and changepoint 2 (so baseline beta_CC  and beta_CHCW is mulpited by phi1 in this period)
  phi2=0.3,
  phi3=0.6,
  phi4=0.3 
)


parameters2<-parameters 
hosp_scaling<-0.75
parameters2["beta_HH"] <- parameters2["beta_HH"]  * hosp_scaling
parameters2["beta_HCWH"] <- parameters2["beta_HCWH"]  * hosp_scaling
parameters2["beta_HHCW"] <- parameters2["beta_HHCW"]  * hosp_scaling
parameters2["beta_CHCW"] <- parameters2["beta_CHCW"]  * hosp_scaling
parameters2["beta_HCWHCW"] <- parameters2["beta_HCWHCW"]  * hosp_scaling


parameters3<-parameters 
hosp_scaling<-0.5
parameters3["beta_HH"] <- parameters3["beta_HH"]  * hosp_scaling
parameters3["beta_HCWH"] <- parameters3["beta_HCWH"]  * hosp_scaling
parameters3["beta_HHCW"] <- parameters3["beta_HHCW"]  * hosp_scaling
parameters3["beta_CHCW"] <- parameters3["beta_CHCW"]  * hosp_scaling
parameters3["beta_HCWHCW"] <- parameters3["beta_HCWHCW"]  * hosp_scaling

times <- seq(1, 300,by=1 ) 

ystart<-c(
  S_H=999,
  E1_H=0,
  E2_H=0,
  I1_H=0,
  I2_H=0,
  Iprimed_H=1,
  R_H=0,
  S_HCW=4000,
  E1_HCW=0,
  E2_HCW=0,
  I1_HCW=0,
  I2_HCW=0,
  Iprimed_HCW=0,
  R_HCW=0,  
  S_C=499990,
  E1_C=0,
  E2_C=0,
  I1_C=10,
  I2_C=0,
  Iprimed_C=0,
  R_C=0,
  CumNosoInfections=0,
  CumDetectedNosoInfections=0,
  CumCOVIDAdmissions=0
  )


out<-ode(y=ystart,times=times, func=HospCovid,parameters)
out.df<-as.data.frame(out)
out.df$H.tot<-out.df$S_H + out.df$E1_H  +   out.df$E2_H +  out.df$I1_H +  out.df$I2_H + out.df$Iprimed_H +  out.df$R_H
out.df$HCW.tot<-out.df$S_HCW + out.df$E1_HCW  +   out.df$E2_HCW +  out.df$I1_HCW +  out.df$I2_HCW + out.df$Iprimed_HCW +  out.df$R_HCW
out.df$C.tot<-out.df$S_C + out.df$E1_C  +   out.df$E2_C +  out.df$I1_C +  out.df$I2_C + out.df$Iprimed_C +  out.df$R_C

out.df$CplusH<- out.df$C.tot + out.df$H.tot

out.df$CplusH
out.df$HCW.tot
out.df$H.tot
out.df$C.tot

out2<-ode(y=ystart,times=times, func=HospCovid,parameters2)
out2.df<-as.data.frame(out2)
out3<-ode(y=ystart,times=times, func=HospCovid,parameters3)
out3.df<-as.data.frame(out3)

par(mfrow=c(3,3))
plot(out.df$time,out.df$Iprimed_H, col='red',type='l',main="Hospitalised cases",sub = "(red=severe; orange= other)",ylim=c(0,1000),ylab="",xlab="")
lines(out.df$E1_H + out.df$E2_H + out.df$I1_H + out.df$I2_H, col="orange")

plot(out.df$E1_HCW + out.df$E2_HCW + out.df$I1_HCW + out.df$I2_HCW, col="orange",type='l',main="HCW cases",sub = "(red=severe; orange= other)",ylab="",xlab="")
lines(out.df$time,out.df$Iprimed_HCW, col='red')

plot(out.df$E1_C + out.df$E2_C + out.df$I1_C + out.df$I2_C, col="orange",main="Community cases",sub = "(red=severe; orange= other)",type='l',ylab="",xlab="")
lines(out.df$time,out.df$Iprimed_C, col='red')

n<-length(out.df$CumDetectedNosoInfections)
out.df$DailyDetectedNosoInfections<-0
for(i in  2:n) out.df$DailyDetectedNosoInfections[i]<- out.df$CumDetectedNosoInfections[i]- out.df$CumDetectedNosoInfections[i-1]
out.df$DailyNosoInfections<-0
for(i in  2:n) out.df$DailyNosoInfections[i]<- out.df$CumNosoInfections[i]- out.df$CumNosoInfections[i-1]

out.df$DailyCOVIDAdmissions<-0
for(i in  2:n) out.df$DailyCOVIDAdmissions[i]<- out.df$CumCOVIDAdmissions[i]- out.df$CumCOVIDAdmissions[i-1]
out.df$DailyCOVIDAdmissionsByApproxOnset<-NA  #i.e. amongst covid admission to hospital, approx how many had onset on this data  (caclulated assuming 7 days from onset to admission)
for(i in  1:(n-7)) out.df$DailyCOVIDAdmissionsByApproxOnset[i] <-out.df$DailyCOVIDAdmissions[i+7]
   
out.df$ProportionNosocomial<-out.df$DailyDetectedNosoInfections/(out.df$DailyDetectedNosoInfections + out.df$DailyCOVIDAdmissions)
plot(out.df$DailyNosoInfections,type='l',main="Daily nosocomial infections", sub="(dashed line: detected nosocomial infections)",ylab="",xlab="")
lines(out.df$DailyDetectedNosoInfections,lty=2)
plot(out.df$DailyCOVIDAdmissions,type='l',main="Daily covid admissions",ylab="",xlab="")
plot(out.df$ProportionNosocomial,type='l',main="Proportion nosocomial", sub="by detection or admission date",ylab="",xlab="")

#below we include only detected nosocomial
out.df$ProportionNosocomialByOnset<-out.df$DailyDetectedNosoInfections/(out.df$DailyDetectedNosoInfections + out.df$DailyCOVIDAdmissionsByApproxOnset)
#below we include undetected nosocomial
out.df$ProportionNosocomialByOnsetTotal<-out.df$DailyNosoInfections/(out.df$DailyNosoInfections + out.df$DailyCOVIDAdmissionsByApproxOnset)
plot(out.df$DailyCOVIDAdmissionsByApproxOnset,type='l', main=" COVID admissions",sub="(by approx onset)",ylab="",xlab="")

plot(out.df$ProportionNosocomialByOnsetTotal,type='l',main="Proportion nosocomial total", sub="(by onset date)",ylab="",xlab="")

plot(out.df$ProportionNosocomialByOnset,type='l', main="Proportion nosocomial",sub="(by onset date)",ylab="",xlab="")


# Now add a ggplot where figures are stacked 

