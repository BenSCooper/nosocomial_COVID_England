#  heatmap plots
#  test version 
# where we use Iprimed_C  to make decision about lockdown
# start lockdown is Iprimed_C is above 1000, and stop it if is below 10

### set up parameters and ICs 
library(lhs)
library(dplyr)
library(ggplot2)
library(cowplot)
library(hrbrthemes)
library(viridis)
library(gridExtra)

# beta_HH is transmission parameter for transmission from infectious hospital patient to susceptible hospital infection
# mu is hospital death and discharge rate  for hospitalised (not for COVID). In general will be time varying.
# muprimed is hospital death and discharge rate  for hospitalised COVID patients (i.e. those hospitalised because of COVID)
#  alpha is admission rate to hospital from community for those not known to be covid 
# alphaprimed is admission rate to hospital from communith for those with sever covid (i.e .in the compartment Iprimed_C)
# gamma1 is progression rate from E1 to E2, 
# gamma2 is progression rate from E2 to I1,
# rho1 is progression rate from I1 to I2,
# rho2 is progression rate from I2 to R or Iprimed (seriously ill),
# rho3 is progression rate from Iprimed to R
# pi_H is the proportion of hospitalised infected patients who progress to severe infection, Iprimed (where definition of severe is that hospitalisation would be recommended)
# pi_C is the proportion of infected patients in the community who progress to severe infection, Iprimed
# in general components of next generation matrix will be time varying

#R_HH<-   0.5   # R_HH= beta_HH/mean.infectious.period etc



days = 450 # how many days to simulate

mean.infectious.period<-6.66


R_HH<-   1.0
R_HCWH<- 0.2
R_HHCW<- 0.8
R_CC<-   3
R_HCWC<-  0.2 
R_HCWHCW<-0.5
R_CHCW <-0.1

ystart0<-c(
  S_H=1000,
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
  E1_C= 0,
  E2_C=0,
  I1_C=10,
  I2_C=0,
  Iprimed_C=0,
  R_C=0,
  CumNosoInfections=0,
  CumDetectedNosoInfections=0,
  CumCOVIDAdmissions=0,
  # new_inf_at_time_t0 = 0,
  # new_inf_at_time_t1 = 0,
  # new_inf_at_time_t2 = 0,
  # new_inf_at_time_t3 = 0,
  # new_inf_at_time_t4 = 0,
  # new_inf_at_time_t5 = 0,
  # new_inf_at_time_t6 = 0,
  # seven_day_cases = 0,
  lockdown=0,
  numlockdowns=0
)



gamma1<-0.5
gamma2<-0.5
mu<-0.2
rho1=0.3
rho2 =0.3 
rho3=0.1

mean.infectious.period<-6.66


# mean.infectious.period.hcw<-2 # assumed mean infectious period of a hcw in a hospital before they self-isolate
mean.infectious.period.hcw<-1/rho1 # assumed mean infectious period of a hcw in a hospital before they self-isolate

mean.infectious.period.h<- (gamma1/(gamma1+mu))*(gamma2/(gamma2+mu))*(1/(mu+rho1))*(1+rho1/(mu+rho2))   #assumed mean infectious period of patient infected in a hospital
# (approximate....as for the moment we ignore discharge and readmission effects, which will lead to a small correction)



parameters0<-c(
  
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
  mu = 0.2,
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
  # changepoint1=23 , # day on which there is first change in transmission rate in the community (23rd march)
  # changepoint2=23 + 103, # 0.96 (23/03 - 11/05, 49) + 0.72 (11/05 - 19/06, 39) + 0.89 (19/05 - 04/07, 15) (march to july 4th)
  # changepoint3= 23+ 103 + 27, # to August 1st
  # changepoint4= 23 + 103 + 27 + 31 , # from september 2nd
  
  #  phi1=0.27,  # scaling factor for community transmission between changepoint1 and changepoint 2 (so baseline beta_CC  and beta_CHCW is mulpited by phi1 in this period)
  #  phi2=0.35, # is this c1 to c3 or c2 to c3?? assuming 1
  #  phi3=0.41,
  #  phi4=0.46 
  trigger_lockdown_start = 0,
  trigger_lockdown_end = 0,
  #   phi_ld = 0.25,  # scaling factor for community transmission in lockdown periods
  phi_ld = 0.1,  # scaling factor for community transmission in lockdown periods
  # phi_ld = 0.2,
  phi_nonld = 0.5 # scaling factor for community transmission when lockdown is released 
  # phi_nonld = 0.6 # scaling factor for community transmission when lockdown is released 
)


## generate potential values and remove any where trigger for ending is greater than trigger for starting

#cr_start_sens <- c(seq(70, 190, by = 30)/500000)
#ld_start_trigger <- c(seq(11, 20, by = 1))
#ld_end_trigger <- c(seq(1,0, by = 1))

ld_start_trigger <- c(seq(10, 25, by = 0.5))
ld_end_trigger <- c(seq(0.5,5, by = 0.25))


#cr_start_sens <- c(seq(150, 1000, by = 50)/500000)
# * cr_start_sens <- c(seq(110, 1010, by = 100)/500000)

# * cr_end_sens <- c(seq(10,100, by = 10)/500000)


# trans_mult = c(0:5)
# trans_mult = c(1,0.5,0.25 )   # multiplier for hospital transmission rates (corresponds to hosp_scaling for high, medium and low hospital transmission)
trans_mult = c(1,0.75,0.5 )   # multiplier for hospital transmission rates (corresponds to hosp_scaling for high, medium and low hospital transmission)

#cr_end_sens[1] <- 1
cr_comb <- expand.grid(ld_start_trigger, ld_end_trigger, trans_mult)
cr_comb = cr_comb[cr_comb[,1] > cr_comb[,2],]

# parameters for null intervention

parameter.null.high<-parameters0
parameter.null.high[["trigger_lockdown_start"]]=9999999
parameter.null.med<-parameter.null.high
parameter.null.med[["beta_HH"]]<-0.75*parameter.null.high[["beta_HH"]]
parameter.null.med[["beta_HHCW"]]<-0.75*parameter.null.high[["beta_HHCW"]]
parameter.null.med[["beta_HCWH"]]<-0.75*parameter.null.high[["beta_HCWH"]]
parameter.null.med[["beta_HCWHCW"]]<-0.75*parameter.null.high[["beta_HCWHCW"]]
parameter.null.med[["betaprimed_HH"]]<-0.75*parameter.null.high[["betaprimed_HH"]]
parameter.null.med[["betaprimed_HHCW"]]<-0.75*parameter.null.high[["betaprimed_HHCW"]]

parameter.null.low<-parameter.null.high
parameter.null.low[["beta_HH"]]<-0.5*parameter.null.high[["beta_HH"]]
parameter.null.low[["beta_HHCW"]]<-0.5*parameter.null.high[["beta_HHCW"]]
parameter.null.low[["beta_HCWH"]]<-0.5*parameter.null.high[["beta_HCWH"]]
parameter.null.low[["beta_HCWHCW"]]<-0.5*parameter.null.high[["beta_HCWHCW"]]
parameter.null.low[["betaprimed_HH"]]<-0.5*parameter.null.high[["betaprimed_HH"]]
parameter.null.low[["betaprimed_HHCW"]]<-0.5*parameter.null.high[["betaprimed_HHCW"]]



new_param_sets = c()
# increase cut-off by 10 people
for(i in 1:nrow(cr_comb)){
  nps = parameters0
  nps[["trigger_lockdown_start"]] = cr_comb[i,1]
  nps[["trigger_lockdown_end"]] = cr_comb[i,2]
  
  
  nps[["beta_HH"]] = R_HH*cr_comb[i,3]/mean.infectious.period.h
  nps[["beta_HHCW"]] = R_HHCW*cr_comb[i,3]/mean.infectious.period.h
  nps[["beta_HCWH"]] = R_HCWH*cr_comb[i,3]/mean.infectious.period.hcw
  nps[["beta_HCWHCW"]] = R_HCWHCW*cr_comb[i,3]/mean.infectious.period.hcw
  nps[["betaprimed_HH"]] = 0*cr_comb[i,3]/mean.infectious.period
  nps[["betaprimed_HHCW"]] = 0*cr_comb[i,3]/mean.infectious.period
  new_param_sets = rbind(new_param_sets,nps)
}

colnames(new_param_sets) = names(parameters0)



#################################################################################################################

library(deSolve)

rootfun <- function (t, x, parms) {
  with(as.list(c(parms, x)), {
   triggereventifthisiszero<-(Iprimed_C - trigger_lockdown_start)*(Iprimed_C - trigger_lockdown_end)
   return(triggereventifthisiszero)
  })   
}

eventfun <- function(t, x, parms) {
  #  print("**** evenfun triggered ****")
  #    print(x[["Iprimed_C"]])
  #   print(x[["lockdown"]])
     midpoint<-(parms[["trigger_lockdown_end"]] + parms[["trigger_lockdown_start"]])/2
     if(  x[["Iprimed_C"]] <midpoint &  x[["lockdown"]]==1) {
       x[["lockdown"]]<-0
     }  
    if(x[["Iprimed_C"]]>midpoint &  x[["lockdown"]]==0) {
      x[["lockdown"]]<-1
      x[["numlockdowns"]]<-x[["numlockdowns"]] +1
    }  
  # x[["lockdown"]]<-1
  return(x)
}


HospCovid <- function(t, x, parms) { 
  
  
  with(as.list(c(parms, x)), {
    # beta_HH is transmission parameter for transmission from infectious hospital patient to susceptible hospital infection
    # mu is hospital death and discharge rate  for hospitalised (not for COVID). In general will be time varying.
    # muprimed is hospital death and discharge rate  for hospitalised COVID patients (i.e. those hospitalised because of COVID)
    #  alpha is admission rate to hospital from community for those not known to be covid 
    # alphaprimed is admission rate to hospital from communith for those with sever covid (i.e .in the compartment Iprimed_C)
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
    # seven_day_cases = mean(c(new_inf_at_time_t0, new_inf_at_time_t1, new_inf_at_time_t2, new_inf_at_time_t3, new_inf_at_time_t4, new_inf_at_time_t5, new_inf_at_time_t6) )
    
    if(lockdown){
      phi<-phi_ld
    }  else {
      if(numlockdowns==0) phi<-1 else phi<-phi_nonld
    }
    
    # old_phi = phi
    # if(seven_day_cases >= case_rate_lockdown_start*(S_C + E1_C + E2_C + I1_C + I2_C + Iprimed_C + R_C)){
    #   phi<-phi_ld
    # }
    # else if(seven_day_cases <= case_rate_lockdown_end*(S_C + E1_C + E2_C + I1_C + I2_C + Iprimed_C + R_C)  ) {
    #   phi<-phi_nonld
    # }
    # 
    
    #   else if(seven_day_cases >= lockdown_trigger3*(S_C + E1_C + E2_C + I1_C + I2_C + Iprimed_C + R_C) & seven_day_prev >=lockdown_trigger3*(S_C + E1_C + E2_C + I1_C + I2_C + Iprimed_C + R_C)) {
    #      phi<-phi_ld3
    #    }
    #    else {
    #      phi<-1
    #    }
    
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
    dS_HCW  <- -beta_HHCW*S_HCW*(I1_H)/N_HCW   - betaprimed_HHCW*S_HCW*Iprimed_H/N_HCW  -beta_HCWHCW*S_HCW*(I1_HCW)/N_HCW - phi*beta_CHCW*S_HCW*(I1_C + I2_C)/N_HCW
    dE1_HCW <- beta_HHCW*S_HCW*(I1_H)/N_HCW  +  betaprimed_HHCW*S_HCW*Iprimed_H/N_HCW  + beta_HCWHCW*S_HCW*(I1_HCW)/N_HCW  + phi*beta_CHCW*S_HCW*(I1_C + I2_C)/N_HCW- gamma1*E1_HCW 
    dE2_HCW <- gamma1*E1_HCW - gamma2*E2_HCW
    dI1_HCW <- gamma2*E2_HCW - rho1*I1_HCW 
    dI2_HCW <- rho1*I1_HCW - rho2*I2_HCW  
    dIprimed_HCW <- pi_HCW * rho2*I2_HCW -rho3*Iprimed_HCW 
    dR_HCW  <- (1- pi_HCW) * rho2*I2_HCW + rho3*Iprimed_HCW  
    
    dCumNosoInfections <- beta_HH*S_H*(I1_H + I2_H)/N_H  +  betaprimed_HH*S_H*Iprimed_H/N_H  + beta_HCWH*S_H*(I1_HCW + I2_HCW)/N_H 
    dCumDetectedNosoInfections <- rho1*I1_H   # note that this includes community infections with hospital onset 
    dCumCOVIDAdmissions <- limited.alphaprimed*Iprimed_C
    lockdown<-0        # note that this is the rate of change of lockdowns not the value of lockdowns
    numlockdowns<-0    # note that this is the rate of change of numlockdowns not the number of numlockdowns
    # new_inf_at_time_t6 =  new_inf_at_time_t5 - new_inf_at_time_t6 
    # new_inf_at_time_t5 =  new_inf_at_time_t4 - new_inf_at_time_t5
    # new_inf_at_time_t4 =  new_inf_at_time_t3 - new_inf_at_time_t4
    # new_inf_at_time_t3 =  new_inf_at_time_t2 - new_inf_at_time_t3
    # new_inf_at_time_t2 =  new_inf_at_time_t1 - new_inf_at_time_t2
    # new_inf_at_time_t1 =  new_inf_at_time_t0 - new_inf_at_time_t1
    # new_inf_at_time_t0 = gamma2*E2_C - new_inf_at_time_t0
    
    # phi = phi - old_phi
    # Return values 
    #    list(c(dS_H, dE1_H, dE2_H, dI1_H, dI2_H, dIprimed_H, dR_H,dS_HCW, dE1_HCW, dE2_HCW, dI1_HCW, dI2_HCW, dIprimed_HCW, dR_HCW, dS_C, dE1_C, dE2_C, dI1_C, dI2_C, dIprimed_C, dR_C, 
    #     dCumNosoInfections,dCumDetectedNosoInfections,dCumCOVIDAdmissions, new_inf_at_time_t0, new_inf_at_time_t1, new_inf_at_time_t2, new_inf_at_time_t3, new_inf_at_time_t4, new_inf_at_time_t5, new_inf_at_time_t6, seven_day_cases, lockdown,numlockdowns))
    list(c(dS_H, dE1_H, dE2_H, dI1_H, dI2_H, dIprimed_H, dR_H,dS_HCW, dE1_HCW, dE2_HCW, dI1_HCW, dI2_HCW, dIprimed_HCW, dR_HCW, dS_C, dE1_C, dE2_C, dI1_C, dI2_C, dIprimed_C, dR_C, 
               dCumNosoInfections,dCumDetectedNosoInfections,dCumCOVIDAdmissions, lockdown,numlockdowns))
           
  })  # end with
} # end HospCovid



##########################################################################################################################################

times <- seq(1, days ,by=1 ) 
parameters0[["trigger_lockdown_start"]]<-9999999999
parameters0[["trigger_lockdown_end"]]<-20

test<-ode(y=ystart0,times=times, func=HospCovid, parameters0, events=list(func=eventfun, root=TRUE),
          rootfun=rootfun)

out.null.high<-ode(y=ystart,times=times, func=HospCovid, parameter.null.high)
out.null.med<-ode(y=ystart,times=times, func=HospCovid, parameter.null.med)
out.null.low<-ode(y=ystart,times=times, func=HospCovid, parameter.null.low)
R_Cmax.high<-max(as.data.frame(out.null.high)$R_C)
R_Cmax.med<-max(as.data.frame(out.null.med)$R_C)
R_Cmax.low<-max(as.data.frame(out.null.low)$R_C)


all_res = c()
for(i in 1:nrow(new_param_sets)){
  
  print(i)
  
  parameters = new_param_sets[i,]
  ystart = ystart0
  
  # out<-ode(y=ystart,times=times, func=HospCovid, parameters)
  
  
  out<-ode(y=ystart,times=times, func=HospCovid, parameters, events=list(func=eventfun, root=TRUE),
      rootfun=rootfun)
  
  out.df<-as.data.frame(out)
  out.df$H.tot<-out.df$S_H + out.df$E1_H  +   out.df$E2_H +  out.df$I1_H +  out.df$I2_H + out.df$Iprimed_H +  out.df$R_H
  out.df$HCW.tot<-out.df$S_HCW + out.df$E1_HCW  +   out.df$E2_HCW +  out.df$I1_HCW +  out.df$I2_HCW + out.df$Iprimed_HCW +  out.df$R_HCW
  out.df$C.tot<-out.df$S_C + out.df$E1_C  +   out.df$E2_C +  out.df$I1_C +  out.df$I2_C + out.df$Iprimed_C +  out.df$R_C
  
  out.df$CplusH<- out.df$C.tot + out.df$H.tot
  
  
  n<-length(out.df$CumDetectedNosoInfections)
  out.df$DailyDetectedNosoInfections<-0
  for(j in  2:n) out.df$DailyDetectedNosoInfections[j]<- out.df$CumDetectedNosoInfections[j]- out.df$CumDetectedNosoInfections[j-1]
  out.df$DailyNosoInfections<-0
  for(j in  2:n) out.df$DailyNosoInfections[j]<- out.df$CumNosoInfections[j]- out.df$CumNosoInfections[j-1]
  
  out.df$DailyCOVIDAdmissions<-0
  for(j in  2:n) out.df$DailyCOVIDAdmissions[j]<- out.df$CumCOVIDAdmissions[j]- out.df$CumCOVIDAdmissions[j-1]
  out.df$DailyCOVIDAdmissionsByApproxOnset<-NA  #i.e. amongst covid admission to hospital, approx how many had onset on this data  (caclulated assuming 7 days from onset to admission)
  for(j in  1:(n-7)) out.df$DailyCOVIDAdmissionsByApproxOnset[j] <-out.df$DailyCOVIDAdmissions[j+7]
  
  
  #below we include only detected nosocomial
  out.df$ProportionNosocomialByOnset<-out.df$DailyDetectedNosoInfections/(out.df$DailyDetectedNosoInfections + out.df$DailyCOVIDAdmissionsByApproxOnset)
  #below we include undetected nosocomial
  out.df$ProportionNosocomialByOnsetTotal<-out.df$DailyNosoInfections/(out.df$DailyNosoInfections + out.df$DailyCOVIDAdmissionsByApproxOnset)
  
  
  out.df$ProportionNosocomial<-out.df$DailyDetectedNosoInfections/(out.df$DailyDetectedNosoInfections + out.df$DailyCOVIDAdmissions)
  
  out.df = data.frame(out.df)
  
  out.df$Paramset = i
  
  ps = data.frame(t(parameters))
  ps$Paramset= i
  
  y0 = data.frame(t(ystart))
  colnames(y0) = paste(colnames(y0),0, sep =".")
  y0$Paramset= i
  
  out.df = merge(out.df, ps, by = "Paramset")
  out.df = merge(out.df, y0, by = "Paramset")
  
  all_res = bind_rows(all_res, out.df)
}


##########################################################################


cr_comb$Paramset = 1:nrow(cr_comb)
colnames(cr_comb) = c("LD_start", "LD_end", "trans_mult", "Paramset")

all_res = left_join(all_res, cr_comb)
#all_res$Lockdown = FALSE
#all_res$Lockdown[all_res$phi < 0.4] = TRUE


# aggregate data 
dat_for_heatmap = all_res %>% group_by(start = LD_start, end = LD_end, trans_mult) %>% summarise(total_ld_length = length(which(lockdown == 1)), num_lds=max(numlockdowns),  total_inf = max(R_C))

# plot LD length against start cut-off for different values of "trans_mult" (nosocomial transmission prob multiplier)
#ggplot(dat_for_heatmap[dat_for_heatmap$end =="7",]) + geom_line(size = 1, aes(x = start, y = total_ld_length, group = trans_mult, colour = factor(trans_mult))) + theme_bw()

# plot LD length against against end cut-off for different values of "trans_mult" (nosocomial transmission prob multiplier)
#ggplot(dat_for_heatmap[dat_for_heatmap$start =="130",]) + geom_line(size = 1, aes(x = end, y = total_ld_length, group = trans_mult, colour = factor(trans_mult))) + theme_bw()

dat_for_heatmap$trans_mult2 <-factor(dat_for_heatmap$trans_mult, levels=c(1.0,0.75,0.5),labels=c("High hospital transmission","Intermediate hospital transmission","Low hospital transmission"))
all_res$trans_mult2 <-factor(all_res$trans_mult, levels=c(1.0,0.75,0.5),labels=c("High hospital transmission","Intermediate hospital transmission","Low hospital transmission"))

dat_for_heatmap$startx2<-dat_for_heatmap$start * 2

dat_for_heatmap$endx2<-dat_for_heatmap$end * 2


labelscaling<-1.5
all_res$LD_startx2<-all_res$LD_start * 2
all_res$LD_endx2<-all_res$LD_end* 2
  
## heat map for total number of community cases recovered by end of sim
P1<-ggplot(all_res) + geom_tile(aes(x = LD_startx2, y = LD_endx2, fill = R_C/500000))  +facet_wrap(~trans_mult2)+ labs(x = "Threshold for lockdown to be imposed", y = "Threshold for lockdown to be lifted", fill = "Proportion infected") + theme_bw() +
  scale_fill_gradientn(colours = magma(10, direction = -1)) +
  theme_ipsum() 

## heatmap for total lockdown length
P2<-ggplot(dat_for_heatmap) + geom_tile(aes(x = startx2, y = endx2, fill = total_ld_length))  +facet_wrap(~trans_mult2) + labs(x = "Threshold for lockdown to be imposed", y = "Threshold for lockdown to be lifted", fill = "Total days in lockdown") + theme_bw() + 
  # scale_fill_gradientn(colours = heat.colors(10, rev = T)) +
  scale_fill_gradientn(colours = magma(10, direction = -1)) +
  theme_ipsum() 


# heatmap for reduction from max per lockdown day
dat_for_heatmap = dat_for_heatmap %>% group_by(trans_mult2) %>% mutate(max_per_trans_mult = max(total_inf))
dat_for_heatmap$max_per_trans_mult[dat_for_heatmap$trans_mult==1]<-R_Cmax.high
dat_for_heatmap$max_per_trans_mult[dat_for_heatmap$trans_mult==0.75]<-R_Cmax.med
dat_for_heatmap$max_per_trans_mult[dat_for_heatmap$trans_mult==0.5]<-R_Cmax.low


P3<- ggplot(dat_for_heatmap) + geom_tile(aes(x = startx2, y = endx2, fill = (max_per_trans_mult - total_inf)/(total_ld_length*500000))) +facet_wrap(~trans_mult2) + 
  labs(x = "Threshold for lockdown to be imposed", y = "Threshold for lockdown to be lifted", fill = " Infections averted \n (as proportion of population) \n per day in lockdown") + theme_bw() +
  scale_fill_gradientn(colours = magma(10, direction = -1)) +
  theme_ipsum() 



dat_for_heatmap$f_num_lds<-as.factor(dat_for_heatmap$num_lds)



P4<- ggplot(dat_for_heatmap) + geom_tile(aes(x = startx2, y = endx2, fill = f_num_lds)) +facet_wrap(~trans_mult2) + 
  labs(x = "Threshold for lockdown to be imposed", y = "Threshold for lockdown to be lifted", fill = " Number of lockdowns") + theme_bw() +
  # scale_fill_gradientn(colours = magma(10, direction = -1)) +
  scale_fill_viridis(discrete = TRUE,option="F",direction=-1) +
  theme(axis.title.x = element_text(size = rel(3), angle = 90)) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90)) +
  theme_ipsum() 


P1
P2
P3

# grid.arrange(P1,P2,P3,nrow = 3)


# ggplot(dat_for_heatmap) + geom_point(size =aes(x = total_inf, y = total_ld_length, group = trans_mult, colour = factor(trans_mult))) + theme_bw() 

