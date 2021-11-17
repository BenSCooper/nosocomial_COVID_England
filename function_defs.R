# Functions used for calculating how likely nosocomial covid infections are to be detected
# These will be used to make inferences about the number of infections.

CalcProbPosByDay<-function(los, day_of_infection, pcr_sens_vec, inc_vec, prob_sympt,screening_days=c(0,0,0,0,0,0,0), repeatPCRifsymptomaticandneg=3){
  
  # Returns a vector of length   (los-day_of_infection) which contains probabilities that a hospital acquired infection
  # that occurs on day_of_infection (counting from 1, where 1 is the first day of hospital stay)  is detected on each day of 
  # stay. Elements of the vectored returned are labeled with the day of stay  (from day_of_infection+1  to los) and the values in the vector
  # elements are the calculated probabilities. These depend on the arguments
  # pcr_sens_vec  a vector where the ith element gives the sensitivity of a PCR test performed i days after the day of infection
  # inc_vec  a vector where the ith element gives the probability of an incubation period of i days (given symptomatic)
  # prob_sympt gives the probability that that infection has symptoms that would lead to a PCR test
  # screening_days is vector of seven elements, each 0 or 1, where 0 means no screening on that day of the week 
  # and 1 means there was screening on that day of the week
  
  # Because the chance of detecting the infection on a given day depends on the day of the week of the infection in relation
  # to the days of the week of the screening days, and we  assume each day of the week is equally likely for a new infection, the calculations
  # are performed 7 times with different days of the week  of the infection, and the vector of probabilities returned 
  # contains means from from these seven iterations 
  
  # the  argument repeatPCRifsymptomaticandneg specifies the policy for repeat PCR tests if the patient develops symptoms and has a negative 
  # PCR test. A value of 0 would mean no repeat PCR tests, a value of i would mean if the patient develops symptoms and the initial PCR test is negative, then 
  # on the next i days (or until day of discharge if sooner) perform PCR test until positive
  
  # note that if screening_days==c(0,0,0,0,0,0,0)  then detection will be be based on screening due to symptom onset only
  # otherwise detection will be a result of both screening due to symptoms and routine screening
  # if we want results to be based only on asymptomatic screening then we need to set prob_sympt to 0 
  
  if(los<=day_of_infection) stop("los must be bigger than day_of_infection")
  prob_first_detected_on_day<-rep(0, los-day_of_infection)
  prob_newly_pos_given_neg<-rep(0, los-day_of_infection)
  names(prob_first_detected_on_day)<-(day_of_infection+1):los
  
  # First code this with no screening
  
   if(prob_sympt!=0){ # i.e we are  screening patients as a result of symtpoms
    
    # then calculate prob of symptoms on each day after infection & prob of testing
    # assume daily testing continues on each day following symptom onset
    prob_not_detected<-1  # used to keep track of the probability that infection hasn't been detected as we loop through days
    # if no asymptomatic screening assume last day detection is possible is day of symptom onset  
    last_poss_day_for_new_infection_onset<- min(length(inc_vec), length(prob_first_detected_on_day) )   # counting from day of infection, so 1 means 1 day after day after infection
    prob_post_symptom_onset<-0
    prob_post_symptom_onset_and_no_pos_test <-0
    for(i in 1:last_poss_day_for_new_infection_onset){ #  this loops over possible days of symptom onset
       prob_symptom_onset_today<-inc_vec[i] * prob_sympt
       prob_pcr_pos_if_tested_today<- pcr_sens_vec[i]
       # line below to account for detection probability form testing on day of symptom onset
       prob_first_detected_on_day[i]<-prob_first_detected_on_day[i] + prob_symptom_onset_today * prob_pcr_pos_if_tested_today 
       # now we need some code to say what happens if symptom onset occur on day i, but the PCR test is negative
       # this is determined by the value of repeatPCRifsymptomaticandneg which specifies the number of days to perform repeat testing until positive
       repeattestday<-i+1 #day for repeat testing expressed as a function of the number of days after day_of_infection
       number_repeat_tests_after_negative_PCR_performed<-0   
       prob_all_negscreens_so_far<- prob_symptom_onset_today*(1- prob_pcr_pos_if_tested_today)  # i.e. the prob of a negative screen if tested on day of symptom onset
       #browser()
        while((repeattestday + day_of_infection <= los) & (number_repeat_tests_after_negative_PCR_performed< repeatPCRifsymptomaticandneg)){ 
         prob_first_detected_on_day[repeattestday]<- prob_first_detected_on_day[repeattestday] + prob_all_negscreens_so_far * pcr_sens_vec[repeattestday]
         prob_all_negscreens_so_far<-prob_all_negscreens_so_far*(1-pcr_sens_vec[repeattestday])
         repeattestday<-repeattestday+1
         number_repeat_tests_after_negative_PCR_performed<-number_repeat_tests_after_negative_PCR_performed+1
       }
    }
    # now create a vector prob_newly_pos_given_neg which gives the prob of testing positive on day i, *given* not yet positive by the end of day i-1
    prob_newly_pos_given_neg<-rep(0, length(prob_first_detected_on_day)) 
    prob_newly_pos_given_neg[1] <-prob_first_detected_on_day[1]
    for(i in 2:last_poss_day_for_new_infection_onset){ #  this loops over possible days of symptom onset
      prob_newly_pos_given_neg[i]<-prob_first_detected_on_day[i]/(1-sum(prob_first_detected_on_day[1:(i-1)]))
    }
  }
  
  # print("prob_newly_pos_given_neg")
  #print(prob_newly_pos_given_neg)
  # so at this point prob_first_detected_on_day holds the probability that infection is *first* detected on the given day due
  # to screening as a result of symptomatic infection. 
  # if prob_sympt had been set to 0 when calling the function these probabilities will also be zero.
  
  if(sum(screening_days)!=0){ # i.e there is screening
    #  browser()
    # First create 7 vectors of same length as prob_first_detected_on_day each 
    # with a 1 on days with screening and 0 on days without. 
    # The seven vectors correspond to the day of infection occurring on different days of the week
    # Then for each create a vector of prob of being first detected by each day accounting for screening asymptomatics and symptomatics
    # 
    # Then average this vector to obtain  detection probs for each day.
    if(length(screening_days)!=7)  stop("screening days should be a vector with 7 elements, 0 for days with no screening, 1 for days with screening")
    screening_policies<-matrix(NA,nrow=7, ncol=7)  #  one row per policy, seven rows to account for infections on different days of the week
    for(i in 1:7){
      screening_policies[i,]<-screening_days
      screening_days<-c(screening_days[7],screening_days[1:6])
    }
    # print(screening_policies)
    # Now create a matrix to hold probability of testing positive on each day after infection for each of the seven screening protocols
    days_since_infection<-los-day_of_infection
    screening_on_day<-matrix(0,nrow=7, ncol=days_since_infection)
    prob_screening_pos_on_day<-matrix(0,nrow=7, ncol=days_since_infection)  #  columns index days since infection, rowns index screening policy
    # create a padded_pcr_sens_vec which is of the same length as days since infection
    padded_pcr_sens_vec<-pcr_sens_vec
    if(length(pcr_sens_vec)< days_since_infection) {
      padded_pcr_sens_vec <- c(pcr_sens_vec, rep(0,days_since_infection- length(pcr_sens_vec) ))
    }
    if(length(pcr_sens_vec)> days_since_infection) {
      padded_pcr_sens_vec<-pcr_sens_vec[1:days_since_infection]
    }
    # print(padded_pcr_sens_vec)
    for(j in 1:7){
      screening_on_day[j,] <- screening_policies[j,(0:(days_since_infection-1))%%7 +1 ] #ensures screening (yes/no) is defined for each day following infection
      prob_screening_pos_on_day[j,] <-   screening_on_day[j,] * padded_pcr_sens_vec 
      # note that prob_screening_pos_on_day above is not the prob screening positive for the first time, so these probabilities don't need to add to one
      # rather, it's the just the prob that a screen on given day is positive, or a 0 if the policy says now screening on that day
    }  
    # print(c("screening_on_day =", screening_on_day))
    # print(c("prob_screening_pos_on_day= ", prob_screening_pos_on_day))
    # Now, for each of these seven policies calculate the probability of first screening positive on each day 
    # taking account of the previously calculated probabilities of first screening positive as a result of tests done as a result of 
    # symptomatic infection, which are stored in prob_first_detected_on_day  
    prob_first_screening_pos_on_day<-matrix(0,nrow=7, ncol=days_since_infection) 

    for(j in 1:7){
      # browser()
      prob_not_pos_so_far<-1
      prob_first_screening_pos_on_day[j,1]<-prob_newly_pos_given_neg[1] + (1-prob_newly_pos_given_neg[1])* prob_screening_pos_on_day[j,1]
      if(days_since_infection>1){
        for(k in 2:days_since_infection){
          prob_not_pos_so_far<-prob_not_pos_so_far -prob_first_screening_pos_on_day[j, (k-1)]
          prob_first_screening_pos_on_day[j,k]<-prob_not_pos_so_far*(prob_newly_pos_given_neg[k] + (1-prob_newly_pos_given_neg[k])* prob_screening_pos_on_day[j,k])
        }
      }  
    }  
    # browser()
    prob_first_screening_pos_on_day<-apply(prob_first_screening_pos_on_day,2,mean) # i.e. average over the seven results aboves
    names(prob_first_screening_pos_on_day)<-(day_of_infection+1):los
     # print(screening_on_day)
    # print(screening_policies)
    # print(screening_days)
  } else { # no screening
    prob_first_screening_pos_on_day<-prob_first_detected_on_day
  }
  # return(prob_first_detected_on_day)  
  return(prob_first_screening_pos_on_day)
}
  
CalcProbPosByDay2<-function(los, day_of_infection, pcr_sens_vec, inc_vec, prob_sympt,screening_days=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), repeatPCRifsymptomaticandneg=0){
  # CalcProbPosByDay2 is based on CalcProbPosByDay but whereas as CalcProbPosByDay assumes a weekly pattern of screening repeats throught
  # a patient's stay this version assumes the pattern is fixed and that day of screening is always based on the number of days since patient admission
  # Returns a vector of length   (los-day_of_infection) which contains probabilities that a hospital acquired infection
  # that occurs on day_of_infection (counting from 1, where 1 is the first day of hospital stay)  is detected on each day of 
  # stay. Elements of the vector returned are labeled with the day of stay  (from day_of_infection+1  to los) and the values in the vector
  # elements are the calculated probabilities. These depend on the arguments
  # pcr_sens_vec  a vector where the ith element gives the sensitivity of a PCR test performed i days after the day of infection
  # inc_vec  a vector where the ith element gives the probability of an incubation period of i days (given symptomatic)
  # prob_sympt gives the probability that that infection has symptoms that would lead to a PCR test
  # screening_days is vector of seven elements, each 0 or 1, where 0 means no screening on that day of the week 
  # and 1 means there was screening on that day of the week
  
  # the  argument repeatPCRifsymptomaticandneg specifies the policy for repeat PCR tests if the patient develops symptoms and has a negative 
  # PCR test. A value of 0 would mean no repeat PCR tests, a value of i would mean if the patient develops symptoms and the initial PCR test is negative, then 
  # on the next i days (or until day of discharge if sooner) perform PCR test until positive
  
  # note that if screening_days==c(0,0,0,0,0,0,0)  then detection will be be based on screening due to symptom onset only
  # otherwise detection will be a result of both screening due to symptoms and routine screening
  # if we want results to be based only on asymptomatic screening then we need to set prob_sympt to 0 
  
  if(los<=day_of_infection) stop("los must be bigger than day_of_infection")
  prob_first_detected_on_day<-rep(0, los-day_of_infection)
  prob_newly_pos_given_neg<-rep(0, los-day_of_infection)
  names(prob_first_detected_on_day)<-(day_of_infection+1):los
  
  # First code this with no screening
  
  if(prob_sympt!=0){ # i.e we are  screening patients as a result of symtpoms
    
    # then calculate prob of symptoms on each day after infection & prob of testing
    # assume daily testing continues on each day following symptom onset
    prob_not_detected<-1  # used to keep track of the probability that infection hasn't been detected as we loop through days
    # if no asymptomatic screening assume last day detection is possible is day of symptom onset  
    last_poss_day_for_new_infection_onset<- min(length(inc_vec), length(prob_first_detected_on_day) )   # counting from day of infection, so 1 means 1 day after day after infection
    prob_post_symptom_onset<-0
    prob_post_symptom_onset_and_no_pos_test <-0
    for(i in 1:last_poss_day_for_new_infection_onset){ #  this loops over possible days of symptom onset
      prob_symptom_onset_today<-inc_vec[i] * prob_sympt
      prob_pcr_pos_if_tested_today<- pcr_sens_vec[i]
      # line below to account for detection probability form testing on day of symptom onset
      prob_first_detected_on_day[i]<-prob_first_detected_on_day[i] + prob_symptom_onset_today * prob_pcr_pos_if_tested_today 
      # now we need some code to say what happens if symptom onset occur on day i, but the PCR test is negative
      # this is determined by the value of repeatPCRifsymptomaticandneg which specifies the number of days to perform repeat testing until positive
      repeattestday<-i+1 #day for repeat testing expressed as a function of the number of days after day_of_infection
      number_repeat_tests_after_negative_PCR_performed<-0   
      prob_all_negscreens_so_far<- prob_symptom_onset_today*(1- prob_pcr_pos_if_tested_today)  # i.e. the prob of a negative screen if tested on day of symptom onset
      #browser()
      while((repeattestday + day_of_infection <= los) & (number_repeat_tests_after_negative_PCR_performed< repeatPCRifsymptomaticandneg)){ 
        prob_first_detected_on_day[repeattestday]<- prob_first_detected_on_day[repeattestday] + prob_all_negscreens_so_far * pcr_sens_vec[repeattestday]
        prob_all_negscreens_so_far<-prob_all_negscreens_so_far*(1-pcr_sens_vec[repeattestday])
        repeattestday<-repeattestday+1
        number_repeat_tests_after_negative_PCR_performed<-number_repeat_tests_after_negative_PCR_performed+1
      }
    }
    # now create a vector prob_newly_pos_given_neg which gives the prob of testing positive on day i, *given* not yet positive by the end of day i-1
    prob_newly_pos_given_neg<-rep(0, length(prob_first_detected_on_day)) 
    prob_newly_pos_given_neg[1] <-prob_first_detected_on_day[1]
    for(i in 2:last_poss_day_for_new_infection_onset){ #  this loops over possible days of symptom onset
      prob_newly_pos_given_neg[i]<-prob_first_detected_on_day[i]/(1-sum(prob_first_detected_on_day[1:(i-1)]))
    }
  }
  
  # print("prob_newly_pos_given_neg")
  #print(prob_newly_pos_given_neg)
  # so at this point prob_first_detected_on_day holds the probability that infection is *first* detected on the given day due
  # to screening as a result of symptomatic infection. 
  # if prob_sympt had been set to 0 when calling the function these probabilities will also be zero.
  
  if(sum(screening_days)!=0){ # i.e there is screening
    #  browser()
    # First create 7 vectors of same length as prob_first_detected_on_day each 
    # with a 1 on days with screening and 0 on days without. 
    # The seven vectors correspond to the day of infection occurring on different days of the week
    # Then for each create a vector of prob of being first detected by each day accounting for screening asymptomatics and symptomatics
    # 
    # Then average this vector to obtain  detection probs for each day.
    # *** code below was used for weekly repeating screening patterns, but in this we assume screening is on fixed days from admission and does not repeat
    #  if(length(screening_days)!=7)  stop("screening days should be a vector with 7 elements, 0 for days with no screening, 1 for days with screening")
    #    screening_policies<-matrix(NA,nrow=7, ncol=7)  #  one row per policy, seven rows to account for infections on different days of the week
    # for(i in 1:7){
    #   screening_policies[i,]<-screening_days
    #   screening_days<-c(screening_days[7],screening_days[1:6])
    # }
    # print(screening_policies)
    # ***    
    # Now create a matrix to hold probability of testing positive on each day after infection for each of the seven screening protocols
    days_since_infection<-los-day_of_infection
    #   screening_on_day<-matrix(0,nrow=7, ncol=days_since_infection)
    screening_on_day <- rep(0,days_since_infection)
    
    #  prob_screening_pos_on_day<-matrix(0,nrow=7, ncol=days_since_infection)  #  columns index days since infection, rowns index screening policy
    prob_screening_pos_on_day <- rep(0,days_since_infection)
    
    # create a padded_pcr_sens_vec which is of the same length as days since infection
    padded_pcr_sens_vec<-pcr_sens_vec
    if(length(pcr_sens_vec)< days_since_infection) {
      padded_pcr_sens_vec <- c(pcr_sens_vec, rep(0,days_since_infection- length(pcr_sens_vec) ))
    }
    if(length(pcr_sens_vec)> days_since_infection) {
      padded_pcr_sens_vec<-pcr_sens_vec[1:days_since_infection]
    }
    if(length(screening_days)< los) {
      screening_days <- c(screening_days, rep(0,los- length(screening_days) ))
    }
    if(length(screening_days)> los) {
      screening_days<-screening_days[1:los]
    }
    
    #  print(c("screening days are ",screening_days))
    screening_on_day<-screening_days[(day_of_infection+1):los]
    
    # print(padded_pcr_sens_vec)
    prob_screening_pos_on_day<-screening_on_day*padded_pcr_sens_vec
    
    #   print(c("prob_screening_pos_on_day  ",prob_screening_pos_on_day))
    
    #   screening_on_day[j] <- screening_policies[j,(0:(days_since_infection-1))%%7 +1 ] #ensures screening (yes/no) is defined for each day following infection
    # note that prob_screening_pos_on_day above is not the prob screening positive for the first time, so these probabilities don't need to add to one
    # rather, it's the just the prob that a screen on given day is positive, or a 0 if the policy says now screening on that day
    
    # print(c("screening_on_day =", screening_on_day))
    # print(c("prob_screening_pos_on_day= ", prob_screening_pos_on_day))
    # Now, for each of these seven policies calculate the probability of first screening positive on each day 
    # taking account of the previously calculated probabilities of first screening positive as a result of tests done as a result of 
    # symptomatic infection, which are stored in prob_first_detected_on_day  
    prob_first_screening_pos_on_day<-rep(0,days_since_infection)
    prob_not_pos_so_far<-1
    prob_first_screening_pos_on_day[1]<-prob_newly_pos_given_neg[1] + (1-prob_newly_pos_given_neg[1])* prob_screening_pos_on_day[1]
    if(days_since_infection>1){
      for(k in 2:days_since_infection){
        prob_not_pos_so_far<-prob_not_pos_so_far -prob_first_screening_pos_on_day[(k-1)]
        prob_first_screening_pos_on_day[k]<-prob_not_pos_so_far*(prob_newly_pos_given_neg[k] + (1-prob_newly_pos_given_neg[k])* prob_screening_pos_on_day[k])
      }
    }  
    
    # browser()
    names(prob_first_screening_pos_on_day)<-(day_of_infection+1):los
    # print(screening_on_day)
    # print(screening_policies)
    # print(screening_days)
  } else { # no screening
    prob_first_screening_pos_on_day<-prob_first_detected_on_day
  }
  # return(prob_first_detected_on_day)  
  return(prob_first_screening_pos_on_day)
}


CalcProbDetectedOnDay<-function(los_dist,  pcr_sens_vec, inc_vec, prob_sympt,screening_days=c(0,0,0,0,0,0,0), repeatPCRifsymptomaticandneg=3,wklyscreeningpattern=T){
  #returns a vector of same length as los_dist which holds the probability that one new infection
  #(assumed to infected an in-patient at random on a given day) is detected on 1,2,3....days after hospital admission 
  #if  wklyscreeningpattern is TRUE screening_days is taken a pattern that repeats every seven days
  #assumed to take place on fixed days of the week (rather than at certain times after admission)
  #if  wklyscreeningpattern is FALSE screening_days is taken as a non-repeating pattern with no screening after the last date given
  # and screening is assumed to to take place at fixed days after admission so (0,0,1,0,0,1,0) would mean screening on 3 and 6 days after admission
  #note that screening_days is expected to contain 1 or 0s specifying whether screening is performed on a given day (1) or not (0).

  max.los<-length(los_dist)
  prob.detection.by.day<-matrix(0, nrow=max.los, ncol=max.los, dimnames=list(los=1:max.los, day.of.say=1:max.los))

  for(i in 2:max.los){  # i is los of patient (i.e. number of days of total stay)
    
    prob.detect.by.day.given.los<-rep(0,max.los)
    for(j in 1:(i-1)){ # j  is day.of.infection
    if(wklyscreeningpattern){
      prob.detection<-CalcProbPosByDay(los=i, day_of_infection = j, pcr_sens_vec = pcr_sens_vec, inc_vec=inc_vec, prob_sympt=prob_sympt, screening_days=screening_days, repeatPCRifsymptomaticandneg=repeatPCRifsymptomaticandneg) 
    } else {
      prob.detection<-CalcProbPosByDay2(los=i, day_of_infection = j, pcr_sens_vec = pcr_sens_vec, inc_vec=inc_vec, prob_sympt=prob_sympt, screening_days=screening_days, repeatPCRifsymptomaticandneg=repeatPCRifsymptomaticandneg) 
    }
    detection.days<-as.numeric(names(prob.detection))
    prob.detect.by.day.given.los[detection.days]<-prob.detect.by.day.given.los[detection.days]+prob.detection
    }
    prob.detect.by.day.given.los<-prob.detect.by.day.given.los/i #divide by i, as the i days of infection are assumed equally likely (i.e. patient equally likely to acquire infection on any day of stay)
    
    prob.detection.by.day[i,] <-prob.detect.by.day.given.los
  }
  
  # Now to calculate overall probability of detection on each day of stay we take a weighted mean
  # where the weights for each los (row) are the value of that los * the frequencof that los
  
  weights<-(1:max.los)*los_dist
  
  probs<-apply(prob.detection.by.day,2,weighted.mean, weights)
  return(probs)
}  


binom.ntrial.pos<-function(x, p, ntrials.prior){
  # function that returns the posterior distribution for the unknown
  # number of binomial trials
  # It takes as input:
  # x, the number of successes in an unknown number of binomial trials
  # p, the probability of success in each binomial trial
  # ntrials.prior, a vector specifying the prior prob for 1,2,3...successes
  n<-length(ntrials.prior)
  if(n < x) stop("need non-zero prior prob for at least x trials")
  ntrials.prior<-ntrials.prior/sum(ntrials.prior) #normalize
  
  # Using Bayes formula p(a|b) = p(b|a)p(a)/p(b)
  # Assuming known p, we want to calculate this
  # p(ntrials|x,p)=p(x|p, ntrials) p(ntrials)/p(x)
  # where p(x)= sum over i: p(x| ntrials=i ) p(ntrials=i)
  prob_x_given_p_and_ntrials<-dbinom(x, size=1:n, p)
  numerator<-prob_x_given_p_and_ntrials*ntrials.prior
  denom<-sum(dbinom(x,1:n,p)*ntrials.prior)
  posterior<-numerator/denom
  return(posterior)
}


# # testing
# pcr_sens_vec<-c(0.1,0.3,0.6,0.9, 0.95, 0.95, 0.95, 0.9, 0.9, 0.85, 0.85, 0.85, 0.8,  0.8, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2)  # just made up for now but can look up
# pcr_sens_vec<-rep(0.5,30)
# prob_sympt<-0.0
# inc_vec<-c(0.05,0.15,0.2, 0.2,0.2,0.1,0.05,0.05,.01)
# inc_vec<-inc_vec/sum(inc_vec)
# 
# test<-CalcProbPosByDay(100,15,pcr_sens_vec,inc_vec,prob_sympt,c(0,0,0,0,0,1,1) ,repeatPCRifsymptomaticandneg=3)
# test<-CalcProbPosByDay(100,95,pcr_sens_vec,inc_vec,prob_sympt,c(1,0,0,0,0,0,0) ,repeatPCRifsymptomaticandneg=0)
# print(test)
# print(sum(test))