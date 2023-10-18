# Calculate probability of being infected on each day of stay given infected at some time during stay


calc_prob_infected_by_day_of_stay_given_infected<-function(los=20, probinfecteduringstay= 0.01){
  # arguments are length of stay (in days), and probability infected during stay 
  # (which is used to determine the infection hazard which is assumed constant)
  
  # If h is the hazard of infection then  probinfecteduringstay = 1- exp(-h*los). So...
   h = -log(1-probinfecteduringstay)/los
   # then calculate probability of infection on each day of stay conditional on infection occurring at some time
   probinfectedbydaygiveninfected<-NULL
   for(i in 1: los){
     probinfectedbydaygiveninfected<-c(probinfectedbydaygiveninfected, exp(-h*(i-1))*(1-exp(-h))/probinfecteduringstay)
   }
  names(probinfectedbydaygiveninfected)<-1:los
  return(probinfectedbydaygiveninfected)
  
}

for_given_hazard_find_abs_diff_in_proportion_infected<-function(h, los_distribution, P){
  # Function to be used with optim to find infection hazard that corresponds to infecting a proportion P of 
  # patients with a given length of stay distribution
  # arguments are h, assumed hazard of infection, los_distribution, a length of stay distribution, where labels are 
  #durations in days for LoS and values are probabilities
  # Returns the abosolute value of the difference between P and n the proportion infected given hazard, h, and los distribution
  # first calculate proportion infected
  prop_infected<-rep(NA, length(los_distribution))
  los_values<-as.integer(names(los_distribution))
  
  for(i in 1:length(prop_infected)){
    prop_infected[i]<- 1- exp(-h*los_values[i])
  }
  overallproportioninfected<- sum(los_distribution*prop_infected)
  diff_in_proportion_infected<-abs(overallproportioninfected-P)
  return(diff_in_proportion_infected)
}


calc_prob_infected_by_day_of_stay_given_infected2<-function(los=20, proportionofpatientsinfected= 0.01, losdistribution){
  # arguments are i) length of stay (in days) o fpatient for whom we want to calculate conditional probabilities of 
  # infection
  # the proportion of patients infected during stay assuming initially susceptible, based on losdistribution
  # losdistribution, which is a vector of probabilities of given length of stays (given by the cell labels)
  # (which is used to determine the infection hazard which is assumed constant)
  
  # start with an inital guess for the hazard then use optim to find a value for h to give the right overall 
  # proportin infected 
  
   h_guess<- -log(1-proportionofpatientsinfected)/7 
   # now use optim to find a value of h that infects proportionofpatientsinfected for given LoS distribution
   fit<-optim(h_guess,for_given_hazard_find_abs_diff_in_proportion_infected, los_distribution=losdistribution, P=proportionofpatientsinfected)
   print(fit)
   if(fit$convergence==0){
     h<-fit$par 
   } else { stop("optim did not converge") } 
   print("xxxxx")
   print(h)
   print(los)
   probinfecteduringstay < - 1-exp(- h* los)
   print(probinfecteduringstay)
  # then calculate probability of infection on each day of stay conditional on infection occurring at some time
  probinfectedbydaygiveninfected<-NULL
  for(i in 1: los){
    probinfectedbydaygiveninfected<-c(probinfectedbydaygiveninfected, exp(-h*(i-1))*(1-exp(-h))/probinfecteduringstay)
  }
  names(probinfectedbydaygiveninfected)<-1:los
  return(probinfectedbydaygiveninfected)
}

# Now explore approximation of constant conditional probability of infection with actual Length of stay distribution
los_data <- readRDS("LoS_by_Trust.rds")
los_data<-as.data.frame(LoS_by_Trust)
# los_data holds frequency of lengths of stays of 0,1,2,3,....days for each provider with >10,000 completed episodes over the period
# by trust and (provider) code and week. 
# Initially, just aggregate this across weeks and trusts to calculate probabilities of different los 
# agg_los_dist<-tapply(los_data$n, los_data$los.days, "sum")
agg_los_dist<-tapply(los_data$count, los_data$LoS, "sum")
# exclude episodes with a los of 0 days and normalise
agg_los_dist<-agg_los_dist[2:length(agg_los_dist)]
agg_los_dist<-agg_los_dist/sum(agg_los_dist)