# merge in new variant data into the sitrep_weekly dataframe


# First create a function that randomises variables in nvdata where assign prop_variant data based on 
# randomly permuted region variables and therefore assing prop_variant data to wrong regions 
# This is created so we can check that any association we find between prop_variant and transmissibility
# is no just due to temporal confounding - i.e. we expect the association to disappear when we
# randomly assigns nv data to the wrong regions (but with the same timing)

permute_regions<-function(regions=nvdata$nhser_name){
  regionvalues<-unique(regions)
  n<-length(regionvalues)
  permuted_regionvalues<-rep(NA, length(regionvalues))
  permuted_regions<-rep(NA, length(regions))
  permutation<-order(runif(n)) 
  #so permutation should contain a randomly ordered vector of integers 1 to n
  # we use this to permute region names so regionvalues[1] is renamed to regionvalues[permutation[1]] etc in the permutation
  for(i in 1:n){
    permuted_regionvalues[i]<-regionvalues[permutation[i]]
  }
  # print(regionvalues)
  # print(permuted_regionvalues)
  # now loop through regionvalues and create permuted_rgions
  for(i in 1:7){
    trueregion<-regionvalues[i]
    permutedregion<-permuted_regionvalues[i]
    permuted_regions[regions==trueregion]<-permutedregion
  }
  return(permuted_regions)
}

#  Next new variant data

#nvdata<-readRDS("sgene_by_ltla.rds")
nvdata<-readRDS("sgene_by_utla.rds")

days_since_Jan12020<-as.integer(nvdata$week_specimen - date("2020-01-01"))

# now calc week number so days 1to7 are week 1, days 8-14 are week 2 etc
nvdata$weeknum_spec<-days_since_Jan12020 %/% 7
nvdata$permuted_nhser_name<-permute_regions(nvdata$nhser_name) # permuted regions (used to test that this removes association)
# Now take mean prop_variant by region (nhser_name)

#nvmeans<-aggregate(nvdata$prop_nv, by=list(wk=nvdata$weeknum_spec, reg=nvdata$nhser_name), FUN="mean")

nvdata$propXsmples<-nvdata$prop_sgtf*nvdata$samples

# calculate mean prevalence by region and week
sgtfweightedmeans_numerator<-aggregate(nvdata$propXsmples, by=list(wk=nvdata$weeknum_spec, reg=nvdata$nhser_name), FUN="sum")
sgtfweightedmeans_denom<-aggregate(nvdata$samples, by=list(wk=nvdata$weeknum_spec, reg=nvdata$nhser_name), FUN="sum")
sgtfweightedmeans<-sgtfweightedmeans_numerator
sgtfweightedmeans$x<-sgtfweightedmeans_numerator$x/sgtfweightedmeans_denom$x
rm(sgtfweightedmeans_numerator,sgtfweightedmeans_denom)

# now do the same as the above but using permuted region labels
sgtfweightedmeans_numerator_perm<-aggregate(nvdata$propXsmples, by=list(wk=nvdata$weeknum_spec, reg=nvdata$permuted_nhser_name), FUN="sum")
sgtfweightedmeans_denom_perm<-aggregate(nvdata$samples, by=list(wk=nvdata$weeknum_spec, reg=nvdata$permuted_nhser_name), FUN="sum")
sgtfweightedmeans_permuted<-sgtfweightedmeans_numerator_perm
sgtfweightedmeans_permuted$x<-sgtfweightedmeans_numerator_perm$x/sgtfweightedmeans_denom_perm$x
rm(sgtfweightedmeans_numerator_perm,sgtfweightedmeans_denom_perm)

# nvmeans_permuted<-aggregate(nvdata$prop_variant, by=list(wk=nvdata$weeknum_spec, reg=nvdata$permuted_nhser_name), FUN="mean")

sitrep_weekly$prop_nv <- NA  # proporiton new variant
sitrep_weekly$prop_nv_permuted <- NA #  proporiton new variant for permuted region (used for testing)
n<-dim(sitrep_weekly)[1]
for(i in 1:n){
  region<-sitrep_weekly$region[i]
  week<-sitrep_weekly$wk[i]
  # propnv<-nvmeans$x[nvmeans$wk==week & nvmeans$reg==region]
  propnv<-sgtfweightedmeans$x[sgtfweightedmeans$wk==week & sgtfweightedmeans$reg==region]
  propnv_permuted<-sgtfweightedmeans_permuted$x[sgtfweightedmeans_permuted$wk==week & sgtfweightedmeans_permuted$reg==region]
  #print(propnv)
  if(length(propnv)==1) {
    sitrep_weekly$prop_nv[i] <- propnv
    if(length(propnv_permuted)==1){
      sitrep_weekly$prop_nv_permuted[i] <- propnv_permuted
    } else {
      print(c("no permuted nv data for week", week, " and region ", region))
    }  
  }
}


  