# merge in vaccine data  into the sitrep_weekly dataframe

vax.data<-read.csv(file = "vacc_cov_by_region.csv",  na.strings = "*")

vax.data <- vax.data[,-1]
# So column 1 has region code and col headings for cols 2... give data, and the cell entries show proportion of sampled
# hcws vaccinated 

# steps to process are to transform so there are three columns: region (using same codes as sitrep weekly, date, and vax coverage)

# note that EM and WM are west midlands and east midlands (so can take a average to get Midlands)
#YH is Yorkshire and the Humber which is combined with NE to create North East and Yorkshire in the sitrep.
  
# so create new rows in vax.data for NEY (North East and Yorkshire) and M (Midlands)
# taken as averages of component sub-regions (currently lacking information needed to take weighted averages)


# do this using reshape
cols<-colnames(vax.data)
w<-length(cols)
rows<-rownames(vax.data)
l<-length(rows)

vax.data[,1]<-as.character(vax.data[,1])
rowWM<-match("WM", vax.data[,1])
rowEM<-match("EM", vax.data[,1])
rowYH<-match("YH", vax.data[,1])
rowNE<-match("NE", vax.data[,1])
rowLN<-match("LN", vax.data[,1])
rowNW<-match("NW", vax.data[,1])
rowSW<-match("SW", vax.data[,1])
rowSE<-match("SE", vax.data[,1])
rowEE<-match("EE", vax.data[,1])


# add row for Midlands, averaging WM and EM
vax.data[l+1, 2:w]<- (vax.data[rowWM, 2:w]  + vax.data[rowEM, 2:w])/2  
vax.data[l+1,1] <- "Midlands"

# add row for North East and Yorkshire  averaging NE and YH
vax.data[l+2, 2:w]<- (vax.data[rowNE, 2:w]  + vax.data[rowYH, 2:w])/2  
vax.data[l+2,1] <- "North East and Yorkshire"

# rename others 
vax.data[rowLN,1]<-"London"
vax.data[rowNW,1]<-"North West"
vax.data[rowSW,1]<-"South West"
vax.data[rowSE,1]<-"South East"
vax.data[rowEE,1]<-"East of England"

region.names<-c("North West","North East and Yorkshire","South West","South East","London","East of England","Midlands")
#short.region.names<-c("NW","NE","SW","SE","LN","EE", "WM" ,"EM" ,"YH")

# then drop rows from vax.data which are no in region.names

sel<-vax.data$region %in% region.names
vax.data<-vax.data[sel,]

vdata<- reshape(vax.data, varying= cols[2:w], direction= "long", idvar="region", sep="")


# now give vax coverage by week and standardise region names

# in sitrep week is calculated as

#days_since_Jan12020<-as.integer(sitrep$date - date("2020-01-01"))

# now calc week number so days 1to7 are week 1, days 8-14 are week 2 etc
#sitrep$week<-days_since_Jan12020 %/% 7
vdata$date<-as.Date(vdata$time, "%d.%m.%Y")
vdata$week <- as.integer(vdata$date - date("2020-01-01")) %/% 7
vdata$weekplus2 <- vdata$week +2  #  cumulative proportion of people vaccinated two weeks ago 
vdata$weekplus3 <- vdata$week +3  # cumulative proportion of people vaccinated three weeks ago 


l<-  length(vdata$week)
l2<- length(sitrep_weekly$wk)
sitrep_weekly$hcw_vax<-NA  # cumulative proportion of hcws vaccinated by start of week
sitrep_weekly$hcw_vax[sitrep_weekly$wk<48]<- 0 # since no vaccination before week 48

sitrep_weekly$hcw_vax_plus2<-NA  # cumulative proportion of hcws vaccinated 2 weeks ago by start of week
sitrep_weekly$hcw_vax_plus2[sitrep_weekly$wk<50]<- 0 # since no vaccination before week 48


sitrep_weekly$hcw_vax_plus3<-NA  # cumulative proportion of hcws vaccinated 3 weeks ago by start of week
sitrep_weekly$hcw_vax_plus3[sitrep_weekly$wk<51]<- 0 # since no vaccination before week 48


for(i in 1:l){
  wk<-vdata$week[i]
  reg<-vdata$region[i]
  sel <- sitrep_weekly$wk ==wk  & sitrep_weekly$region== reg
  selrows<-c(1:l2)[sel]
  sitrep_weekly$hcw_vax[sel] <- vdata$X[i]
  sitrep_weekly$hcw_vax[ sitrep_weekly$wk %in% c(58,59)] <-NA   # currently no data 
  
  
  wk<-vdata$weekplus2[i]  
  sel <- sitrep_weekly$wk ==wk  & sitrep_weekly$region== reg
  selrows<-c(1:l2)[sel]
  sitrep_weekly$hcw_vax_plus2[sel] <- vdata$X[i]
  
  wk<-vdata$weekplus3[i]  
  sel <- sitrep_weekly$wk ==wk  & sitrep_weekly$region== reg
  selrows<-c(1:l2)[sel]
  sitrep_weekly$hcw_vax_plus3[sel] <- vdata$X[i]
  
}



