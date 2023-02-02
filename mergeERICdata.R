#  Merge in ERIC data to the weekly sitrep data

# Add to sitrep_weekly df data  on 

# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# 2. Hospital crowding: % general and acute days occupied  
# 3. Hospital building age score
# 4.Size of hospital.   


# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# Get single bed data from ERIC - 201920 - SiteData.csv   but first filter by including only sites where "Site Type"" includes the word "hospital"
# 3 columns to include 
# BI "Single bedrooms for patients with en-suite facilities (No.)"
# BJ "Single bedrooms for patients without en-suite facilities (No.)"
# BK "Isolation rooms"
# - done
# Add up these and divide by number acute and general bed days available per trust for third quarter of 2020. 
# This is column H "Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx"
# 
# Standardize this variable. 
# This is named (newtrustdata$proportion_single_rooms_std)

#library(readr)

#  Merge in ERIC data to the weekly sitrep data

# Add to sitrep_weekly df data  on 

# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# 2. Hospital crowding: % general and acute days occupied  
# 3. Hospital building age score
# 4.Size of hospital.   


# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# Get single bed data from ERIC - 201920 - SiteData.csv   but first filter by including only sites where "Site Type"" includes the word "hospital"
# 3 columns to include 
# BI "Single bedrooms for patients with en-suite facilities (No.)"
# BJ "Single bedrooms for patients without en-suite facilities (No.)"
# BK "Isolation rooms"
# - done
# Add up these and divide by number acute and general bed days available per trust for third quarter of 2020. 
# This is column H "Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx"
# 
# Standardize this variable. 
# This is named (newtrustdata$proportion_single_rooms_std)


# i) read in data  filter to include only rows where the site type is labelled as a hospital

ERIC_data <- read_csv("ERIC_201920_SiteData_for_R.csv")
ERIC_data<-as.data.frame(ERIC_data)
# Restrict to records where Site Type is  a hospital and where Trust Type is either ACUTE or COMMUNITY 
site.col<-match("Site Type", names(ERIC_data))
hosp_site<-grepl("hospital", ERIC_data[,site.col])
type.col<-match("Trust Type", names(ERIC_data))
acute_site<-grepl("ACUTE", ERIC_data[,type.col])
community_site<-grepl("COMMUNITY", ERIC_data[,type.col])
acute_or_community_site<-acute_site|community_site

ERIC_data<-ERIC_data[hosp_site & acute_site, ]
include<-!(ERIC_data$`Trust Code` %in% c("RP4" , "RBS", "RCU"))
ERIC_data<-ERIC_data[include,]
# These excludes these childre-only hospitals 
# "GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST" 
# "ALDER HEY CHILDREN'S NHS FOUNDATION TRUST" 
# “SHEFFIELD CHILDREN'S NHS FOUNDATION TRUST"     
ericorgcodes145<-unique(ERIC_data$`Trust Code`)

# i) Now find single room cols
single.room<-grepl("Single bedroom", names(ERIC_data))
isolation.room<-grepl("Isolation room", names(ERIC_data))
age<-grepl("Age profile", names(ERIC_data))
area<-grepl("Gross internal floor area", names(ERIC_data))
postcode<-grepl("Post Code", names(ERIC_data))
volume<-grepl("Site heated volume", names(ERIC_data))

cols.to.include<-single.room | isolation.room| age| area|postcode|volume
cols.to.include[1:7]<-TRUE

ERIC_data<-ERIC_data[,cols.to.include]
single.room<-grep("Single bedroom", names(ERIC_data))
isolation.room<-grep("Isolation room", names(ERIC_data))
ERIC_data$single_room_total<-as.numeric(ERIC_data[,single.room[1]])  +as.numeric(ERIC_data[,single.room[2]]) + as.numeric(ERIC_data[,isolation.room])

# choose postcode of largest site by Gross internal floor area
temp<-tapply(ERIC_data[,9],ERIC_data[,1],max)
sel<-match(ERIC_data[,1], names(temp))
ERIC_data$largestarea<- temp[sel]
ERIC_data$largestsitebyarea<-ERIC_data$largestarea==ERIC_data[,9]
trustpostcode<-ERIC_data[ERIC_data$largestsitebyarea, c(1,8)] #holds postcode of the largest site by area of given trust 


#  Now sum these by trust 
single.rooms.by.trust<- tapply(ERIC_data$single_room_total, ERIC_data[,1], sum)
volume.by.trust<- tapply(ERIC_data$`Site heated volume (m³)`, ERIC_data[,1], sum)

# now read in general and acute beds by trust, and divide by this and standardize
# library(readxl)
OccupiedBedsbyTrust <- as.data.frame(read_excel("GeneralAcuteOccupiedBedsbyTrust.xlsx", 
                                                col_types = c("text", "text", "text", 
                                                              "text", "text", "numeric", "numeric", 
                                                              "numeric", "numeric")))
newtrustdata<-OccupiedBedsbyTrust[OccupiedBedsbyTrust$GeneralAcuteBedsAvailable>=100,]  #exclude trusts with fewer than 100 acute beds
newtrustdata$singlerooms=NA
sel<-match(newtrustdata$OrgCode, names(single.rooms.by.trust))
newtrustdata$singlerooms<-single.rooms.by.trust[sel]
newtrustdata$volume<-volume.by.trust[sel]

newtrustdata$proportion_single_rooms<-newtrustdata$singlerooms/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$proportion_single_rooms_std<-(newtrustdata$proportion_single_rooms-mean(newtrustdata$proportion_single_rooms,na.rm=T))/sd(newtrustdata$proportion_single_rooms,na.rm=T)

newtrustdata$volume_per_bed<-newtrustdata$volume/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$volume_per_bed_std<-(newtrustdata$volume_per_bed-mean(newtrustdata$volume_per_bed,na.rm=T))/sd(newtrustdata$volume_per_bed,na.rm=T)


# 2. Hospital crowding: % general and acute days occupied  
newtrustdata$occupancy_std<- (newtrustdata$GeneralAcuteBedsPerCentOccupied - mean(newtrustdata$GeneralAcuteBedsPerCentOccupied))/sd(newtrustdata$GeneralAcuteBedsPerCentOccupied)


# 3. Hospital building age score 
# 
# 3. Hospital building age score. Use data from "ERIC - 201920 - SiteData.csv" for site, and 
# for each trusts take a weighted average from sites within trusts where "Site Type"" includes the word "hospital". For weights use column Z "Gross internal floor area (m²)".
#    
# Statistic to use is % of hospital from 1964 or earlier (i.e. over than 55 years old [age data only available in 10 year intervals])
# 
ERIC_data$percent_pre1965<-ERIC_data[,19] + ERIC_data[,20] + ERIC_data[,21]
# now weight by floor area (normalise later by diving by total floor area across sites)
ERIC_data$percent_pre1965.weighted<-ERIC_data$percent_pre1965*ERIC_data[,9]
trustfloorarea<-tapply(ERIC_data[,9], ERIC_data$`Trust Code`, sum)
trust.index<-match(ERIC_data$`Trust Code`,names(trustfloorarea))
ERIC_data$percent_pre1965.weighted2<-ERIC_data$percent_pre1965.weighted/trustfloorarea[trust.index]

percent_pre1965_bytrust<-tapply(ERIC_data$percent_pre1965.weighted2, ERIC_data$`Trust Code`, sum)
sel<-match(newtrustdata$OrgCode, names(percent_pre1965_bytrust))
newtrustdata$percent_pre1965<-percent_pre1965_bytrust[sel]
newtrustdata$percent_pre1965.std<-(newtrustdata$percent_pre1965-mean(newtrustdata$percent_pre1965,na.rm=T))/sd(newtrustdata$percent_pre1965,na.rm=T)


# Size of trust

newtrustdata$trustsize.std<-(newtrustdata$TotalBedsAvailable-mean(newtrustdata$TotalBedsAvailable))/sd(newtrustdata$TotalBedsAvailable)


#  Now merge new fields into sitrep_weekly

trust.index<-match(sitrep_weekly$org, newtrustdata$OrgCode)
# 1. single room data 
sitrep_weekly$proportion_single_rooms_std<-newtrustdata$proportion_single_rooms_std[trust.index]
# 2. Occupancy
sitrep_weekly$occupancy_std<-newtrustdata$occupancy_std[trust.index]
# 3. Building age
sitrep_weekly$percent_pre1965.std<-newtrustdata$percent_pre1965.std[trust.index]
# 4. Trust size
sitrep_weekly$trustsize.std<-newtrustdata$trustsize.std[trust.index]
# 5. volume per bed
sitrep_weekly$trustvolperbed.std<-newtrustdata$volume_per_bed_std[trust.index]


print("ERIC data has been merged")
 



