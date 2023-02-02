
#  Figure 2

#  Merge in ERIC data to the weekly sitrep data

# Add to sitrep_weekly df data  on 

# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# 2. Hospital crowding: % general and acute days occupied  
# 3. Hospital building age score
# 4. Size of hospital.   


# 1. Number of single rooms beds per trust as a percentage of general and acute beds. 
# Get single bed data from ERIC - 201920 - SiteData.csv   but first filter by including only sites where "Site Type"" includes the word "hospital"
# 3 columns to include 
# BI "Single bedrooms for patients with en-suite facilities (No.)"
# BJ "Single bedrooms for patients without en-suite facilities (No.)"
# BK "Isolation rooms"
# 
# Add up these and divide by number acute and general bed days available per trust for third quarter of 2020. 
# This is column H "Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx"
# 

# library(readr)
# require(PostcodesioR)
# require(remotes)
#remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
# ggvoronoi  can be installed from github as above

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
include<-!(ERIC_data$`Trust Code` %in% c("RP4" , "RBS", "RCU","R0A90", "RRKE6"))
ERIC_data<-ERIC_data[include,]
# These excludes these children-only hospitals   
# "GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST" 
# "ALDER HEY CHILDREN'S NHS FOUNDATION TRUST" 
# “SHEFFIELD CHILDREN'S NHS FOUNDATION TRUST"     
#  aslo exclude the 2 nightingale hospitals "R0A90", "RRKE6"
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
newtrustdata$proportion_single_rooms<-newtrustdata$singlerooms/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$proportion_single_rooms_std<-(newtrustdata$proportion_single_rooms-mean(newtrustdata$proportion_single_rooms,na.rm=T))/sd(newtrustdata$proportion_single_rooms,na.rm=T)
newtrustdata$volume<-volume.by.trust[sel]
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


# new get lat and long for trust from postcode

sel<-match(newtrustdata$OrgCode,trustpostcode[,1])
newtrustdata$postcode<-trustpostcode[sel,2]
# note that in data supplied post-code for Sunderland Royal Hospital was incorrectly given as
# "SR7 4TP". It should have been "SR4 7TP" Change by hand
SRHnum<-match("SR7 4TP", newtrustdata$postcode)
newtrustdata$postcode[SRHnum] <- "SR4 7TP"

newtrustdata$longitude<-NA
newtrustdata$latitude<-NA
for(i in 1:length(newtrustdata$postcode)){
  if(!is.na(newtrustdata$postcode[i])){
    temp<-postcode_lookup(newtrustdata$postcode[i])
    newtrustdata$longitude[i]<-temp$longitude
    newtrustdata$latitude[i]<-temp$latitude
  }  
}

# Merge into newtrustdata all the other columns to plot
# A. cumulative day 15 cases 

sitrep_time_series <- readRDS("sitreps_eng_expanded.rds")
day15totals<-tapply(sitrep_time_series$n_inpatients_diagnosed_15_,sitrep_time_series$org_code,sum,na.rm=T)
pos<-match(newtrustdata$OrgCode, names(day15totals))
newtrustdata$day15totals<-day15totals[pos]
day8to14totals<-tapply(sitrep_time_series$n_inpatients_diagnosed_8_14,sitrep_time_series$org_code,sum,na.rm=T)
pos<-match(newtrustdata$OrgCode, names(day8to14totals))
newtrustdata$day8to14totals<-day8to14totals[pos]

newtrustdata$day15std <- newtrustdata$day15totals/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$day15std<-(newtrustdata$day15std - mean(newtrustdata$day15std,na.rm=T))/sd(newtrustdata$day15std,na.rm = T)

newtrustdata$day8to14std<- newtrustdata$day8to14totals/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$day8to14std<-(newtrustdata$day8to14std - mean(newtrustdata$day8to14std,na.rm=T))/sd(newtrustdata$day8to14std,na.rm = T)

# now add cumulative staff cases per acute bed
# but only include weeks 38 to  55 (not recorded reliably outside this period)
selweeks<-sitrep_weekly$wk>37 & sitrep_weekly$wk<55
cumul_hcw_cases<-tapply(sitrep_weekly$mean_staff_covid_isolated[selweeks], sitrep_weekly$org[selweeks], sum, na.rm=F)
pos<-match(newtrustdata$OrgCode, names(cumul_hcw_cases))
newtrustdata$cumul_hcw_cases<-cumul_hcw_cases[pos]

# replace 0s with NA 
temp<-newtrustdata$cumul_hcw_cases==0
newtrustdata$cumul_hcw_cases[temp]<-NA

newtrustdata$hcw_casesstd <- newtrustdata$cumul_hcw_cases/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$hcw_casesstd<-(newtrustdata$hcw_casesstd-mean(newtrustdata$hcw_casesstd,na.rm=T))/sd(newtrustdata$hcw_casesstd,na.rm=T)

# now add cumulative community acquired cases in patients
cumul_ca_cases<-tapply(sitrep_weekly$ca_cases, sitrep_weekly$org, sum, na.rm=T)
pos<-match(newtrustdata$OrgCode, names(cumul_ca_cases))
newtrustdata$cumul_ca_cases<-cumul_ca_cases[pos]
newtrustdata$cumul_ca_cases_std <- newtrustdata$cumul_ca_cases/newtrustdata$GeneralAcuteBedsAvailable
newtrustdata$cumul_ca_cases_std<-(newtrustdata$cumul_ca_cases_std-mean(newtrustdata$cumul_ca_cases_std,na.rm=T))/sd(newtrustdata$cumul_ca_cases_std,na.rm=T)

#add some jitter and remove those with missing latidude from plot
newtrustdata2<-newtrustdata
missing<-is.na(newtrustdata2$latitude)
newtrustdata2<-newtrustdata2[!missing,]
newtrustdata2$latitude<-newtrustdata2$latitude + rnorm(1,0,0.0001)
newtrustdata2$longitude<-newtrustdata2$longitude + rnorm(1,0,0.0001)
longpos<- match("longitude",names(newtrustdata2))
latpos<- match("latitude",names(newtrustdata2))

names(newtrustdata2)[longpos]<-"x"
names(newtrustdata2)[latpos]<-"y"



# start with voronoi tesselation (can use geogrid to create a hexagon grid from this later)
# require(ggplot2)
# require(ggvoronoi)
# require(rgdal)
# require(maps)
# require(tidyverse)
# require(rgeos)
# require(maptools)
# require(sp)
# require(gtable)
# require(gridExtra)
# require(inlmisc)
# require(geogrid)
# require(sf)
# require(tmap)
# require(broom)
# require(cartogram)



shp<-readOGR("Countries_(December_2018)_Boundaries_GB_BUC.shp") # shape file from https://data.gov.uk/dataset/ca487e92-5414-4606-b01c-f841ed326690/countries-december-2018-boundaries-gb-buc
shpLL = spTransform(shp, "+init=epsg:4326")  # needed to transform to standard co-corinante system
shp<-fortify(shpLL)
par(mfrow=c(3,3))

sel<-shp$id==0
shp0<-shp[sel,]
shp1<-shp0[shp0$piece==1 ,]

v_spdf<-voronoi_polygon(data=newtrustdata2, x="x", y="y",outline=shp1)

hexseed<-32
new_cells <- calculate_grid(shape = v_spdf,learning_rate=0.05,
                            grid_type = "hexagonal", seed = hexseed)
original_shapes<-v_spdf
grid_shapes <- assign_polygons(original_shapes, new_cells)

grid_shapes@data$id<-grid_shapes@data$OrgCode

var_df2<-fortify(grid_shapes, region= "id")

temp<-original_shapes
attributes(temp)$proj4string<-CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
temp2<-cartogram_dorling(temp, "TotalBedsAvailable", m_weight = 0.05,k=1)
# plot(temp2)  # best so far
# plot(temp2, col=r)
var_df2<-fortify(temp2, region= "OrgCode")

# merge in total numbers of staff

truststaff <- read_csv("HCHS staff in NHS Trusts and CCGs in England, Organisation and Job Type.csv")

staff.totals<-tapply(truststaff$FTE, truststaff$`Org code`, "sum" )
sel<-match(var_df2$id,names(staff.totals))
var_df2$totalstaff<-staff.totals[sel]

# merge in total number of admissions in period

tempadms<-read.csv("AllAdmissions by day and trust.csv")
totaladms<-tapply(tempadms$n,INDEX =tempadms$Procode, FUN = "sum")
sel<-match(var_df2$id,names(totaladms))
var_df2$totaladms<-totaladms[sel]
var_df2$trustnum<-match(var_df2$id,newtrustdata2$OrgCode)
var_df2$TotalBedsAvailable <- newtrustdata2$TotalBedsAvailable[var_df2$trustnum]
var_df2$GeneralAcuteBedsAvailable<-newtrustdata2$GeneralAcuteBedsAvailable[var_df2$trustnum]


var_df2$day15std <-newtrustdata2$day15std[var_df2$trustnum]
var_df2$day15totals<-newtrustdata2$day15totals[var_df2$trustnum]
var_df2$day15per1000adms<-var_df2$day15totals*1000/var_df2$totaladms
var_df2$day15per100beds<-var_df2$day15totals*100/var_df2$GeneralAcuteBedsAvailable

# problem with above is that there are three trusts with missing admission date RET, RNN, RVY
newtrustdata$GeneralAcuteBedsAvailable

var_df2$day8to14std <-newtrustdata2$day8to14std[var_df2$trustnum]
var_df2$day8to14totals<-newtrustdata2$day8to14totals[var_df2$trustnum]
var_df2$day8to14per1000adms<-var_df2$day8to14totals*1000/var_df2$totaladms
var_df2$day8to14per100beds<-var_df2$day8to14totals*100/var_df2$GeneralAcuteBedsAvailable

var_df2$hcw_casesstd <-newtrustdata2$hcw_casesstd[var_df2$trustnum]

var_df2$cumul_hcw_cases<-newtrustdata2$cumul_hcw_cases[var_df2$trustnum]
var_df2$hcw_casesper100staff<- var_df2$cumul_hcw_cases*100/var_df2$totalstaff
var_df2$hcw_casesper100beds<- var_df2$cumul_hcw_cases*100/var_df2$GeneralAcuteBedsAvailable
var_df2$cumul_ca_cases_std <- newtrustdata2$cumul_ca_cases_std[var_df2$trustnum]
var_df2$cumul_ca_cases <- newtrustdata2$cumul_ca_cases[var_df2$trustnum]
var_df2$cumul_ca_casesper100beds<-var_df2$cumul_ca_cases*100/var_df2$GeneralAcuteBedsAvailable

var_df2$occupancy_std <- newtrustdata2$occupancy_std[var_df2$trustnum]
var_df2$occupancy <- 100*newtrustdata2$GeneralAcuteBedsPerCentOccupied[var_df2$trustnum]


var_df2$percent_pre1965.std <- newtrustdata2$percent_pre1965.std[var_df2$trustnum]
var_df2$percent_pre1965 <- newtrustdata2$percent_pre1965[var_df2$trustnum]

var_df2$proportion_single_rooms_std <- newtrustdata2$proportion_single_rooms_std[var_df2$trustnum]
var_df2$proportion_single_rooms <- 100* pmin(newtrustdata2$proportion_single_rooms[var_df2$trustnum],1)


var_df2$trustsize.std <- newtrustdata2$trustsize.std[var_df2$trustnum]
var_df2$volume_per_bed_std<-newtrustdata2$volume_per_bed_std[var_df2$trustnum]
var_df2$volume_per_bed<-newtrustdata2$volume_per_bed[var_df2$trustnum]


var_df2$vaccHCWwk52 <-newtrustdata2$vaccHCWwk52[var_df2$trustnum]
var_df2$vaccHCWwk59 <-newtrustdata2$vaccHCWwk59[var_df2$trustnum]


brk<-c(0:20)/20
bl<-as.character(brk)
# cols = rev(heat.colors(20))
cols <- GetColors(n = 10, scheme = "smooth rainbow", alpha = NULL, 
                  start = 0, end = 1, bias = 1, reverse = TRUE, blind = NULL, 
                  gray = FALSE)




# 15 day
p.day15<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=day15std),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Hospital-associated infections") +
  # scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  scale_fill_gradient(low="pink",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.day15.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=day15std,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Hospital-associated infections") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.day15.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=day15per100beds,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="Per 100 beds") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Definite hospital-associated infections") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# 8-14 day
p.day8to14<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=day8to14std),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Probable hospital-associated infections") +
  # scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  scale_fill_gradient(low="pink",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.day8to14.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=day8to14std,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Probable hospital-associated infections") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.day8to14.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=day8to14per100beds,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="Per 100 beds") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Probable hospital-associated infections") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# HCW
p.HCW<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=hcw_casesstd),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Healthcare worker cases") +
  # scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  scale_fill_gradient(low="pink",high="black",guide=F) +
  theme_void() +
  coord_fixed()


p.HCW.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=hcw_casesstd,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Healthcare worker cases") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.HCW.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=hcw_casesper100beds,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="Per 100 beds") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Healthcare worker cases") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.HCW.new2a<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=hcw_casesper100staff,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Healthcare worker cases") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()


# community-acquired admissions
p.ca<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=cumul_ca_cases_std),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Community-onset infections") +
  # scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  scale_fill_gradient(low="pink",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.ca.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=cumul_ca_cases_std,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Community-acquired admissions") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.ca.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=cumul_ca_casesper100beds,group=group)) +
  scale_fill_gradient(low="pink",high="black",guide="colourbar",name="Per 100 beds") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Community-acquired admissions") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()


# occupancy
p.occ<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=occupancy_std),outline=shp0,
               color="steelblue2",size=.0125) +
  ggtitle("Bed occupancy") +
  scale_fill_gradient(low="steelblue2",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.occ.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=occupancy_std,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Bed occupancy") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.occ.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=occupancy,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="%") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Bed occupancy") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# building age
p.age<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=percent_pre1965.std),outline=shp0,
               color="steelblue2",size=.0125) +
  ggtitle("Hospital age") +
  scale_fill_gradient(low="steelblue2",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.age.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=percent_pre1965.std,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Hospital age") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.age.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=percent_pre1965,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="% pre-1965") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Hospital age") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# single rooms
p.single <-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=proportion_single_rooms_std),outline=shp0,
               color="steelblue2",size=.0125) +
  ggtitle("Single rooms") +
  scale_fill_gradient(low="steelblue2",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.single.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=proportion_single_rooms_std,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Single rooms") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.single.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=proportion_single_rooms,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="%") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Single rooms") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# size
p.size<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=trustsize.std),outline=shp0,
               color="steelblue2",size=.0125) +
  ggtitle("Number of acute beds") +
  scale_fill_gradient(low="steelblue2",high="black",guide=F) +
  theme_void() +
  coord_fixed()


p.size.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=trustsize.std,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Number of acute beds") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# volume
p.volume<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=volume_per_bed_std),outline=shp0,
               color="steelblue2",size=.0125) +
  ggtitle("Heated volume per bed") +
  scale_fill_gradient(low="steelblue2",high="black",guide=F) +
  theme_void() +
  coord_fixed()

p.volume.new<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=volume_per_bed_std,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name="") +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Heated volume per bed") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

p.volume.new2<-ggplot(var_df2) +
  geom_polygon(aes(x=long,y=lat,fill=volume_per_bed,group=group)) +
  scale_fill_gradient(low="steelblue2",high="black",guide="colourbar",name=expression(~m^3)) +
  #  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], name="", guide = "colourbar", aesthetics = "colour") +
  theme_void() +
  ggtitle("Heated volume per bed") +
  #  guides(colour = guide_colourbar(barheight = unit(10, "cm"))) +
  coord_fixed()

# Vaccinated HCWs week 52
sel<-sitrep_weekly$wk==52
temp<- tapply(sitrep_weekly$hcw_vax_plus2[sel], sitrep_weekly$org[sel], mean, na.rm=T)
pos<-match(newtrustdata$OrgCode, names(temp))
newtrustdata$vaccHCWwk52<-temp[pos]

brk<-c(0:20)/20
bl<-as.character(brk)
# cols = rev(heat.colors(20))
cols <- GetColors(n = 10, scheme = "smooth rainbow", alpha = NULL, 
                  start = 0, end = 1, bias = 1, reverse = FALSE, blind = NULL, 
                  gray = FALSE)
sel<-sitrep_weekly$wk==59
p.vacc52<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=vaccHCWwk52),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Immunised HCWs, week 52") +
  # scale_fill_gradient(low="#4dffb8",high="black",guide=F) +
  #  scale_fill_gradientn(colours=cols[1:4],breaks=brk[1:5], guide = "legend",name=NULL) +
  scale_fill_gradientn(colours=cols[1:4],breaks=brk[1:5], guide = "colourbar",name=NULL) +
  
  theme_void() +
  coord_fixed()

# Vaccinated HCWs week 59

sel<-sitrep_weekly$wk==59
temp<- tapply(sitrep_weekly$hcw_vax_plus2[sel], sitrep_weekly$org[sel], mean, na.rm=T)
pos<-match(newtrustdata$OrgCode, names(temp))
newtrustdata$vaccHCWwk59<-temp[pos]
p.vacc59<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=vaccHCWwk59),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Immunised HCWs, week 59") +
  #  scale_fill_gradient(low="#4dffb8",high="black",guide=F,breaks=brk) +
  scale_fill_gradientn(colours=cols[7:10],breaks=brk[17:20], guide = "colourbar",name=NULL) +
  theme_void() +
  coord_fixed()

# B117 Prevalence week 42
cols<-GetColors(  20, scheme = "iridescent")
brk<-c(0:20)/20
sel<-sitrep_weekly$wk==42
temp<- tapply(sitrep_weekly$prop_nv[sel], sitrep_weekly$org[sel], mean, na.rm=T)
pos<-match(newtrustdata$OrgCode, names(temp))
newtrustdata$B117wk42<-temp[pos]

p.B117wk42<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=B117wk42),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Alpha prevalence, week 42") +
  scale_fill_gradientn(colours=cols[1:4],breaks=brk[1:5], guide = "colourbar",name=NULL) +
  
  theme_void() +
  coord_fixed()

# B117 Prevalence week 52
sel<-sitrep_weekly$wk==52
temp<- tapply(sitrep_weekly$prop_nv[sel], sitrep_weekly$org[sel], mean, na.rm=T)
pos<-match(newtrustdata$OrgCode, names(temp))
newtrustdata$B117wk52<-temp[pos]
p.B117wk52<-ggplot(newtrustdata,aes(longitude,latitude)) +
  geom_voronoi(aes(fill=B117wk52),outline=shp0,
               color="#4dffb8",size=.0125) +
  ggtitle("Alpha prevalence, week 52") +
  scale_fill_gradientn(colours=cols[14:20],breaks=brk[14:20], guide = "colourbar",name=NULL) +
  #scale_fill_gradientn(low="#4dffb8",high="black",guide=F) +
  theme_void() +
  coord_fixed()


# voronoi area  plots

grid.arrange(p.day15, p.day8to14, p.HCW, p.ca,
             p.occ, p.age, p.single,p.size, 
             p.vacc52, p.vacc59, p.B117wk42, p.B117wk52,
             nrow = 3)
# dorling  plots
grid.arrange(p.day15.new, p.day8to14.new, p.HCW.new, p.ca.new,
             p.occ.new, p.age.new, p.single.new, p.size.new, 
             p.vacc52, p.vacc59, p.B117wk42, p.B117wk52,
             nrow = 3)
# now adding heated volume
grid.arrange(p.day15.new, p.day8to14.new, p.HCW.new, p.ca.new,
             p.occ.new, p.age.new, p.single.new, p.volume.new, 
             p.vacc52, p.vacc59, p.B117wk42, p.B117wk52,
             nrow = 3)
# now with absolute values rather than standardized
grid.arrange(p.day15.new2, p.day8to14.new2, p.HCW.new2, p.ca.new2,
             p.occ.new2, p.age.new2, p.single.new2, p.volume.new2, 
             p.vacc52, p.vacc59, p.B117wk42, p.B117wk52,
             nrow = 3)
# Merge in region into var_df2 and export for additional plotting
sel<-match(var_df2$id,sitrep_weekly$org )
var_df2$region<-sitrep_weekly$region[sel]
saveRDS(var_df2,file = "var_df2.RDS")

