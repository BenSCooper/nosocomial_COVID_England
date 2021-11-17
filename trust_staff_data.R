#  Number of staff by trust 
truststaff <- read_csv("~/Dropbox/studies/covid19/nosocomial transmission stuff/sitrep/additional_data/HCHS staff in NHS Trusts and CCGs in England, Organisation and Job Type CSV.csv")

staff.totals<-tapply(truststaff$FTE, truststaff$`Org code`, "sum" )

staff_absent_covid19_totals<-tapply(sitrep_weekly$mean_staff_absent_covid_1, sitrep_weekly$org, "sum" ,na.rm=T)
#  above gives sum of days of staff isolated with covid19. Assume isolation is for 
# 10 days then below should give number of episodes of staff isolation 
staff_absent_covid19<-staff_absent_covid19_totals/10  # 


sort(staff_absent_covid19)
sort(staff.totals)


# now ratio of staff isolated with covid19 to total number of staff in trust 
trusts<-names(staff_absent_covid19)

# match(names(staff.totals),trusts)
# match(trusts,names(staff.totals))

pos<-match(trusts,names(staff.totals))
staff_by_trust<- staff.totals[pos]

proportion_infected<-staff_absent_covid19/staff_by_trust
