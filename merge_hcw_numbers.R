#  Now merge in number of staff at each trust (including non-patient-facing staff)

#  Number of staff by trust 
truststaff <- read_csv("HCHS staff in NHS Trusts and CCGs in England, Organisation and Job Type.csv")

staff.totals<-tapply(truststaff$FTE, truststaff$`Org code`, "sum" )
trust.index<-match(sitrep_weekly$org, names(staff.totals))
sitrep_weekly$num_hcws<-staff.totals[trust.index]
