#  Plots of cross correlation
# 
# i) for each trust consider time series of weekly infections in HCWs and new infections in patients
# ii) Pre-whiten data using TSA package. prewhiten function
# iii) Calculate ccf in a sliding winder for lags +/- 5 weeks for each trust
# iv) plot mean cross-correlation for each of 10 lags for 3 pairs of interactins
#    - ca-acquired [just used admitted] , hcw infections and patient infections
# Should look something like this 
# https://towardsdatascience.com/four-ways-to-quantify-synchrony-between-time-series-data-b99136c4a9c9 
# except put time on the y-axis and correlations on the x-axis
require(TSA)

trusts<-unique(sitrep_noso_covid_data4$trustindex)
lagmax<-4
m<-2*lagmax +1
n<-length(trusts)
m1.x1y<-matrix(NA, nrow=n,ncol=m)
m2.x1z<-matrix(NA, nrow=n,ncol=m)

m1.x2y<-matrix(NA, nrow=n,ncol=m)
m2.x2z<-matrix(NA, nrow=n,ncol=m)

m1.x3y<-matrix(NA, nrow=n,ncol=m)
m2.x3z<-matrix(NA, nrow=n,ncol=m)

m3.yz<-matrix(NA, nrow=n,ncol=m)
maxweek<-18
i<-1
for(i in 1:n){
  print(i)
  trustid<-trusts[i]
  
  sel<-sitrep_noso_covid_data4$trustindex==trustid & sitrep_noso_covid_data4$weekindex <=maxweek
  
  x1<-sitrep_noso_covid_data4$ha1[sel]
  x2<-sitrep_noso_covid_data4$ha2[sel]
  x3<-sitrep_noso_covid_data4$ha3[sel]
  y<-sitrep_noso_covid_data4$hcw[sel]
  z<-sitrep_noso_covid_data4$adm[sel]
  #  prewhiten
  if(sum(x3)>1 & sum(y)>1 & sum(z)>1){
    par(mfrow=c(2,3),mar=c(3,3,1,1))
    xcor<-prewhiten(x2,x2, lag.max=4)
    xcor<-prewhiten(y,y, lag.max=4)
    xcor<-prewhiten(z,z, lag.max=4)
    
    xcor<-prewhiten(x1,y, lag.max=4)
    m1.x1y[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(x2,y, lag.max=4)
    m1.x2y[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(x3,y, lag.max=4)
    m1.x3y[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(x1,z, lag.max=4)
    m2.x1z[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(x2,z, lag.max=4)
    m2.x2z[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(x3,z, lag.max=4)
    m2.x3z[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
    
    xcor<-prewhiten(y,z, lag.max=4)
    m3.yz[i,] <- t(as.matrix(xcor$ccf[-lagmax:lagmax][[1]]))
  }
}

countnotna<-function(x){
  sum(!is.na(x))
}


ccfdata_ha1<-data.frame(mean_ccf=c(meanccfx1y,meanccfx1z,meanccfxyz,
                               lag=-4:4,xy="Possible hospital acquired infection, HCW infections"))

meanccfx1y<-apply(m1.x1y,MARGIN = 2, mean, na.rm=T)
meanccfx1z<-apply(m2.x1z,MARGIN = 2, mean, na.rm=T)
meanccfyz<-apply(m3.yz,MARGIN = 2, mean, na.rm=T)


meanccfx2y<-apply(m1.x2y,MARGIN = 2, mean, na.rm=T)
meanccfx2z<-apply(m2.x2z,MARGIN = 2, mean, na.rm=T)
meanccfyz<-apply(m3.yz,MARGIN = 2, mean, na.rm=T)

meanccfx3y<-apply(m1.x3y,MARGIN = 2, mean, na.rm=T)
meanccfx3z<-apply(m2.x3z,MARGIN = 2, mean, na.rm=T)
meanccfyz<-apply(m3.yz,MARGIN = 2, mean, na.rm=T)



sdccfx1y<-apply(m1.x1y,MARGIN = 2, sd, na.rm=T)
sdccfx1z<-apply(m2.x1z,MARGIN = 2, sd, na.rm=T)

sdccfx2y<-apply(m1.x2y,MARGIN = 2, sd, na.rm=T)
sdccfx2z<-apply(m2.x2z,MARGIN = 2, sd, na.rm=T)

sdccfx3y<-apply(m1.x3y,MARGIN = 2, sd, na.rm=T)
sdccfx3z<-apply(m2.x3z,MARGIN = 2, sd, na.rm=T)

sdccfyz<-apply(m3.yz,MARGIN = 2, sd, na.rm=T)

nccfx1y<-apply(m1.x1y,MARGIN = 2, countnotna)
nccfx1z<-apply(m2.x1z,MARGIN = 2, countnotna)

nccfx2y<-apply(m1.x2y,MARGIN = 2, countnotna)
nccfx2z<-apply(m2.x2z,MARGIN = 2, countnotna)

nccfx3y<-apply(m1.x3y,MARGIN = 2, countnotna)
nccfx3z<-apply(m2.x3z,MARGIN = 2, countnotna)

nccfyz<-apply(m3.yz,MARGIN = 2, countnotna)

seccfx1y <- sdccfx1y/sqrt(nccfx1y)
seccfx1z <- sdccfx1z/sqrt(nccfx1z)

seccfx2y <- sdccfx2y/sqrt(nccfx2y)
seccfx2z <- sdccfx2z/sqrt(nccfx2z)

seccfx3y <- sdccfx3y/sqrt(nccfx3y)
seccfx3z <- sdccfx3z/sqrt(nccfx3z)
seccfyz <- sdccfyz/sqrt(nccfyz)

semultiplierfor95pc<-1.96

semultiplierfor90pc<-1.64
upper95ccfx1y <- meanccfx1y + semultiplierfor90pc* seccfx1y
lower95ccfx1y <- meanccfx1y - semultiplierfor90pc* seccfx1y

upper95ccfx2y <- meanccfx2y + semultiplierfor90pc* seccfx2y
lower95ccfx2y <- meanccfx2y - semultiplierfor90pc* seccfx2y

upper95ccfx3y <- meanccfx3y + semultiplierfor90pc* seccfx3y
lower95ccfx3y <- meanccfx3y - semultiplierfor90pc* seccfx3y

upper95ccfx1z <- meanccfx1z + semultiplierfor90pc* seccfx1z
lower95ccfx1z <- meanccfx1z - semultiplierfor90pc* seccfx1z

upper95ccfx2z <- meanccfx2z + semultiplierfor90pc* seccfx2z
lower95ccfx2z <- meanccfx2z - semultiplierfor90pc* seccfx2z

upper95ccfx3z <- meanccfx3z + semultiplierfor90pc* seccfx3z
lower95ccfx3z <- meanccfx3z - semultiplierfor90pc* seccfx3z

upper95ccfyz <- meanccfyz + semultiplierfor90pc* seccfyz
lower95ccfyz <- meanccfyz - semultiplierfor90pc* seccfyz

xy<-rep("Possible ha-infections v HCW infections",9)
xy<-c(xy, rep("Possible ha-infections v Covid-19 admissions",9))
xy<-c(xy, rep("HCW infections v Covid-19 admissions",9))

ccfdata_ha1<-data.frame(mean_ccf=c(meanccfx1y,meanccfx1z,meanccfyz),
                        lag=-4:4,variables=xy)

xy<-rep("Probable ha-infections v HCW infections",9)
xy<-c(xy, rep("Probable ha-infections v Covid-19 admissions",9))
xy<-c(xy, rep("HCW infections v Covid-19 admissions",9))

ccfdata_ha2<-data.frame(mean_ccf=c(meanccfx2y,meanccfx2z,meanccfyz),
                        lag=-4:4,variables=xy)

xy<-rep("Definite ha-infections v HCW infections",9)
xy<-c(xy, rep("Definite ha-infections v Covid-19 admissions",9))
xy<-c(xy, rep("HCW infections v Covid-19 admissions",9))
ccfdata_ha3<-data.frame(mean_ccf=c(meanccfx3y,meanccfx3z,meanccfyz),
                        lag=-4:4,variables=xy)
ccfdata_ha1$lower90<-c(lower95ccfx1y,lower95ccfx1z,lower95ccfyz)
ccfdata_ha1$upper90<-c(upper95ccfx1y,upper95ccfx1z,upper95ccfyz)
ccfdata_ha2$lower90<-c(lower95ccfx2y,lower95ccfx2z,lower95ccfyz)
ccfdata_ha2$upper90<-c(upper95ccfx2y,upper95ccfx2z,upper95ccfyz)
ccfdata_ha3$lower90<-c(lower95ccfx3y,lower95ccfx3z,lower95ccfyz)
ccfdata_ha3$upper90<-c(upper95ccfx3y,upper95ccfx3z,upper95ccfyz)

require(ggplot2)
require(hrbrthemes)
require(viridis)

labelscaling<-1.5
ggplot(ccfdata_ha1, 
      aes(x=lag, y=mean_ccf))+
      geom_line()+
      geom_ribbon(aes(ymin=lower90, ymax=upper90),fill = "grey80",alpha=0.5) +
      facet_wrap(vars(variables)) +
      geom_line() +
      labs(x="Lag (weeks)", y="Cross-correlation") +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = rel(labelscaling))) +
      theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90))
  
ggplot(ccfdata_ha2, 
       aes(x=lag, y=mean_ccf))+
  geom_line()+
  geom_ribbon(aes(ymin=lower90, ymax=upper90),fill = "grey80",alpha=0.5) +
  facet_wrap(vars(variables)) +
  geom_line() +
  labs(x="Lag (weeks)", y="Cross-correlation") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size = rel(labelscaling))) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90))

ggplot(ccfdata_ha3, 
       aes(x=lag, y=mean_ccf))+
  geom_line()+
  geom_ribbon(aes(ymin=lower90, ymax=upper90),fill = "grey80",alpha=0.5) +
  facet_wrap(vars(variables)) +
  geom_line() +
  labs(x="Lag (weeks)", y="Cross-correlation") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size = rel(labelscaling))) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90))




  
         
      
# 
# 
# 
# plot(upper95ccfx1y, ylim=c(-0.25,0.25),type="l",col="grey",main="ccf ha1 and hcw")
# lines(meanccfx1y,col="blue")
# lines(lower95ccfx1y,col="grey")
# 
# grid()
# 
# plot(upper95ccfx2y, ylim=c(-0.25,0.25),type="l",col="grey",main="ccf ha2 and hcw")
# lines(meanccfx2y,col="blue")
# lines(lower95ccfx2y,col="grey")
# 
# grid()
# 
# plot(upper95ccfx3y, ylim=c(-0.25,0.25),type="l",col="grey",main="ccf ha3 and hcw")
# lines(meanccfx3y,col="blue")
# lines(lower95ccfx3y,col="grey")
# 
# grid()
# 
# plot(upper95ccfx1z, ylim=c(-0.25,0.25),type="l",col="grey", main="ccf ha1 and covid admissions")
# lines(meanccfx1z,col="blue")
# lines(lower95ccfx1z,col="grey")
# grid()
# 
# plot(upper95ccfx2z, ylim=c(-0.25,0.25),type="l",col="grey", main="ccf ha2 and covid admissions")
# lines(meanccfx2z,col="blue")
# lines(lower95ccfx2z,col="grey")
# grid()
# 
# plot(upper95ccfx3z, ylim=c(-0.25,0.25),type="l",col="grey", main="ccf ha3 and covid admissions")
# lines(meanccfx3z,col="blue")
# lines(lower95ccfx3z,col="grey")
# grid()
# 
# 
# plot(upper95ccfyz, ylim=c(-0.25,0.25),type="l",col="grey", main="ccf hcw and covid admissions")
# lines(meanccfyz,col="blue")
# lines(lower95ccfyz,col="grey")
# grid()
# 
# 

