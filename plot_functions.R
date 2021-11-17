#plotting functions for plotting aspects of stan model fits

plot_obs_v_fitted_by_trust<-function(stanobject, standata, nrows=10, figtitle=""){
  # plots a grid of nrows*nrows (one plot for each trust)
  # showing observed acquisitions by week and expected 
  N<-standata$N  # number of observations
  par(mfrow=c(nrows,nrows),mar=c(0,0,0,0),oma=c(3,3,3,3))
  for(i in 1:(nrows^2)){
    sel<-(1:N) [standata$trustindex==i]
    #print(sel)
    varnames<- paste("mu[",sel,"]",sep="")
    pred_means<-summary(stanobject, varnames)$summary[,1]
    weeks<-length(sel)
    plot(1:weeks,pred_means, type='l',ylim=c(0,100),xlab='',ylab='',xaxt='n',yaxt='n',col="green")
    points(1:weeks,standata$ha1[sel],col="steelblue")
    lines(1:weeks,pred_means,col="green")
  }
  axis(side=1,at=1:weeks)
  axis(side=4, at=c(0,100))
  mtext(figtitle, side=3,outer=T,line=1.5)
}

plot_obs_v_fitted_by_trust.hcw<-function(stanobject, standata, nrows=10, figtitle=""){
  # plots a grid of nrows*nrows (one plot for each trust)
  # showing observed acquisitions by week and expected 
  N<-standata$N  # number of observations
  par(mfrow=c(nrows,nrows),mar=c(0,0,0,0),oma=c(3,3,3,3))
  for(i in 1:(nrows^2)){
    sel<-(1:N) [standata$trustindex==i]
    #print(sel)
    varnames<- paste("mu[",sel,"]",sep="")
    pred_means<-summary(stanobject, varnames)$summary[,1]
    weeks<-length(sel)
    plot(1:weeks,pred_means, type='l',ylim=c(0,100),xlab='',ylab='',xaxt='n',yaxt='n',col="pink")
    points(1:weeks,standata$hcw[sel],col="steelblue")
    lines(1:weeks,pred_means,col="pink")
  }
  axis(side=1,at=1:weeks)
  axis(side=4, at=c(0,100))
  mtext(figtitle, side=3,outer=T,line=1.5)
}


plot_infection_detected_stacked_area<-function(standata,  figtitle="Detected infections ", figsubtitle="",wknums=42:55){
  require(hrbrthemes)
  require(viridis)
  N<-dim(standata)[1]  # number of observations
  sel<-standata$wk %in% wknums
  
  obs_ca<-standata$ca_cases[sel]
  obs_ha<-standata$ha_cases3[sel]
  obs_hcw<-standata$mean_staff_covid_isolated[sel]
  included_weeks<-standata$wk[sel]
  obs_ca_totals<-tapply(obs_ca,included_weeks, sum, na.rm="T")
  obs_ha_totals<-tapply(obs_ha,included_weeks, sum, na.rm="T")
  obs_hcw_totals<-tapply(obs_hcw,included_weeks, sum, na.rm="T")

  
  ca_df<-data.frame(week=  as.numeric(names(obs_hcw_totals)),  infections.detected=obs_ca_totals,source="Community-acquired cases")
  ha_df<-data.frame(week=  as.numeric(names(obs_hcw_totals)), infections.detected=obs_ha_totals,source="Hospital-acquired cases")
  hcw_df<-data.frame(week=as.numeric(names(obs_hcw_totals)), infections.detected=obs_hcw_totals,source="Healthcare workers")
  infections<-rbind(ca_df,ha_df,hcw_df)
  infections$source<-as.factor(infections$source)
  infections$week<-as.numeric(infections$week)
  
  P<-ggplot(infections, aes(x=week, y=infections.detected, fill=source)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    #    scale_fill_viridis(discrete = T) +
    scale_fill_ft() +
    theme_ipsum() + 
    #ggtitle("Predicted sources of new infections in patients")+
    labs(title=figtitle,
         subtitle=figsubtitle,
         caption="") +
    theme(legend.position = c(0.2, 0.9))
  #theme(legend.position="bottom")
  return(P) 
}


plot_infection_source_stacked_area<-function(stanobject, standata,  figtitle="Predicted sources for patient infections", figsubtitle=""){
  require(hrbrthemes)
  require(viridis)
  N<-dim(standata)[1]  # number of observations
  sel<-(1:N) 
  varnames1<- paste("ca_totals[",sel,"]",sep="")
  varnames2<- paste("ha_totals[",sel,"]",sep="")
  varnames3<- paste("hcw_totals[",sel,"]",sep="")
  pred_means1<-summary(stanobject, varnames1)$summary[,1]
  pred_means2<-summary(stanobject, varnames2)$summary[,1]
  pred_means3<-summary(stanobject, varnames3)$summary[,1]
  # now sum predicted means by date 
  pred_means_ca<-tapply(pred_means1,stan4data$wk, sum, na.rm=T)
  pred_means_ha<-tapply(pred_means2,stan4data$wk, sum, na.rm=T)
  pred_means_hcw<-tapply(pred_means3,stan4data$wk, sum, na.rm=T)
  
  ca_df<-data.frame(week=names(pred_means_ca), predicted.mean=pred_means_ca,source="Community-acquired cases")
  ha_df<-data.frame(week=names(pred_means_ha), predicted.mean=pred_means_ha,source="Hospital-acquired cases")
  hcw_df<-data.frame(week=names(pred_means_hcw), predicted.mean=pred_means_hcw,source="Healthcare workers")
  infection_sources<-rbind(ca_df,ha_df,hcw_df)
  infection_sources$source<-as.factor(infection_sources$source)
  infection_sources$week<-as.numeric(infection_sources$week)
  
  P<-ggplot(infection_sources, aes(x=week, y=predicted.mean, fill=source)) + 
    geom_area(alpha=0.6 , size=.5, colour="white",show.legend = FALSE) +
    #   scale_fill_viridis(discrete = T) +
      scale_fill_ft() +
    theme_ipsum() + 
    #ggtitle("Predicted sources of new infections in patients")+
    labs(title=figtitle,
         subtitle=figsubtitle,
         caption="") 
    #  theme(legend.position="bottom")
    return(P) 
}

plot_spline_component<-function(stanobject, standata, figtitle=""){
  wkindex<- unique(standata$weekindex)
  varnames<- paste("spline_component[",wkindex,"]",sep="")
  splinemeans<-exp(summary(stanobject, varnames)$summary[,1])
  plot(wkindex,splinemeans,type='l', xlab="Week index", ylab="Spline component",main=figtitle)
}
