#  plots of unmitigated epidemcs for supp mat


source("covid ode hospital model unmitigated.R")
# above creates out.df and does some base R graphics plots
require(gtable)
require(ggplot2)
require(gridExtra)
require(bayesplot)
require(hrbrthemes)
require(viridis)


out.df$infH<- out.df$E1_H + out.df$E2_H + out.df$I1_H + out.df$I2_H #hospital acquired cases 
out.df$infHCW<- out.df$E1_HCW + out.df$E2_HCW + out.df$I1_HCW + out.df$I2_HCW
out.df$infC<-out.df$E1_C + out.df$E2_C + out.df$I1_C + out.df$I2_C

# now fields for parameters 2 and 3 which reduce all nosocomial transmission
out.df$Iprimed_Hp2<- out2.df$Iprimed_H  #hospital acquired cases 
out.df$infHp2<- out2.df$E1_H + out2.df$E2_H + out2.df$I1_H + out2.df$I2_H #hospital acquired cases 
out.df$infHCWp2<- out2.df$E1_HCW + out2.df$E2_HCW + out2.df$I1_HCW + out2.df$I2_HCW
out.df$infCp2<-out2.df$E1_C + out2.df$E2_C + out2.df$I1_C + out2.df$I2_C

out.df$Iprimed_Hp3<- out3.df$Iprimed_H  #hospital acquired cases 
out.df$infHp3<- out3.df$E1_H + out3.df$E2_H + out3.df$I1_H + out3.df$I2_H #hospital acquired cases 
out.df$infHCWp3<- out3.df$E1_HCW + out3.df$E2_HCW + out3.df$I1_HCW + out3.df$I2_HCW
out.df$infCp3<-out3.df$E1_C + out3.df$E2_C + out3.df$I1_C + out3.df$I2_C

labelscaling<-1.5

ymax1<- max(out.df$Iprimed_H)
ymax1a<- max(out.df$infH)
ymax1<-max(ymax1, ymax1a)
ymax2<- max(out.df$infHCW)
ymax3<- max(out.df$infC)
cp1<- parameters["changepoint1"]
cp2<-parameters["changepoint2"]
cp3<-parameters["changepoint3"]
cp4<-parameters["changepoint4"]



pA1<-ggplot(out.df, aes(time)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line(aes(y = Iprimed_H, colour = "Community-acquired")) +
  geom_line(aes(y = infH, colour = "Hospital-acquired")) +
  ylim(0,ymax1) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected hospitalised patients") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90), legend.title =element_blank() ,legend.position = "none")

pA2<-ggplot(out.df, aes(time,infHCW)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax2) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected healthcare workers") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90))

pA3<-ggplot(out.df, aes(time,infC)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax3) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Community infected") +
  labs(x= "Day") +
  #  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90))

pA1.2<-ggplot(out.df, aes(time)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line(aes(y = Iprimed_Hp2, colour = "Community-acquired")) +
  geom_line(aes(y = infHp2, colour = "Hospital-acquired")) +
  ylim(0,ymax1) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected hospitalised patients") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank(),legend.title =element_blank() ,legend.position = "none")

pA2.2<-ggplot(out.df, aes(time,infHCWp2)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax2) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected healthcare workers") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank())

pA3.2<-ggplot(out.df, aes(time,infCp2)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax3) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Community infected") +
  labs(x= "Day") +
  #  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank())

pA1.3<-ggplot(out.df, aes(time)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line(aes(y = Iprimed_Hp3, colour = "Community-acquired")) +
  geom_line(aes(y = infHp3, colour = "Hospital-acquired")) +
  ylim(0,ymax1) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected hospitalised patients") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank(),  legend.title =element_blank(),legend.position = c(0.8,0.9))

# theme(axis.title.y = element_text(size = rel(labelscaling), angle = 90), legend.title =element_blank(),legend.position = "none")

pA2.3<-ggplot(out.df, aes(time,infHCWp3)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax2) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Infected healthcare workers") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank())

pA3.3<-ggplot(out.df, aes(time,infCp3)) +
  # geom_vline(xintercept = cp1, color = "grey", size=0.5) +
  # geom_vline(xintercept = cp2, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp3, color = "grey", size=0.5,linetype="dashed") +
  # geom_vline(xintercept = cp4, color = "grey", size=0.5) +
  geom_line() +
  ylim(0,ymax3) +
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(y= "Community infected") +
  labs(x= "Day") +
  #  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank())


grid.arrange(pA1,pA1.2,pA1.3, pA2,pA2.2,pA2.3, pA3, pA3.2,pA3.3,nrow = 3)


