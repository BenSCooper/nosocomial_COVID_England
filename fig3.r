# This assumes all required models are in the workspace. 
#  layout(matrix(c(1,2,3,4,4,4), 2,3,byrow=T) )

require(gtable)
require(ggplot2)
require(gridExtra)
require(bayesplot)
require(hrbrthemes)
require(viridis)


# for panel plot

# panel S_A is estimates and intervals for patient acquisition...plot in three groups on different scales
# panel S_B is the same but for HCW infections
# panel C is posterior predictive distributions for patients 
# panel D is the same for HCWs

# panel S_A is estimates and intervals for patient acquisition 
pmod<-modelP3.ha2 #patient model to use

#pos<-match(c("a0","b0","c0","d0"), names(pmod))
pos<-match(c("ca_coeff","ha_coeff","hcw_coeff"), names(pmod))

names(pmod)[pos] <- c("SARS-CoV-2 admissions previous week","Hospital-associated SARS-CoV-2 previous week","HCW cases previous week")
pos<-match(c("nv0", "v0"), names(pmod))
names(pmod)[pos] <-c("Alpha","Vaccinated HCWs")
pos<-match(c("sr0","sizetrust0","occ0","agetrust0","vol"),  names(pmod))
names(pmod)[pos] <-c("Single rooms", "Trust size","Occupancy", "Trust age","Heated volume/bed")

#additive terms
parstoplot.1<-c("SARS-CoV-2 admissions previous week","Hospital-associated SARS-CoV-2 previous week","HCW cases previous week")
#multiplicative terms
parstoplot.2<-c("Single rooms", "Trust size","Occupancy", "Trust age","Heated volume/bed", "Alpha","Vaccinated HCWs" )

  
color_scheme_set("pink")
 pA<- mcmc_intervals(pmod, pars=c(parstoplot.1,parstoplot.2),
                     prob=0.5,prob_outer=.9, 
                     outer_size = 1.2, point_size = 3
 )
 
 pA.part1<- mcmc_intervals(pmod, pars=parstoplot.1,
                     prob=0.5,prob_outer=.9, 
                     outer_size = 1.2, point_size = 3
 )
 pA.part1<- pA.part1 +  theme_ipsum() 
 pA.part2<- mcmc_intervals(pmod, pars=parstoplot.2,
                           transformations = exp,
                           prob=0.5,prob_outer=.9, 
                           outer_size = 1.2, point_size = 3
 )
 
 pA.part2<- pA.part2 + scale_y_discrete(
   labels = c(`t(Single rooms)` = "Single rooms",
              `t(Trust size)` ="Size NHS Trust",
              `t(Alpha)` ="Alpha",
              `t(Trust age)` ="Age NHS Trust",
              `t(Heated volume/bed)` ="Heated volume/bed",
              `t(Occupancy)` ="Bed occupancy",
              `t(Vaccinated HCWs)` ="Vaccinated HCWs"))
 
 pA.part2<- pA.part2 +  theme_ipsum() 
 

 #  transforms<-list(`Single rooms`="exp", `Trust size`= "exp",`Occupancy`="exp", `Trust age`="exp", `B117`="exp", `Vaccinated HCWs` ="exp")
 
 
 # pA<- mcmc_intervals(pmod, pars=c(parstoplot.1,parstoplot.2),
 #                    transformations=transforms,
 #                     prob=0.5,prob_outer=.9, 
 #                     outer_size = 1.2, point_size = 3)


 # panel B is estimates and intervals for hcw acquisition (for supplmentary material)
 pmod<-modelH3.ha2 #patient model to use
 
 #pos<-match(c("a0","ca_coeff","ha_coeff","hcw_coeff"), names(pmod))
 
 
 pos<-match(c("ca_coeff","ha_coeff","hcw_coeff"), names(pmod))
 names(pmod)[pos] <- c("SARS-CoV-2 admissions previous week","Hospital-associated SARS-CoV-2 previous week","HCW cases previous week")
 pos<-match(c("nv0", "v0"), names(pmod))
 names(pmod)[pos] <-c("Alpha","Vaccinated HCWs")
 pos<-match(c("sr0","sizetrust0","occ0","agetrust0","vol"),  names(pmod))
 names(pmod)[pos] <-c("Single rooms", "Trust size","Occupancy", "Trust age","Heated volume/bed")
 
 #additive terms
 parstoplot.1<-c("SARS-CoV-2 admissions previous week","Hospital-associated SARS-CoV-2 previous week","HCW cases previous week")
 #multiplicative terms
 parstoplot.2<-c("Single rooms", "Trust size","Occupancy", "Trust age","Heated volume/bed", "Alpha","Vaccinated HCWs" )
 
 color_scheme_set("brightblue")
 # pB<- mcmc_intervals(pmod, pars=parstoplot,
 #                     prob=0.5,prob_outer=.90, 
 #                     outer_size = 1.2, point_size = 3,
 # )
 
 pB.part1<- mcmc_intervals(pmod, pars=parstoplot.1,
                           prob=0.5,prob_outer=.9, 
                           outer_size = 1.2, point_size = 3
 )
 pB.part1<- pB.part1 +  theme_ipsum() 
 pB.part2<- mcmc_intervals(pmod, pars=parstoplot.2,
                           transformations = exp,
                           prob=0.5,prob_outer=.9, 
                           outer_size = 1.2, point_size = 3
 )
 
 pB.part2<- pB.part2 + scale_y_discrete(
   labels = c(`t(Single rooms)` = "Single rooms",
              `t(Trust size)` ="Size NHS Trust",
              `t(Alpha)` ="Alpha",
              `t(Trust age)` ="Age NHS Trust",
              `t(Heated volume/bed)` ="Heated volume/bed",
              `t(Occupancy)` ="Bed occupancy",
              `t(Vaccinated HCWs)` ="Vaccinated HCWs"))
 
 pB.part2<- pB.part2 +  theme_ipsum() 
 

 
 pE<- plot_infection_detected_stacked_area(stan4data)
 pE<-pE + theme(plot.title = element_text(size=11))
 # panel B is posterior means of sources of new infections in patients
 pF<- plot_infection_source_stacked_area(modelP3.ha2,stan4data)
 pF<-pF + theme(plot.title = element_text(size=11))
 
 # panel G is posterior means of sources of new infections in HCWs
 pG<- plot_infection_source_stacked_area(modelH3.ha2,stan4data, figtitle="Predicted sources for HCW infections", figsubtitle="")
 pG<-pG + theme(plot.title = element_text(size=11))
                #  panel.grid.major = element_blank(),
                #  panel.grid.minor = element_blank())
 
 pE
 pF
 pG

 
 
# panel D is posterior predictive distributions for patients 
 #plot.for.largest.trusts<-TRUE
 #num.trusts.per.plot<-20
 plot.for.largest.trusts<-TRUE
 num.trusts.per.plot<-96
trusts<-unique(sitrep_noso_covid_data4$trustindex)
if(plot.for.largest.trusts){
  i<-match(trusts,sitrep_noso_covid_data4$trustindex)
  sizes<-sitrep_noso_covid_data4$trustsize_std[i]
  pos<-order(-sizes)# index numbers for trusts in decreasing size
  trusts.to.plot<-trusts[pos[1:num.trusts.per.plot]]
} else {
  trusts.to.plot<-trusts[1:num.trusts.per.plot]
}  
sel<-sitrep_noso_covid_data4$trustindex %in% trusts.to.plot
y<-sitrep_noso_covid_data4$ha2[sel]
week<-sitrep_noso_covid_data4$weekindex[sel] + 37 #add 37 to give week number
group<-sitrep_noso_covid_data4$trustindex[sel]
n<-length(y)
sel.obnums<-c(1:length(sitrep_noso_covid_data4$ha2))[sel]
varnames<-paste0("y_rep[",sel.obnums,"]")
yrep<-extract(modelP3.ha2,varnames )
m<-length(yrep[[1]])
y_rep_mat<-matrix(NA, nrow=m, ncol=n)
for(i in 1:n) y_rep_mat[,i] <-yrep[[i]]

color_scheme_set("pink")
pD<-ppc_ribbon_grouped(y, y_rep_mat, x = week, group) +
  ggplot2::scale_x_continuous(breaks = pretty) +
  theme(panel.spacing = unit(0.2, "lines"))

pD<-pD+legend_none() + xaxis_title(on = FALSE)

pD<- pD+ theme(
           strip.background = element_blank(),
           strip.text.x = element_blank()
       )

# panel E is posterior predictive distributions for HCWs 
y<-sitrep_noso_covid_data4$hcw[sel]
yrep<-extract(modelH3.ha2,varnames )
m<-length(yrep[[1]])
y_rep_mat<-matrix(NA, nrow=m, ncol=n)
for(i in 1:n) y_rep_mat[,i] <-yrep[[i]]
color_scheme_set("brightblue")
pE<-ppc_ribbon_grouped(y, y_rep_mat, x = week, group) +
  ggplot2::scale_x_continuous(breaks = pretty) +
  theme(panel.spacing = unit(0.2, "lines"))
pE<-pE+legend_none() + xaxis_title(on = FALSE)

pE<- pE+ theme(
  strip.background = element_blank(),
  strip.text.x = element_blank()
)


pA.part1
pA.part2
pB.part1
pB.part2

PC
pD
pE


