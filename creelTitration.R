# CJD 9.22.23

# I'm going to start playing with building a titration of the creel data

rm(list=ls())
library(wdnr.fmdb)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(BayesianTools)
setwd("C:/Users/dassocju/OneDrive - State of Wisconsin/Office_of_Applied_Science/Creel_SFR")

# reading in DNR creel data

# cserv=get_creel_surveys()
# cvis=get_creel_visits()
# ccou=get_creel_counts()
# cint=get_creel_int_party()
# cfish=get_creel_fish_data()
# cfish.i=get_creel_fishlen_data()

# saving all these together since they take a long time to load
# saveRDS(list(cserv,cvis,ccou,cint,cfish,cfish.i),"creelDataSet_all.RData")

call=readRDS("creelDataSet_all.RData")
# unlist, etc. to get back to individual dfs
cserv=call[[1]]
cvis=call[[2]]
ccou=call[[3]]
cint=call[[4]]
cfish=call[[5]]
cfish.i=call[[6]]


crYear=cserv%>%
  group_by(year)%>%
  filter(wbic%in%ctwiWBIC.creel)%>%
  summarize(nsurvs=length(unique(survey.seq.no)))

ggplot(crYear)+
  geom_col(aes(x=year, y=nsurvs))
## playing with built in functions first

ceff=calc_creel_effort(creel_count_data=ccou,creel_int_data=cint) # don't use the grouping argument here, seems to break it

charv=calc_creel_harvest(creel_count_data = ccou,creel_int_data = cint,creel_fish_data = cfish) # don't use the grouping argument here, seems to break it

charvR=calc_creel_harvest_rates(cfish)

# 
# #empty dfs to hold output
# ttrEff=as.data.frame(matrix(NA,nrow=1,ncol=ncol(ceff)))
# colnames(ttrEff)=colnames(ceff)
# ttrEff$pReduc=NA
# 
# ttrHarv=as.data.frame(matrix(NA,nrow=1,ncol = ncol(charv)))
# colnames(ttrHarv)=colnames(charv)
# ttrHarv$pReduc=NA
# 
# ttrHarvR=as.data.frame(matrix(NA,nrow = 1,ncol = ncol(charvR)))
# colnames(ttrHarvR)=colnames(charvR)
# ttrHarvR$pReduc=NA
# 
# pRemove=seq(0.1,0.8, by=0.1) # percent of data to remove at each loop
# survs=unique(ccou$survey.seq.no) # unqiue surveys to remove a percentage of data from (corresponds to a unique lake-year that was creeled)
# 
# set.seed(2)
# for(r in 1:length(pRemove)){
#   for(i in 1:length(survs)){
#     
#     # reducing data size
#     full=ccou[ccou$survey.seq.no==survs[i],]
#     reduc=full[-c(runif(n=round(nrow(full)*pRemove[r]),min = 1,max=nrow(full))),]
#     
#     full.int=cint[cint$survey.seq.no==survs[i],]
#     reduc.int=full.int[-c(runif(n=round(nrow(full.int)*pRemove[r]),min = 1,max = nrow(full.int))),]
#     #removed.int=full.int[!(do.call(paste0,full.int)%in%do.call(paste0,reduc.int)),]
#     
#     full.harv=cfish[cfish$survey.seq.no==survs[i],]
#     reduc.harv=full.harv[full.harv$int.party.seq.no%in%reduc.int$int.party.seq.no,] # removing all fish data associated with the removed interview data
#     
#     #calculating creel stats
#     teff=calc_creel_effort(creel_count_data = reduc,creel_int_data = reduc.int)
#     tharv=calc_creel_harvest(creel_count_data = reduc,creel_int_data = reduc.int,creel_fish_data = reduc.harv)
#     tharvR=calc_creel_harvest_rates(reduc.harv)
#     
#     
#   # adding data to output dataframes
#     addEff=cbind(teff,pReduc=rep(pRemove[r],nrow(teff)));colnames(addEff)=colnames(ttrEff)
#     addHarv=cbind(tharv,pReduc=rep(pRemove[r],nrow(tharv)));colnames(addHarv)=colnames(ttrHarv)
#     addHarvR=cbind(tharvR,pReduc=rep(pRemove[r],nrow(tharvR)));colnames(addHarvR)=colnames(ttrHarvR)
#     
#     ttrEff=rbind(ttrEff,addEff)
#     ttrHarv=rbind(ttrHarv,addHarv)
#     ttrHarvR=rbind(ttrHarvR,addHarvR)
#     
#   }
# }
# 
# saveRDS(list(ttrEff,ttrHarv,ttrHarvR),"titrationOutput.RData")

ttrList=readRDS("titrationOutput.RData")

ttrEff=ttrList[[1]];ttrEff=ttrEff[!is.na(ttrEff$wbic),]
ttrHarv=ttrList[[2]];ttrHarv=ttrHarv[!is.na(ttrHarv$wbic),]
ttrHarvR=ttrList[[3]];ttrHarvR=ttrHarvR[!is.na(ttrHarvR),]


# Effort titration 

# looking at just one lake to begin with so I can understand the data better

wbic=ttrEff[ttrEff$wbic==ttrEff$wbic[2] & !is.na(ttrEff$month),]
actual=ceff[ceff$wbic==unique(wbic$wbic) & !is.na(ceff$month),]
# ggplot(wbic)+theme_classic()+
#   geom_line(aes(x=pReduc,y=total.effort,color=daytype))+
#   geom_line(aes(x=pReduc,y=total.effort+total.effort.sd, color=daytype),linetype=2)+
#   geom_line(aes(x=pReduc,y=total.effort-total.effort.sd, color=daytype),linetype=2)+
#   facet_wrap(~month,scales = "free_y")+
#   labs(x="% of Data Removed",y="Total Monthly Effort +/- 1 SD")

ggplot(wbic[wbic$year==2006,])+theme_classic()+
  geom_pointrange(aes(x=pReduc,y=total.effort,ymax=total.effort+total.effort.sd,ymin=total.effort-total.effort.sd, color=daytype))+
  geom_line(aes(x=pReduc,y=total.effort,color=daytype))+
  geom_hline(data=actual,aes(yintercept = total.effort, color=daytype))+
  facet_wrap(~month,scales = "free_y")+
  labs(x="% of Data Removed",y="Total Monthly Effort +/- 1 SD")

ggplot(wbic)+theme_classic()+
  geom_col(aes(y=total.effort,x=daytype,fill=as.character(month)),position = "stack")+
  labs(x="",y="Total Effort",fill="Month")


# good, useful plots above, now how to scale this up to many lakes?

# I'm going to make a summary dataframe and a loop to go through each creel-year and pull out when the lost in accuracy occurs for each month and day type
survs=unique(ttrEff$survey.seq.no[!is.na(ttrEff$survey.seq.no)])
thresh=data.frame(wbic=NA,
                  year=NA,
                  month=NA,
                  daytype=NA,
                  survey.seq.no=NA,
                  actualEff=NA,
                  reducedEff=NA,
                  reducedEff.sd=NA,
                  pReduc=NA)

for(i in 1:length(survs)){
  tdat=ttrEff[ttrEff$survey.seq.no==survs[i],]
  adat=ceff[ceff$survey.seq.no==survs[i],]
  for(y in 1:length(unique(adat$year))){
    for(m in 1:length(unique(adat$month))){
      for(d in 1:length(unique(adat$daytype))){
        z=tdat[tdat$year==unique(adat$year)[y] & tdat$month==unique(adat$month)[m] & tdat$daytype==unique(adat$daytype)[d],]
        b=adat[adat$year==unique(adat$year)[y] & adat$month==unique(adat$month)[m] & adat$daytype==unique(adat$daytype)[d],]
        
        if(any(unique(z$total.effort)!=0)){
          pR=max(unique(z$pReduc)[which(!(b$total.effort<(z$total.effort+z$total.effort.sd) & b$total.effort>(z$total.effort-z$total.effort.sd)))])
            if(is.infinite(pR)){
            addDat=c(wbic=unique(z$wbic),
                     year=unique(z$year),
                     month=unique(z$month),
                     daytype=unique(z$daytype),
                     survey.seq.no=survs[i],
                     actualEff=b$total.effort,
                     reducedEff=NA,
                     reducedEff.sd=NA,
                     pReduc=NA)
            
            thresh=rbind(thresh,addDat)
          }else{
          addDat=c(wbic=unique(z$wbic),
                   year=unique(z$year),
                   month=unique(z$month),
                   daytype=unique(z$daytype),
                   survey.seq.no=survs[i],
                   actualEff=b$total.effort,
                   reducedEff=z$total.effort[z$pReduc==pR],
                   reducedEff.sd=z$total.effort.sd[z$pReduc==pR],
                   pReduc=pR)
          
          thresh=rbind(thresh,addDat)
          }
        }
      }
    } 
  }
}
thresh=thresh[!is.na(thresh$wbic),]
# warnings about -inf are fine and dealt with in t he loop using the is.infinite() call

ggplot(thresh)+theme_classic()+
  geom_bar(aes(x=pReduc,fill=daytype),position = "dodge")+
  facet_wrap(~month, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Day Type")+
  theme(legend.position = "bottom")

# grouping across month

annualThresh=thresh%>%
  group_by(wbic,year,daytype, survey.seq.no)%>%
  summarise(actualEff=sum(as.numeric(actualEff)),
            reducedEff=sum(as.numeric(reducedEff),na.rm = T),
            reducedEff.sd=sd(as.numeric(reducedEff),na.rm = T),
            meanPR=mean(as.numeric(pReduc),na.rm=T))

ggplot(annualThresh)+theme_classic()+
  geom_density(aes(x=meanPR,fill=daytype),alpha=0.2)


# consider at some point doing something similar to the 'thresh' loop and calculating % diff between the effort estimate at each pReduce and the true estimate % diff = ((reduced-actual)/actual)*100. Then you could plot pReduce by % diff and start building that curve we talked about at the meeting.

# threshold loop for catch

survs=unique(ttrHarv$survey.seq.no[!is.na(ttrHarv$survey.seq.no)])
thresh.c=data.frame(wbic=NA,
                  year=NA,
                  month=NA,
                  daytype=NA,
                  survey.seq.no=NA,
                  species=NA,
                  actual.catch.rate=NA,
                  reduced.catch.rate=NA,
                  reduced.catch.rate.sd=NA,
                  pReduc=NA)

for(i in 1:length(survs)){
  tdat=ttrHarv[ttrHarv$survey.seq.no==survs[i],]
  adat=charv[charv$survey.seq.no==survs[i],]
  for(y in 1:length(unique(adat$year))){
    for(m in 1:length(unique(adat$month))){
      for(d in 1:length(unique(adat$daytype))){
        for(s in 1:length(unique(adat$species))){
          z=tdat[tdat$year==unique(adat$year)[y] &
                   tdat$month==unique(adat$month)[m] & 
                   tdat$daytype==unique(adat$daytype)[d] & 
                   tdat$species==unique(adat$species)[s],]
          
          b=adat[adat$year==unique(adat$year)[y] & 
                   adat$month==unique(adat$month)[m] & 
                   adat$daytype==unique(adat$daytype)[d] &
                   adat$species==unique(adat$species)[s],]
          # square rooting varience here to get a SD to keep things in the same terms as the effort threshold above
          if(any(unique(z$total.spp.catch)!=0)){
            pR=max(unique(z$pReduc)[which(!(b$catch.rate<(z$catch.rate+sqrt(z$catch.rate.var)) & b$catch.rate>(z$catch.rate-sqrt(z$catch.rate.var))))])
            if(is.infinite(pR)){
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.catch.rate=b$catch.rate,
                       reduced.catch.rate=NA,
                       reduced.catch.rate.sd=NA,
                       pReduc=NA)
              
              thresh.c=rbind(thresh.c,addDat)
            }else{
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.catch.rate=b$catch.rate,
                       reduced.catch.rate=z$catch.rate[z$pReduc==pR],
                       reduced.catch.rate.sd=sqrt(z$catch.rate.var[z$pReduc==pR]),
                       pReduc=pR)
              
              thresh.c=rbind(thresh.c,addDat)
            }
          }
        }
      }
    } 
  }
}
thresh.c=thresh.c[!is.na(thresh.c$wbic),]

## REMEMBER THESE ARE CATCH RATES
# warnings about -inf are fine and dealt with in the loop using the is.infinite() call
thresh.c$month=factor(thresh.c$month, levels = sort(as.numeric(unique(thresh.c$month))), labels = sort(as.numeric(unique(thresh.c$month))))
thresh.c.p=thresh.c[thresh.c$species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout"),] # picking out important species to plot
wknd=ggplot(thresh.c.p[thresh.c.p$daytype=="weekend",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wkdy=ggplot(thresh.c.p[thresh.c.p$daytype=="weekday",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wknd
wkdy

# grouping across month

annualThresh.c=thresh.c.p%>%
  group_by(wbic,year,daytype, survey.seq.no, species)%>%
  summarise(actualcatch.rate=sum(as.numeric(actual.catch.rate)),
            reduced.catch.rate=sum(as.numeric(reduced.catch.rate),na.rm = T),
            reduced.catch.rate.sd=sd(as.numeric(reduced.catch.rate.sd),na.rm = T),
            meanPR=mean(as.numeric(pReduc),na.rm=T))

ggplot(annualThresh.c)+theme_classic()+
  geom_density(aes(x=meanPR,fill=daytype),alpha=0.2)+
  facet_wrap(~species, scales = "free")+
  scale_fill_viridis_d()+
  labs(x="Mean % of Data Removed", y="Density",fill="Day Type")


# threshold loop for harvest

survs=unique(ttrHarv$survey.seq.no[!is.na(ttrHarv$survey.seq.no)])
thresh.h=data.frame(wbic=NA,
                    year=NA,
                    month=NA,
                    daytype=NA,
                    survey.seq.no=NA,
                    species=NA,
                    actual.total.harvest=NA,
                    reduced.total.harvest=NA,
                    reduced.harvest.sd=NA,
                    pReduc=NA)

for(i in 1:length(survs)){
  tdat=ttrHarv[ttrHarv$survey.seq.no==survs[i],]
  adat=charv[charv$survey.seq.no==survs[i],]
  for(y in 1:length(unique(adat$year))){
    for(m in 1:length(unique(adat$month))){
      for(d in 1:length(unique(adat$daytype))){
        for(s in 1:length(unique(adat$species))){
          z=tdat[tdat$year==unique(adat$year)[y] &
                   tdat$month==unique(adat$month)[m] & 
                   tdat$daytype==unique(adat$daytype)[d] & 
                   tdat$species==unique(adat$species)[s],]
          
          b=adat[adat$year==unique(adat$year)[y] & 
                   adat$month==unique(adat$month)[m] & 
                   adat$daytype==unique(adat$daytype)[d] &
                   adat$species==unique(adat$species)[s],]
          # square rooting varience here to get a SD to keep things in the same terms as the effort threshold above
          if(any(unique(z$total.harvest)!=0)){
            pR=max(unique(z$pReduc)[which(!(b$total.harvest<(z$total.harvest+sqrt(z$harvest.var)) & b$total.harvest>(z$total.harvest-sqrt(z$harvest.var))))])
            if(is.infinite(pR)){
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.total.harvest=b$total.harvest,
                       reduced.total.harvest=NA,
                       reduced.harvest.sd=NA,
                       pReduc=NA)
              
              thresh.h=rbind(thresh.h,addDat)
            }else{
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.total.harvest=b$total.harvest,
                       reduced.total.harvest=z$total.harvest[z$pReduc==pR],
                       reduced.harvest.sd=sqrt(z$harvest.var[z$pReduc==pR]),
                       pReduc=pR)
              
              thresh.h=rbind(thresh.h,addDat)
            }
          }
        }
      }
    } 
  }
}
thresh.h=thresh.h[!is.na(thresh.h$wbic),]

# warnings about -inf are fine and dealt with in the loop using the is.infinite() call
thresh.h$month=factor(thresh.h$month, levels = sort(as.numeric(unique(thresh.h$month))), labels = sort(as.numeric(unique(thresh.h$month))))
thresh.h.p=thresh.h[thresh.h$species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout"),] # picking out important species to plot
wknd=ggplot(thresh.h.p[thresh.h.p$daytype=="weekend",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wkdy=ggplot(thresh.h.p[thresh.h.p$daytype=="weekday",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wknd
wkdy

# grouping across month

annualThresh.h=thresh.h.p%>%
  group_by(wbic,year,daytype, survey.seq.no, species)%>%
  summarise(actual.total.harvest=sum(as.numeric(actual.total.harvest)),
            reduced.total.harvest=sum(as.numeric(reduced.total.harvest),na.rm = T),
            reduced.harvest.sd=sd(as.numeric(reduced.harvest.sd),na.rm = T),
            meanPR=mean(as.numeric(pReduc),na.rm=T))

ggplot(annualThresh.h)+theme_classic()+
  geom_density(aes(x=meanPR,fill=daytype),alpha=0.2)+
  facet_wrap(~species, scales = "free")+
  scale_fill_viridis_d()+
  labs(x="Mean % of Data Removed", y="Density",fill="Day Type")

# threshold loop for harvest rate

survs=unique(ttrHarv$survey.seq.no[!is.na(ttrHarv$survey.seq.no)])
thresh.hr=data.frame(wbic=NA,
                    year=NA,
                    month=NA,
                    daytype=NA,
                    survey.seq.no=NA,
                    species=NA,
                    actual.harvest.rate=NA,
                    reduced.harvest.rate=NA,
                    reduced.harvest.rate.sd=NA,
                    pReduc=NA)

for(i in 1:length(survs)){
  tdat=ttrHarv[ttrHarv$survey.seq.no==survs[i],]
  adat=charv[charv$survey.seq.no==survs[i],]
  for(y in 1:length(unique(adat$year))){
    for(m in 1:length(unique(adat$month))){
      for(d in 1:length(unique(adat$daytype))){
        for(s in 1:length(unique(adat$species))){
          z=tdat[tdat$year==unique(adat$year)[y] &
                   tdat$month==unique(adat$month)[m] & 
                   tdat$daytype==unique(adat$daytype)[d] & 
                   tdat$species==unique(adat$species)[s],]
          
          b=adat[adat$year==unique(adat$year)[y] & 
                   adat$month==unique(adat$month)[m] & 
                   adat$daytype==unique(adat$daytype)[d] &
                   adat$species==unique(adat$species)[s],]
          # square rooting varience here to get a SD to keep things in the same terms as the effort threshold above
          if(any(unique(z$harvest.rate)!=0)){
            pR=max(unique(z$pReduc)[which(!(b$harvest.rate<(z$harvest.rate+sqrt(z$harvest.rate.var)) & b$harvest.rate>(z$harvest.rate-sqrt(z$harvest.rate.var))))])
            if(is.infinite(pR)){
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.harvest.rate=b$harvest.rate,
                       reduced.harvest.rate=NA,
                       reduced.harvest.sd=NA,
                       pReduc=NA)
              
              thresh.hr=rbind(thresh.hr,addDat)
            }else{
              addDat=c(wbic=unique(z$wbic),
                       year=unique(z$year),
                       month=unique(z$month),
                       daytype=unique(z$daytype),
                       survey.seq.no=survs[i],
                       species=unique(adat$species)[s],
                       actual.harvest.rate=b$harvest.rate,
                       reduced.harvest.rate=z$harvest.rate[z$pReduc==pR],
                       reduced.harvest.sd=sqrt(z$harvest.rate.var[z$pReduc==pR]),
                       pReduc=pR)
              
              thresh.hr=rbind(thresh.hr,addDat)
            }
          }
        }
      }
    } 
  }
}
thresh.hr=thresh.hr[!is.na(thresh.hr$wbic),]

# warnings about -inf are fine and dealt with in the loop using the is.infinite() call
thresh.hr$month=factor(thresh.hr$month, levels = sort(as.numeric(unique(thresh.hr$month))), labels = sort(as.numeric(unique(thresh.hr$month))))
thresh.hr.p=thresh.hr[thresh.hr$species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout"),] # picking out important species to plot
wknd=ggplot(thresh.hr.p[thresh.hr.p$daytype=="weekend",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wkdy=ggplot(thresh.hr.p[thresh.hr.p$daytype=="weekday",])+theme_classic()+
  geom_bar(aes(x=pReduc,fill=month),position = "dodge")+
  scale_fill_viridis_d()+
  facet_wrap(~species, scales = "free")+
  labs(x="Proportion of Data Removed", fill= "Month")+
  theme(legend.position = "bottom")
wknd
wkdy

# grouping across month

annualthresh.hr=thresh.hr.p%>%
  group_by(wbic,year,daytype, survey.seq.no, species)%>%
  summarise(actual.harvest.rate=sum(as.numeric(actual.harvest.rate)),
            reduced.harvest.rate=sum(as.numeric(reduced.harvest.rate),na.rm = T),
            reduced.harvest.rate.sd=sd(as.numeric(reduced.harvest.rate.sd),na.rm = T),
            meanPR=mean(as.numeric(pReduc),na.rm=T))

ggplot(annualthresh.hr)+theme_classic()+
  geom_density(aes(x=meanPR,fill=daytype),alpha=0.2)+
  facet_wrap(~species, scales = "free")+
  scale_fill_viridis_d()+
  labs(x="Mean % of Data Removed", y="Density",fill="Day Type")


#### QUICK FMPT ####
 # I need to effort and harvest calculations with the full removal of the winter creel data
 # I also need to provide a harvest breakdown by month similar to the effort breakdown.



nw.cserv=cserv # nothing to change here, it's the general survey info
nw.cvis=cvis%>%
  mutate(month=month(sample.date))%>%
  filter(month%in%c(4:10))# keeping only the non-winter months
nw.ccou=ccou%>%
  mutate(month=month(sample.date))%>%
  filter(month%in%c(4:10))
nw.cint=cint%>%
  mutate(month=month(sample.date))%>%
  filter(month%in%c(4:10))
nw.cfish=cfish%>%
  mutate(month=month(sample.date))%>%
  filter(month%in%c(4:10))


nw.ceff=calc_creel_effort(creel_count_data=nw.ccou,creel_int_data=nw.cint) # don't use the grouping argument here, seems to break it

nw.charv=calc_creel_harvest(creel_count_data = nw.ccou,creel_int_data = nw.cint,creel_fish_data = nw.cfish) # don't use the grouping argument here, seems to break it

nw.charvR=calc_creel_harvest_rates(nw.cfish)


# comparison of effort and harvest calculates with and without winter months data
effComp=rbind(cbind(ceff,treat=rep("full",nrow(ceff))),
              cbind(nw.ceff,treat=rep("noWinter",nrow(nw.ceff))))

# annual effort totals
yrSum=effComp%>%
  group_by(wbic, survey.seq.no,treat)%>%
  summarise(sumEff=sum(total.effort))

ggplot(yrSum)+theme_classic()+
  geom_density(aes(sumEff,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()

# kolmogorov-smirnov test to see if the two distributions are different

ks.test(yrSum$sumEff[yrSum$treat=="full"],yrSum$sumEff[yrSum$treat=="noWinter"],alternative = "two.sided") # believe this is the best test to use
#t.test(yrSum$sumEff[yrSum$treat=="full"],yrSum$sumEff[yrSum$treat=="noWinter"],alternative = "two.sided") # definitely not the right test since the data is not normally distributed
wilcox.test(yrSum$sumEff[yrSum$treat=="full"],yrSum$sumEff[yrSum$treat=="noWinter"],alternative = "two.sided", paired=F)

### HARV

# harvest breakdown by month for a few key species
pharv=charv[charv$species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout"),]
pharv$month=factor(pharv$month,levels = sort(unique(pharv$month)),labels = sort(unique(pharv$month)))
ggplot(pharv)+theme_classic()+
  geom_col(aes(y=total.harvest, x=daytype, fill=month),position = "stack")+
  labs(x="Day Type",y="Total Harvest",fill="Month")+
  scale_fill_viridis_d()
  

# comparison of effort and harvest calculates with and without winter months data
harvComp=rbind(cbind(charv,treat=rep("full",nrow(charv))),
                cbind(nw.charv,treat=rep("noWinter",nrow(nw.charv))))

# annual harvest totals
yrSum=harvComp%>%
  group_by(wbic, survey.seq.no,treat,species)%>%
  summarise(sumCatch=sum(total.spp.catch),
            sumHarv=sum(total.spp.harvest),
            meanHarvR=mean(spp.harvest.rate))%>%
  filter(species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout")) # pulling out some relevant species

p.c=ggplot(yrSum)+theme_classic()+
  geom_density(aes(sumCatch,fill=treat),alpha=0.2)+coord_cartesian(xlim=c(0,2000))+scale_fill_viridis_d()
p.h=ggplot(yrSum)+theme_classic()+
  geom_density(aes(sumHarv,fill=treat),alpha=0.2)+coord_cartesian(xlim=c(0,2000))+scale_fill_viridis_d()
p.hr=ggplot(yrSum)+theme_classic()+
  geom_density(aes(meanHarvR,fill=treat),alpha=0.2)+scale_fill_viridis_d()
ggarrange(p.c,p.h,p.hr, common.legend = T)


ggplot(yrSum)+theme_classic()+facet_wrap(~species, scales="free")+
  geom_density(aes(sumCatch,fill=treat),alpha=0.2)+coord_cartesian(xlim=c(0,2000))+scale_fill_viridis_d()
ggplot(yrSum)+theme_classic()+facet_wrap(~species, scales="free")+
  geom_density(aes(sumHarv,fill=treat),alpha=0.2)+coord_cartesian(xlim=c(0,2000))+scale_fill_viridis_d()
ggplot(yrSum)+theme_classic()+facet_wrap(~species, scales="free")+
  geom_density(aes(meanHarvR,fill=treat),alpha=0.2)+scale_fill_viridis_d()

# kolmogorov-smirnov test to see if the two distributions are different

test=ks.test(yrSum$sumCatch[yrSum$treat=="full" & yrSum$species=="walleye"],yrSum$sumCatch[yrSum$treat=="noWinter" & yrSum$species=="walleye"],alternative = "two.sided")

trts=unique(yrSum$treat)
spps=unique(yrSum$species)
out=data.frame(species=spps,
               catch.sigDiff=NA,
               harv.sigDiff=NA,
               harvR.sigDiff=NA)

for(s in 1:length(spps)){
    testC=ks.test(yrSum$sumCatch[yrSum$treat=="full" & yrSum$species==spps[s]],
                  yrSum$sumCatch[yrSum$treat=="noWinter" & yrSum$species==spps[s]], 
                  alternative = "two.sided")
    testH=ks.test(yrSum$sumHarv[yrSum$treat=="full" & yrSum$species==spps[s]],
                  yrSum$sumHarv[yrSum$treat=="noWinter" & yrSum$species==spps[s]], 
                  alternative = "two.sided")
    testHR=ks.test(yrSum$meanHarvR[yrSum$treat=="full" & yrSum$species==spps[s]],
                  yrSum$meanHarvR[yrSum$treat=="noWinter" & yrSum$species==spps[s]], 
                  alternative = "two.sided")
    out$catch.sigDiff[out$species==spps[s]]=ifelse(testC$p.value<0.05,T,F)
    out$harv.sigDiff[out$species==spps[s]]=ifelse(testH$p.value<0.05,T,F)
    out$harvR.sigDiff[out$species==spps[s]]=ifelse(testHR$p.value<0.05,T,F)
}

summary(out$catch.sigDiff)
summary(out$harv.sigDiff)
summary(out$harvR.sigDiff)
# looks like pulling out winter creel only matters for BCP harvest, all NPK metrics, and YWP catch and harvest


# bayesian test to see whether data from with and without winter creel are different

# first how is the data distributed for each metric

# effort lognormally distributed
ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=sumEff,fill=treat),alpha=0.2)

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=log(sumEff),fill=treat),alpha=0.2)


# catch appears lognormally distributed for most species, brown trout may be an exception and would maybe need beta distribution. Lots of 0s though, try poisson instead of lognormal since it's discrete intergers.

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=sumCatch, fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=log(sumCatch), fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")

# harvest appears lognormally distributed for most species, muskellunge, lake trout, smallmouth, largemouth are exceptions, consider gamma here

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=sumHarv, fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=log(sumHarv), fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")

# harvest appears approximately lognormally distributed for all species, could use gamma for BLG and BCP

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=meanHarvR, fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")

ggplot(yrSum)+theme_classic()+
  geom_density(aes(x=log(meanHarvR), fill=treat),alpha=0.2)+
  facet_wrap(~species, scales="free")


# building a likelihood for effort data first

library(BayesianTools)

# annual effort totals
yrSum.EF=effComp%>%
  group_by(wbic, survey.seq.no,treat)%>%
  summarise(sumEff=sum(total.effort))

# removing 0 effort surveys, only 1.5% of data and we really should be surveying wbics that get effort to begin with so having that 0 info in here isn't that useful other than to say they were a waste of a survey.
yrSum.EF=yrSum.EF[yrSum.EF$sumEff!=0,]
tparms=c(mean(log(yrSum.EF$sumEff[yrSum.EF$treat=="full"])), sd(log(yrSum.EF$sumEff[yrSum.EF$treat=="full"])))
prior=createTruncatedNormalPrior(mean=c(mean(log(yrSum.EF$sumEff)),sd(log(yrSum.EF$sumEff))), 
                                 sd=c(1,1),
                                 lower = c(5,0),
                                 upper = c(15,5))
effLL.full=function(param){
  alpha=param[1]
  beta=param[2]

  points=rlnorm(sum(yrSum.EF$treat=="full"), meanlog = alpha, sdlog = beta)
  ll=dlnorm(yrSum.EF$sumEff[yrSum.EF$treat=="full"], meanlog = mean(log(points)), sdlog = sd(log(yrSum.EF$sumEff[yrSum.EF$treat=="full"])), log = T)
  return(sum(ll))
}

effLL.nw=function(param){
  alpha=param[1]
  beta=param[2]
  
  points=rlnorm(sum(yrSum.EF$treat=="noWinter"), meanlog = alpha, sdlog = beta)
  ll=dlnorm(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"], meanlog = mean(log(points)), sdlog = sd(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"]), log = T)
  return(sum(ll))
}

setup.full=createBayesianSetup(effLL.full, prior = prior)
setup.nw=createBayesianSetup(effLL.nw, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F)

out.full=runMCMC(bayesianSetup = setup.full, sampler = "DEzs", settings = settings)
out.nw=runMCMC(bayesianSetup = setup.nw, sampler = "DEzs", settings = settings)

#saveRDS(list(out.full,out.nw),"bayesEffort.RData") # saving out put to load into Rmd since it takes some time

m.full=marginalLikelihood(out.full)
m.nw=marginalLikelihood(out.nw)

exp(m.full$ln.ML-m.nw$ln.ML) # says models are the same basically

summary(out.full)
summary(out.nw)

marginalPlot(out.full,prior = T)
marginalPlot(out.nw,prior = T)

# showing that the parameters and their CIs overlap
full=getSample(out.full)
nw=getSample(out.nw)

parComp.alpha=data.frame(dataset=c("full","noWinter"),
                    lower.95.ci=c(sort(full[,1])[length(full[,1])*0.025],
                                  sort(nw[,1])[length(nw[,1])*0.025]),
                    median=c(median(full[,1]), median(nw[,1])),
                    upper.95.ci=c(sort(full[,1])[length(full[,1])*0.975],
                                  sort(nw[,1])[length(nw[,1])*0.975]))

parComp.beta=data.frame(dataset=c("full","noWinter"),
                         lower.95.ci=c(sort(full[,2])[length(full[,2])*0.025],
                                       sort(nw[,2])[length(nw[,2])*0.025]),
                         median=c(median(full[,2]), median(nw[,2])),
                         upper.95.ci=c(sort(full[,2])[length(full[,2])*0.975],
                                       sort(nw[,2])[length(nw[,2])*0.975]))
# looking to see if parm estimates produce data that visually at least look like the observed
pars=getSample(out.full)

fullComp=data.frame(eff=c(yrSum.EF$sumEff[yrSum.EF$treat=="full"],rlnorm(n=length(yrSum.EF$sumEff[yrSum.EF$treat=="full"]),
                                                                         meanlog = tparms[1],
                                                                         sdlog = tparms[2])),
                    treat=c(rep("observed",length(yrSum.EF$sumEff[yrSum.EF$treat=="full"])),rep("pred",length(yrSum.EF$sumEff[yrSum.EF$treat=="full"]))))

ggplot(fullComp)+theme_classic()+
  geom_density(aes(x=log(eff),fill=treat),alpha=0.2)

pars=getSample(out.nw)

nwComp=data.frame(eff=c(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"],rlnorm(n=length(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"]),
                                                                         meanlog = tparms[1],
                                                                         sdlog = tparms[2])),
                    treat=c(rep("observed",length(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"])),rep("pred",length(yrSum.EF$sumEff[yrSum.EF$treat=="noWinter"]))))

ggplot(nwComp)+theme_classic()+
  geom_density(aes(x=log(eff),fill=treat),alpha=0.2)



### now repeat above analyses for catch, harvest, and harvest rate


# annual catch effort and harvest totals
yrSum.C=harvComp%>%
  group_by(wbic, survey.seq.no,treat,species)%>%
  summarise(sumCatch=sum(total.spp.catch),
            sumHarv=sum(total.spp.harvest),
            meanHarvR=mean(spp.harvest.rate))%>%
  filter(species%in%c("black_crappie","bluegill","largemouth_bass","muskellunge","northern_pike","pumpkinseed","rock_bass","smallmouth_bass","walleye","yellow_perch","brook_trout","brown_trout","lake_trout","rainbow_trout")) # pulling out some relevant species


### Catch
yrSum.Ca=yrSum.C[yrSum.C$species!="brown_trout",] # removing this species since it's such a low number and non poisson distributed

# tparms=yrSum.Ca%>%
#   group_by(species)%>%
#   summarise(lambda=mean(sumCatch))
# 
# # how to deal with different species?
# pdens=function(par){
#   return(dpois(par, lambda = tparms$lambda, log = T))
# }
# psampler=function(n=length(tparms$lambda)){
#   return(rpois(n, lambda=tparms$lambda))
# }
# prior=createPrior(density = pdens, sampler = psampler)
# 
# # I'm going to try a uniform distribution for each species specific lambda, everything is a range of 0 to one order of magnitude bigger than the mean Lambda
# unifPrior=createUniformPrior(lower=rep(0,13), upper = c(1000,5000,100,100,1000,100,1000,1000,100,100,1000,1000,1000))
# 
# 
# cLL.full=function(param){
#   lambda.bcp=param[1]
#   lambda.blg=param[2]
#   lambda.bkt=param[3]
#   lambda.lkt=param[4]
#   lambda.lmb=param[5]
#   lambda.msk=param[6]
#   lambda.npk=param[7]
#   lambda.pks=param[8]
#   lambda.rnt=param[9]
#   lambda.rkb=param[10]
#   lambda.smb=param[11]
#   lambda.wly=param[12]
#   lambda.ywp=param[13]
#   
#   p.bcp=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="black_crappie"& yrSum.Ca$treat=="full"]),lambda = lambda.bcp)
#   p.blg=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="bluegill"& yrSum.Ca$treat=="full"]),lambda = lambda.blg)
#   p.bkt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="brook_trout"& yrSum.Ca$treat=="full"]),lambda = lambda.bkt)
#   p.lkt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="lake_trout"& yrSum.Ca$treat=="full"]),lambda = lambda.lkt)
#   p.lmb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="largemouth_bass"& yrSum.Ca$treat=="full"]),lambda = lambda.lmb)
#   p.msk=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="muskellunge"& yrSum.Ca$treat=="full"]),lambda = lambda.msk)
#   p.npk=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="northern_pike"& yrSum.Ca$treat=="full"]),lambda = lambda.npk)
#   p.pks=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="pumpkinseed"& yrSum.Ca$treat=="full"]),lambda = lambda.pks)
#   p.rnt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="rainbow_trout"& yrSum.Ca$treat=="full"]),lambda = lambda.rnt)
#   p.rkb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="rock_bass"& yrSum.Ca$treat=="full"]),lambda = lambda.rkb)
#   p.smb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="smallmouth_bass"& yrSum.Ca$treat=="full"]),lambda = lambda.smb)
#   p.wly=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="walleye"& yrSum.Ca$treat=="full"]),lambda = lambda.wly)
#   p.ywp=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="yellow_perch"& yrSum.Ca$treat=="full"]),lambda = lambda.ywp)
#   
#   ll.bcp=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="black_crappie"& yrSum.Ca$treat=="full"], lambda = p.bcp, log = T)
#   ll.blg=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="bluegill"& yrSum.Ca$treat=="full"], lambda = p.blg, log = T)
#   ll.bkt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="brook_trout"& yrSum.Ca$treat=="full"], lambda = p.bkt, log = T)
#   ll.lkt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="lake_trout"& yrSum.Ca$treat=="full"], lambda = p.lkt, log = T)
#   ll.lmb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="largemouth_bass"& yrSum.Ca$treat=="full"], lambda = p.lmb, log = T)
#   ll.msk=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="muskellunge"& yrSum.Ca$treat=="full"], lambda = p.msk, log = T)
#   ll.npk=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="northern_pike"& yrSum.Ca$treat=="full"], lambda = p.npk, log = T)
#   ll.pks=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="pumpkinseed"& yrSum.Ca$treat=="full"], lambda = p.pks, log = T)
#   ll.rnt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="rainbow_trout"& yrSum.Ca$treat=="full"], lambda = p.rnt, log = T)
#   ll.rkb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="rock_bass"& yrSum.Ca$treat=="full"], lambda = p.rkb, log = T)
#   ll.smb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="smallmouth_bass"& yrSum.Ca$treat=="full"], lambda = p.smb, log = T)
#   ll.wly=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="walleye"& yrSum.Ca$treat=="full"], lambda = p.wly, log = T)
#   ll.ywp=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="yellow_perch"& yrSum.Ca$treat=="full"], lambda = p.ywp, log = T)
#   
#   return(sum((ll.bcp*.101),
#              (ll.blg*.106),
#              (ll.bkt*.001),
#              (ll.lkt*.002),
#              (ll.lmb*.103),
#              (ll.msk*.087),
#              (ll.npk*.105),
#              (ll.pks*.077),
#              (ll.rnt*.0008),
#              (ll.rkb*.096),
#              (ll.smb*.101),
#              (ll.wly*.109),
#              (ll.ywp*.108))) # weighting likelihood by species prevalence in creel data so the more commonly caught species are more important in determining likelihood fit
# }
# 
# cLL.nw=function(param){
#   lambda.bcp=param[1]
#   lambda.blg=param[2]
#   lambda.bkt=param[3]
#   lambda.lkt=param[4]
#   lambda.lmb=param[5]
#   lambda.msk=param[6]
#   lambda.npk=param[7]
#   lambda.pks=param[8]
#   lambda.rnt=param[9]
#   lambda.rkb=param[10]
#   lambda.smb=param[11]
#   lambda.wly=param[12]
#   lambda.ywp=param[13]
#   
#   p.bcp=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="black_crappie"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.bcp)
#   p.blg=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="bluegill"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.blg)
#   p.bkt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="brook_trout"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.bkt)
#   p.lkt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="lake_trout"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.lkt)
#   p.lmb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="largemouth_bass"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.lmb)
#   p.msk=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="muskellunge"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.msk)
#   p.npk=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="northern_pike"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.npk)
#   p.pks=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="pumpkinseed"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.pks)
#   p.rnt=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="rainbow_trout"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.rnt)
#   p.rkb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="rock_bass"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.rkb)
#   p.smb=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="smallmouth_bass"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.smb)
#   p.wly=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="walleye"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.wly)
#   p.ywp=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species=="yellow_perch"& yrSum.Ca$treat=="noWinter"]),lambda = lambda.ywp)
#   
#   ll.bcp=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="black_crappie"& yrSum.Ca$treat=="noWinter"], lambda = p.bcp, log = T)
#   ll.blg=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="bluegill"& yrSum.Ca$treat=="noWinter"], lambda = p.blg, log = T)
#   ll.bkt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="brook_trout"& yrSum.Ca$treat=="noWinter"], lambda = p.bkt, log = T)
#   ll.lkt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="lake_trout"& yrSum.Ca$treat=="noWinter"], lambda = p.lkt, log = T)
#   ll.lmb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="largemouth_bass"& yrSum.Ca$treat=="noWinter"], lambda = p.lmb, log = T)
#   ll.msk=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="muskellunge"& yrSum.Ca$treat=="noWinter"], lambda = p.msk, log = T)
#   ll.npk=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="northern_pike"& yrSum.Ca$treat=="noWinter"], lambda = p.npk, log = T)
#   ll.pks=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="pumpkinseed"& yrSum.Ca$treat=="noWinter"], lambda = p.pks, log = T)
#   ll.rnt=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="rainbow_trout"& yrSum.Ca$treat=="noWinter"], lambda = p.rnt, log = T)
#   ll.rkb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="rock_bass"& yrSum.Ca$treat=="noWinter"], lambda = p.rkb, log = T)
#   ll.smb=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="smallmouth_bass"& yrSum.Ca$treat=="noWinter"], lambda = p.smb, log = T)
#   ll.wly=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="walleye"& yrSum.Ca$treat=="noWinter"], lambda = p.wly, log = T)
#   ll.ywp=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species=="yellow_perch"& yrSum.Ca$treat=="noWinter"], lambda = p.ywp, log = T)
#   
#   return(sum((ll.bcp*.101),
#              (ll.blg*.106),
#              (ll.bkt*.001),
#              (ll.lkt*.002),
#              (ll.lmb*.103),
#              (ll.msk*.087),
#              (ll.npk*.105),
#              (ll.pks*.077),
#              (ll.rnt*.0008),
#              (ll.rkb*.096),
#              (ll.smb*.101),
#              (ll.wly*.109),
#              (ll.ywp*.108))) # weighting likelihood by species prevalence in creel data so the more commonly caught species are more important in determining likelihood fit
# }
# 
# setup.full.c=createBayesianSetup(cLL.full, prior = unifPrior)
# setup.nw.c=createBayesianSetup(cLL.nw, prior = unifPrior)
# 
# settings.c=list(iterations=1000, nrChains=3, message=F)
# 
# startT=Sys.time()
# out.full.c=runMCMC(bayesianSetup = setup.full.c, sampler = "DEzs", settings = settings.c)
# out.nw.c=runMCMC(bayesianSetup = setup.nw.c, sampler = "DEzs", settings = settings.c)
# endT=Sys.time()
# tdiff=endT-startT # 1.5 hrs for 3 chains of 1000 iterations, means that 100000 for 3 chains could take a week to run
# 
# # m.full=marginalLikelihood(out.full.c)
# # m.nw=marginalLikelihood(out.nw.c)
# # 
# # exp(m.full.c$ln.ML-m.nw.c$ln.ML) # says models are the same basically
# 
# summary(out.full.c)
# summary(out.nw.c)
# 
# marginalPlot(out.full.c,prior = T)
# marginalPlot(out.nw.c,prior = T)


# looking to see if it makes more sense to create generic prior functions and model each species individually instead of all at once as I've done above since that may not scale well to large numbers of iterations
loopStart=Sys.time()
tparms=yrSum.Ca%>%
  group_by(species)%>%
  summarise(lambda=mean(sumCatch))
tparms$low=c(300,1000,0,0,100,0,0,0,0,20,0,200,700)
tparms$upper=c(700,2500,100,60,400,60,200,300,30,50,300,400,1000)

out.generic=list()
for(i in 1:length(tparms$species)){
  
  # set up uniform prior
  unifPrior.generic=createUniformPrior(lower=tparms$low[tparms$species==tparms$species[i]], upper = tparms$upper[tparms$species==tparms$species[i]])
  
  cLL.full.generic=function(param){
    lambda=param[1]
    
    p=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i]& yrSum.Ca$treat=="full"]),lambda = lambda)
    
    ll=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i]& yrSum.Ca$treat=="full"], lambda = p, log = T)
    
    return(sum(ll))
  }
  
  cLL.nw.generic=function(param){
    lambda=param[1]
    
    p=rpois(sum(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i]& yrSum.Ca$treat=="noWinter"]),lambda = lambda)
    
    ll=dpois(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i]& yrSum.Ca$treat=="noWinter"], lambda = p, log = T)
    
    return(sum(ll))
  }
  
  setup.full.c.generic=createBayesianSetup(cLL.full.generic, prior = unifPrior.generic)
  setup.nw.c.generic=createBayesianSetup(cLL.nw.generic, prior = unifPrior.generic)
  
  settings.c.generic=list(iterations=20000, nrChains=3, message=F)
  
  startT=Sys.time()
  out.full.c.generic=runMCMC(bayesianSetup = setup.full.c.generic, sampler = "DEzs", settings = settings.c.generic)
  out.nw.c.generic=runMCMC(bayesianSetup = setup.nw.c.generic, sampler = "DEzs", settings = settings.c.generic)
  endT=Sys.time()
  tdiff=endT-startT 
  
  out.generic[[i]]=list(spp=tparms$species[i],
                        full=out.full.c.generic,
                        nw=out.nw.c.generic,
                        runtime=tdiff)
  
}

#saveRDS(out.generic, file = "sppCatchBayes_shortrun.RData") # this is slightly shorter than the other way and may scale better.
saveRDS(out.generic, file = "sppCatchBayes_longrun.RData") # this takes just over 1.14 days
loopEnd=Sys.time()
loopRun=loopEnd-loopStart

# code to narrow uniform priors which will also help model speed
for( i in 1:13){
  marginalPlot(out.generic[[i]][["full"]],prior=T)
}

# next look at convergence for all species models - after the long run they still don't seem converged
convSum=data.frame(spp=rep(NA,13),
                   full=rep(NA,13),
                   noWinter=rep(NA,13))
for( i in 1:length(out.generic)){
  convSum$full[i]=gelmanDiagnostics(out.generic[[i]][["full"]])[[1]][1]
  convSum$noWinter[i]=gelmanDiagnostics(out.generic[[i]][["nw"]])[[1]][1]
  convSum$spp[i]=out.generic[[i]][["spp"]]
                 
}

# runtime for all species
rtSum=data.frame(spp=rep(NA,13),
                 runtime=rep(NA,13))
for(i in 1:length(out.generic)){
  rtSum$spp[i]=out.generic[[i]][["spp"]]
  rtSum$runtime[i]=out.generic[[i]][["runtime"]] # these times need to be standardized still to the same unit
}

# bayes factor for all species --- this takes a long time to run for all models

bfSum=data.frame(spp=rep(NA,13),
                 f.nw=rep(NA,13))
for(i in 1:length(out.generic)){
  m1=marginalLikelihood(out.generic[[i]][["full"]])
  m2=marginalLikelihood(out.generic[[i]][["nw"]])
  
  bfSum$f.nw[i]=exp(m1$ln.ML - m2$ln.ML)
  bfSum$spp[i]=out.generic[[i]][["spp"]]
}

# overlapping parm estiamtes for all species

estSum=data.frame(spp=rep(NA,13),
                  lcl.f=rep(NA,13),
                  median.f=rep(NA,13),
                  ucl.f=rep(NA,13),
                  lcl.nw=rep(NA,13),
                  median.nw=rep(NA,13),
                  ucl.nw=rep(NA,13),
                  overlap=rep(NA,13))
for(i in 1:length(out.generic)){
  
  tFull=getSample(out.generic[[i]][["full"]])
  tnw=getSample(out.generic[[i]][["nw"]])
  
  estSum$spp[i]=out.generic[[i]][["spp"]]
  estSum$lcl.f[i]=sort(tFull)[length(tFull)*0.025]
  estSum$median.f[i]=median(tFull)
  estSum$ucl.f[i]=sort(tFull)[length(tFull)*0.975]
  estSum$lcl.nw[i]=sort(tnw)[length(tnw)*0.025]
  estSum$median.nw[i]=median(tnw)
  estSum$ucl.nw[i]=sort(tnw)[length(tnw)*0.975]
  estSum$overlap[i]=estSum$lcl.f[i] <= estSum$ucl.nw[i] && estSum$lcl.nw[i] <=estSum$ucl.f[i] # overlap test
  
}


# put the above information in a summary table

# sim data as sanity check

# looking to see if parm estimates produce data that visually at least look like the observed

pdat=data.frame(spp=NA,
                data=NA,
                treat=NA,
                noWinter=NA)
for(i in 1:length(out.generic)){
  pars.f=getSample(out.generic[[i]][["full"]])
  
  fullComp=data.frame(data=c(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="full"],
                              rlnorm(n=length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="full"]),
                                                                           meanlog = mean(log(pars.f)),
                                                                           sdlog = sd(log(pars.f)))),
                      treat=c(rep("observed",length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="full"])),
                                  rep("pred",length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="full"]))),
                      noWinter=FALSE)
  
  pars.nw=getSample(out.generic[[i]][["nw"]])
  
  nwComp=data.frame(data=c(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="noWinter"],
                              rlnorm(n=length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="noWinter"]),
                                     meanlog = mean(log(pars.nw)),
                                     sdlog = sd(log(pars.nw)))),
                      treat=c(rep("observed",length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="noWinter"])),
                              rep("pred",length(yrSum.Ca$sumCatch[yrSum.Ca$species==tparms$species[i] & yrSum.Ca$treat=="noWinter"]))),
                    noWinter=TRUE)
  addDat=cbind(spp=rep(out.generic[[i]][["spp"]],nrow(fullComp)+nrow(nwComp)),rbind(fullComp,nwComp))
  pdat=rbind(pdat,addDat)
}

ggplot(pdat)+theme_classic()+
  geom_density(aes(x=data, fill=treat),alpha=0.2)+
  facet_wrap(spp~noWinter, scales = "free")
### all of the above isn't really finished but for now I'm working on walleye specific stuff for FMPT below


#### Walleye for FMPT ####

# looking at CTWI lakes only and only walleye

# need spring fyke netting survey info to get number of walleye marked 

# need individual creel fish data to get number of marked fish returned in the creel

# need a list of CTWI wbics to filter survey info by

lchar=get_fmdb_lakechar()
ctwiWBIC=lchar[lchar$trtystat==1 & !is.na(lchar$trtystat),]
ctwiWBIC.creel=ctwiWBIC$wbic[ctwiWBIC$wbic%in%cserv$wbic] # wbics in CTWI that have been creeled

tagdat=read.csv("tags_and_marks.csv") # marking data from fmdb since the package function URLs didn't seem to be working
tagdat$County=standardize_county_names(tagdat$County)
tagdat$Gear=standardize_string(tagdat$Gear)
tagdat$Waterbody.Name=standardize_waterbody_names(tagdat$Waterbody.Name)

fndat=tagdat%>% # this is just in case we care what type of mark is there, but I'm going to overwrite this for now and just get total number marked.
  filter(WBIC%in%ctwiWBIC.creel & Species.Code=="X22" & Gear=="fyke_net", !is.na(Mark.Given) & Mark.Given!="")%>%
  group_by(WBIC, Waterbody.Name,Survey.Year,Survey.Begin.Date,Survey.End.Date,Survey.Seq.No,Mark.Given)%>%
  summarise(nFish=sum(Number.of.Fish))

# now summing across all types of marks
fndat=tagdat%>% 
  filter(WBIC%in%ctwiWBIC.creel & Species.Code=="X22" & Gear=="fyke_net", !is.na(Mark.Given) & Mark.Given!="")%>%
  group_by(WBIC, Waterbody.Name,Survey.Year,Survey.Begin.Date,Survey.End.Date,Survey.Seq.No)%>%
  summarise(nFish=sum(Number.of.Fish))%>%
  mutate(Survey.Begin.Date=lubridate::mdy(Survey.Begin.Date),
         Survey.End.Date=lubridate::mdy(Survey.End.Date),
         begin.md=format(Survey.Begin.Date, "%m-%d"),
         end.md=format(Survey.End.Date, "%m-%d"))%>%
  filter(begin.md<"06-01") # getting rid of any marking surveys that may have taken place well after fishing season opened.

fdat=fndat%>% # now that I have only surveys I want, pooling across start and end dates within a year to create an anual total number marked for comparison to creel data
  group_by(WBIC, Waterbody.Name, Survey.Year)%>%
  summarise(nFN.marked=sum(nFish))

# table of marks found during creel surveys
crRecap=cfish.i%>%
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  mutate(month=month(sample.date),
         daytype=ifelse(wday(sample.date)%in%c(1,7),"weekend","weekday"))%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel
crRecap=crRecap%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# I think spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates=charv%>%
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst=crRecap%>%
  left_join(harvestEstimates)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp=markHarvEst%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked) # checked against the Nebagamon example provided by Tom and I'm really close with these estimates. It looks like my # of clipped fish counts are different from what Tom has in his exploitation DB. Not sure why that is but that's what appears to be causing the slight different in our exploitation rate estimates.
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp[,c(1:11,15:17)],"exploitation_rates_actual_creel_data.csv",row.names = F)
naExps=ang.exp[is.na(ang.exp$exp.rate),] # looking at the lake-years without data to see if there's a problem with my code or just missing data
# looks like it's either cases were no fish were marked in spring fyking for that creel survey or if for a specific strata no walleye were harvested or no marked walleye were harvested, either of those will throw an NA in the exploitation rate calculation

# there are 6 observation where the number of marks returned was higher than what was marked, I'm throwing these out.
ang.exp=ang.exp[!is.na(ang.exp$exp.rate) & ang.exp$exp.rate<1.0,]
# creating survey-level exploitation rates
ang.exp=ang.exp%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

# # plot of exploitation rate by month
# ang.exp$plot.month=month(ang.exp$month,label = T)
# ggplot(ang.exp)+theme_classic()+
#   geom_boxplot(aes(y=exp.rate, x=plot.month), fill="grey")+
#   geom_hline(yintercept = mean(ang.exp$exp.rate, na.rm = T), color="red")+
#   labs(y="Exploitation Rate", x="Month")
# 
# # plot of exploitation rate by month - logged
# ang.exp$plot.month=month(ang.exp$month,label = T)
# ggplot(ang.exp)+theme_classic()+
#   geom_boxplot(aes(y=log(exp.rate), x=plot.month), fill="grey")+
#   geom_hline(yintercept = -4.449, color="red")+ # setting this manually since the 0 exp rates mess up the logging here
#   labs(y="Log(Exploitation Rate)", x="Month")

# plot of number of marks returned per month standardized by the number of marks at large
# ggplot(ang.exp)+theme_classic()+
#   geom_boxplot(aes(y=(n.marks.recapped/nFN.marked), x=plot.month), fill="grey")+
#   geom_hline(yintercept=mean((n.marks.recapped/nFN.marked),na.rm=T),color="red")+
#   labs(y="Marks Returned / # Marked", x="Month")

# now I have 'actual' exploitation rates

# next I want to make a few scenarios with 'reduced' data, a 'noWinter' scenario, 'May-August' scenario, and 3 % removals (0.25 weekdays, 0.5 weekdays, 0.5 weekend)

ttrExp=as.data.frame(matrix(NA,ncol=ncol(cfish.i)))
colnames(ttrExp)=colnames(cfish.i)

ifish=cfish.i%>% # making a data frame to add my groupings too so I can leave cfish.i alone
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  mutate(daytype=ifelse(wday(sample.date)%in%c(1,7),"weekday","weekend"),
         month=month(sample.date),
         season=ifelse(month%in%4:10,"openwater","winter"))

iharv=charv%>% # making a separate data frame to add groupings to so I can leave charv alone
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  mutate(season=ifelse(month%in%4:10,"openwater","winter"))

# now making a reduced dataframe for each scenario

###### NO WINTER ####

ifish.nw=ifish%>%
  filter(season=="openwater")%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

ifish.nw=ifish.nw%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates.nw=iharv%>%
  filter(season=="openwater")%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst.nw=ifish.nw%>%
  left_join(harvestEstimates.nw)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp.nw=markHarvEst.nw%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp.nw[,c(1:11,15:17)],"exploitation_rates_noWinter_creel_data.csv",row.names = F)
# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
ang.exp.nw=ang.exp.nw[!is.na(ang.exp.nw$exp.rate) & ang.exp.nw$exp.rate<1.0,]
# creating survey-level exploitation rates
ang.exp.nw=ang.exp.nw%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

###### SUMMER ONLY ####

ifish.ma=ifish%>%
  filter(month%in%c(5:8))%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

ifish.ma=ifish.ma%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates.ma=iharv%>%
  filter(month%in%c(5:8))%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst.ma=ifish.ma%>%
  left_join(harvestEstimates.ma)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp.ma=markHarvEst.ma%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp.ma[,c(1:11,15:17)],"exploitation_rates_mayAug_creel_data.csv",row.names = F)
# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
ang.exp.ma=ang.exp.ma[!is.na(ang.exp.ma$exp.rate) & ang.exp.ma$exp.rate<1.0,]
# creating survey-level exploitation rates
ang.exp.ma=ang.exp.ma%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

# for each of these % reductions I'm revoving data based on visit fish seq no to simulate what would happen if that creel clear visit to the lake were removed.
###### 25% WEEKDAYS ####

rm.fish.seq.no=NA
rm.visit.fish.seq.no=NA
ifish.surv=unique(ifish$survey.seq.no)

set.seed(3)
for(i in 1:length(ifish.surv)){ # survey loop
  full=ifish[ifish$survey.seq.no==ifish.surv[i] & ifish$daytype=="weekday",]
  ms=unique(full$month)
  if(nrow(full)>0){
    for(m in 1:length(ms)){ # month loop
      visits=unique(full$visit.fish.seq.no[full$month==ms[m]])
      visit.rm=visits[c(round(runif(round(0.25*length(visits)),min=1,max = length(visits))))]
      t.rm=full$fish.data.seq.no[full$visit.fish.seq.no%in%visit.rm]# fish seq nos to remove that associated with the vist seq nos selected for removal
      
      rm.visit.fish.seq.no=c(rm.visit.fish.seq.no,visit.rm) # visit fish seq nos to use to reduce the interview, count, and fish data needed to get harvest estimates
      rm.fish.seq.no=c(rm.fish.seq.no,t.rm) # vector of fish to remove from individual fish data
      
    }
  }
}

ifish.25wd=ifish[!(ifish$fish.data.seq.no%in%rm.fish.seq.no),] # reduced individual fish data
iint.25wd=cint%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced interview data
icou.25wd=ccou%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced count data
ifishAg.25wd=cfish%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced aggregate fish data

iharv.25wd=calc_creel_harvest(creel_count_data = icou.25wd,
                              creel_int_data = iint.25wd,
                              creel_fish_data = ifishAg.25wd) # harvest estimates from the reduced data

wd25=ifish.25wd%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

wd25=wd25%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates.25wd=iharv.25wd%>%
  filter(species.code=="X22")%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst.25wd=wd25%>%
  left_join(harvestEstimates.25wd)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp.25wd=markHarvEst.25wd%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp.25wd[,c(1:11,15:17)],"exploitation_rates_weekday25removal_creel_data.csv",row.names = F)
# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
ang.exp.25wd=ang.exp.25wd[!is.na(ang.exp.25wd$exp.rate) & ang.exp.25wd$exp.rate<1.0,]
# creating survey-level exploitation rates
ang.exp.25wd=ang.exp.25wd%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))


###### 50% WEEKDAYS ####

rm.fish.seq.no=NA
rm.visit.fish.seq.no=NA
ifish.surv=unique(ifish$survey.seq.no)

set.seed(3)
for(i in 1:length(ifish.surv)){ # survey loop
  full=ifish[ifish$survey.seq.no==ifish.surv[i] & ifish$daytype=="weekday",]
  ms=unique(full$month)
  if(nrow(full)>0){
    for(m in 1:length(ms)){ # month loop
      visits=unique(full$visit.fish.seq.no[full$month==ms[m]])
      visit.rm=visits[c(round(runif(round(0.50*length(visits)),min=1,max = length(visits))))]
      t.rm=full$fish.data.seq.no[full$visit.fish.seq.no%in%visit.rm]# fish seq nos to remove that associated with the vist seq nos selected for removal
      
      rm.visit.fish.seq.no=c(rm.visit.fish.seq.no,visit.rm) # visit fish seq nos to use to reduce the interview, count, and fish data needed to get harvest estimates
      rm.fish.seq.no=c(rm.fish.seq.no,t.rm) # vector of fish to remove from individual fish data
      
    }
  }
}

ifish.50wd=ifish[!(ifish$fish.data.seq.no%in%rm.fish.seq.no),] # reduced individual fish data
iint.50wd=cint%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced interview data
icou.50wd=ccou%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced count data
ifishAg.50wd=cfish%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced aggregate fish data

iharv.50wd=calc_creel_harvest(creel_count_data = icou.50wd,
                              creel_int_data = iint.50wd,
                              creel_fish_data = ifishAg.50wd) # harvest estimates from the reduced data

wd50=ifish.50wd%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

wd50=wd50%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates.50wd=iharv.50wd%>%
  filter(species.code=="X22")%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst.50wd=wd50%>%
  left_join(harvestEstimates.50wd)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp.50wd=markHarvEst.50wd%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp.50wd[,c(1:11,15:17)],"exploitation_rates_weekday50removal_creel_data.csv",row.names = F)
# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
ang.exp.50wd=ang.exp.50wd[!is.na(ang.exp.50wd$exp.rate) & ang.exp.50wd$exp.rate<1.0,]

# creating survey-level exploitation rates
ang.exp.50wd=ang.exp.50wd%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

###### 50% WEEKENDS ####

rm.fish.seq.no=NA
rm.visit.fish.seq.no=NA
ifish.surv=unique(ifish$survey.seq.no)

set.seed(3)
for(i in 1:length(ifish.surv)){ # survey loop
  full=ifish[ifish$survey.seq.no==ifish.surv[i] & ifish$daytype=="weekdend",]
  ms=unique(full$month)
  if(nrow(full)>0){
    for(m in 1:length(ms)){ # month loop
      visits=unique(full$visit.fish.seq.no[full$month==ms[m]])
      visit.rm=visits[c(round(runif(round(0.50*length(visits)),min=1,max = length(visits))))]
      t.rm=full$fish.data.seq.no[full$visit.fish.seq.no%in%visit.rm]# fish seq nos to remove that associated with the vist seq nos selected for removal
      
      rm.visit.fish.seq.no=c(rm.visit.fish.seq.no,visit.rm) # visit fish seq nos to use to reduce the interview, count, and fish data needed to get harvest estimates
      rm.fish.seq.no=c(rm.fish.seq.no,t.rm) # vector of fish to remove from individual fish data
      
    }
  }
}

ifish.50we=ifish[!(ifish$fish.data.seq.no%in%rm.fish.seq.no),] # reduced individual fish data
iint.50we=cint%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced interview data
icou.50we=ccou%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced count data
ifishAg.50we=cfish%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced aggregate fish data

iharv.50we=calc_creel_harvest(creel_count_data = icou.50we,
                              creel_int_data = iint.50we,
                              creel_fish_data = ifishAg.50we) # harvest estimates from the reduced data

we50=ifish.50we%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

we50=we50%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates.50we=iharv.50we%>%
  filter(species.code=="X22")%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

markHarvEst.50we=we50%>%
  left_join(harvestEstimates.50we)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp.50we=markHarvEst.50we%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)
# writing this detailed dataset to a .csv for ashley and steph
# write.csv(ang.exp.50we[,c(1:11,15:17)],"exploitation_rates_weekend50removal_creel_data.csv",row.names = F)
# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
ang.exp.50we=ang.exp.50we[!is.na(ang.exp.50we$exp.rate) & ang.exp.50we$exp.rate<1.0,]

# creating survey-level exploitation rates
ang.exp.50we=ang.exp.50we%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

##### VIZ AND ANALYSIS ####

# some visualizations and analyses to see if exploitation rate differs between treatment types

# combining actual and reduced dataframes into one big one for plotting

ttrExp=rbind(cbind(ang.exp,treat=rep("actual",nrow(ang.exp))),
             cbind(ang.exp.nw, treat=rep("noWinter",nrow(ang.exp.nw))),
             cbind(ang.exp.ma, treat=rep("mayAug",nrow(ang.exp.ma))),
             cbind(ang.exp.25wd, treat=rep("wd25",nrow(ang.exp.25wd))),
             cbind(ang.exp.50wd, treat=rep("wd50",nrow(ang.exp.50wd))),
             cbind(ang.exp.50we, treat=rep("we50",nrow(ang.exp.50we))))

colnames(ttrExp)=c("survey.seq.no","exp.rate","exp.rate.sd","treat")

ggplot(ttrExp)+theme_classic()+
  geom_density(aes(x=log(exp.rate), fill=treat),alpha=0.2)
# data is non normally distributed, exp.rate is continuous and greater than 0, consider gamma or lognormal distribution for modeling

# temporal trends through time in exploitation rate, effort, harvest, and harvest rate
ggplot(ttrExp[ttrExp$treat=="actual",])+theme_classic()+
  geom_boxplot(aes(x=as.character(year),y=log(exp.rate)),position = position_dodge(width = 1), fill="grey")+
  labs(x="Year",y="Log(Exploitation Rate)")

ggplot(ceff)+theme_classic()+
  geom_boxplot(aes(x=as.character(year),y=log(total.effort)), position = position_dodge(width = 1), fill="grey")+
  labs(x="Year",y="Log(Total Effort)")

ggplot(charv[charv$species.code=="X22" & charv$wbic%in%ctwiWBIC.creel,])+theme_classic()+
  geom_boxplot(aes(x=as.character(year),y=log(spp.harvest)), position = position_dodge(width = 1), fill="grey")+
  labs(x="Year",y="Log(Harvest)")

ggplot(charv[charv$species.code=="X22" & charv$wbic%in%ctwiWBIC.creel,])+theme_classic()+
  geom_boxplot(aes(x=as.character(year),y=log(spp.harvest.rate)), position = position_dodge(width = 1), fill="grey")+
  labs(x="Year",y="Log(Harvest Rate)")

## LAKE SPECIFIC ANALYSES 
# look at survey-specific changes in resulting exploitation rate
# also going to pull some lake characteristic information in here if there are patterns in what lakes see big changes under reductions and what ones don't
# lake chars to consider: NR, C-ST, ST pop status, angler effort density, lake class, distance from pop center.

# if I'm going to account for chain lakes and the inflated exploitation estimates they can provide when walleye operate as one large population and not individual populations local to each lake in the chain, then I will want to use the FM chain code I believe and sub those in for WBIC in the analyses below.

# for now ignoring this issue other than to say that I do remove exploitation rates above 1

# I'm going to make a data frame that essentially compares exploitation rates on a lake by lake basis across each data scenario.

ttrExp=ttrExp%>%
  left_join(cserv[,2:5])

pdat=ttrExp[ttrExp$year%in%c(2015,2019,2022),]
ggplot(pdat)+theme_classic()+
  geom_pointrange(aes(x=paste(year,waterbody.name,sep = "_"), 
                      y=exp.rate, ymin=exp.rate-exp.rate.sd, 
                      ymax=exp.rate+exp.rate.sd, color=treat), 
                  position = position_dodge(width = 1))+
  coord_cartesian(ylim = c(0,0.2))+
  theme(axis.text.x = element_text(angle=45,hjust=1), legend.position = c(.75,.75))+
  labs(x="Lake-Year",y="Exploitation Rate (+/- 1 SD)",color="Scenario")+
  scale_color_viridis_d()

# metrics comparing differences in individual lake estimates

trLake=ttrExp
trLake$a.diff=NA
trLake$a.exceed=NA

for(i in 1:nrow(trLake)){
  trLake$a.diff[i]=trLake$exp.rate[trLake$treat=="actual" & trLake$survey.seq.no==trLake$survey.seq.no[i]]-trLake$exp.rate[i]
  trLake$a.exceed[i]=trLake$a.diff[i]>trLake$exp.rate.sd[trLake$treat=="actual" & trLake$survey.seq.no==trLake$survey.seq.no[i]]
}

ggplot(trLake)+theme_classic()+
  geom_point(aes(x=year,y=a.diff, color=treat)) # no obvious trend through time, probably a good thing!

ggplot(trLake)+theme_classic()+
   geom_boxplot(aes(y=a.diff, x=treat))


exceedSummary=trLake%>%
  group_by(treat)%>%
  summarise(nTrue=sum(a.exceed,na.rm = T),
            nFalse=sum(a.exceed==F,na.rm = T),
            nNA=sum(is.na(a.exceed)))

# bringing in some relvant lake characteristics to understand if there is any pattern in the magnitude of the difference from actual

# walleye recruitment class from lchar
# effort from ceff pooled to survey level
lEff=calc_creel_effort(creel_count_data = ccou,
                       creel_int_data = cint,
                       grouping = c("wbic","survey.seq.no","month","daytype"))
# current lake classes
lc=read.csv('lake class predictions.csv')
lc$LakeClass=gsub(" ","-",lc$LakeClass)

chars=lEff%>%
  group_by(wbic,survey.seq.no)%>%
  summarise(effort_hrs=sum(total.effort),
            effort_hrs.sd=sd(total.effort,na.rm=T))%>%
  left_join(lchar[,c(1,3,15,35)])%>%
  mutate(effortHrs.acre=effort_hrs/lake.area,
         effortHrs.acre.sd=sd(effort_hrs/lake.area,na.rm=T))%>%
  left_join(lc[,c(1,10)],by=c('wbic'='WBIC'))
trLake_chars=trLake%>%
  left_join(chars)

stClass=trLake_chars%>%
  group_by(wae.code,treat)%>%
  summarise(meanDiff=mean(a.diff,na.rm = T),
            sdDiff=sd(a.diff,na.rm=T))
ggplot(stClass)+theme_classic()+
  geom_pointrange(aes(x=wae.code,y=meanDiff,ymin=meanDiff-sdDiff,ymax=meanDiff+sdDiff,color=treat),position = position_dodge(width = 1))+
  scale_color_viridis_d()+theme(legend.position = c(0.7,0.3))+
  labs(x="WAE Recruitment Code",y="Mean Difference from Actual Exploitation Rate",color="Scenario")

ggplot(trLake_chars)+theme_classic()+
  geom_point(aes(y=a.diff,x=log(lake.area),color=treat))+
  scale_color_viridis_d()+theme(legend.position = c(0.8,0.4))+
  labs(x="Log(Lake Area in Acres)", y="Difference from Actual exploitation rate",color="Scenario")

ggplot(trLake_chars)+theme_classic()+
  geom_point(aes(y=a.diff,x=log(effortHrs.acre),color=treat))+
  scale_color_viridis_d()+theme(legend.position = c(0.8,0.4))+
  labs(x="Log(Effort/Acre)",y="Difference from Actual exploitation rate",color="Scenario")

lcSum=trLake_chars%>%
  group_by(LakeClass,treat)%>%
  summarise(meanDiff=mean(a.diff,na.rm = T),
            sdDiff=sd(a.diff,na.rm=T))
ggplot(lcSum)+theme_classic()+
  geom_pointrange(aes(x=LakeClass,y=meanDiff, ymin=meanDiff-sdDiff,ymax=meanDiff+sdDiff,color=treat),position = position_dodge(width = 1))+
  scale_color_viridis_d()+
  theme(axis.text.x = element_text(angle=45,hjust = 1), legend.position = c(0.8,0.25))+
  labs(x="Lake Class",y="Mean Difference from Actual Exploitation Rate", color="Scenario")



## BAYESIAN MODELING 
# this looks at whether or not the distributions of exploitaion rates are different from each other or not.
# one likelihood to estimate parms for using the 6 different treatments
# creating data frame to model with u rates that are NA removed, these are 6% of the data and are from years where creel happened but no fish were FN marked concurrently.
modDat=ttrExp[!is.na(ttrExp$exp.rate),]

#removing 0s since they can't be logged and if I were to make them a small number they would throw off the data and make it bimodal which would probably mean switching to a gamma or beta distribution to model the data. Could work, but I don't have time to mess with that right now. I'm going to operate under the assumption that if a survey give a 0 exploitation rate with the full survey, then us collecting less data isn't going to change that number. 
zeros.actual=sum(modDat$exp.rate[modDat$treat=="actual"]==0)
zeros.nw=sum(modDat$exp.rate[modDat$treat=="noWinter"]==0)
zeros.ma=sum(modDat$exp.rate[modDat$treat=="mayAug"]==0)
zeros.wd25=sum(modDat$exp.rate[modDat$treat=="wd25"]==0)
zeros.wd50=sum(modDat$exp.rate[modDat$treat=="wd50"]==0)
zeros.we50=sum(modDat$exp.rate[modDat$treat=="we50"]==0)

modDat=modDat[modDat$exp.rate!=0,]

#### ACTUAL ####
uLL.a=function(param){
  alpha=param[1]
  beta=param[2]

  us=rlnorm(nrow(modDat[modDat$treat=="actual",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="actual"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

# uLL.a=function(param){
#   alpha=param[1]
#   beta=param[2]
#   mu=mean(modDat$exp.rate[modDat$treat=="actual"],na.rm = T)
#   sigma2=var(modDat$exp.rate[modDat$treat=="actual"],na.rm = T)
# 
#   us=rbeta(nrow(modDat[modDat$treat=="actual",]), shape1 = alpha, shape2 = beta)
#   ll=dbeta(modDat$exp.rate[modDat$treat=="actual"], shape1 = (mu^2-mu^3-mu*sigma2)/sigma2, shape2 = (mu-2*mu^2+mu^3-sigma2+mu*sigma2)/sigma2, log = T)
#   return(sum(ll))
# }


# uLL.a=function(param){
#   shape=param[1]
#   rate=param[2]
#   mu=mean(modDat$exp.rate[modDat$treat=="actual"],na.rm = T)
#   sigma2=var(modDat$exp.rate[modDat$treat=="actual"],na.rm = T)
#   
#   us=rgamma(nrow(modDat[modDat$treat=="actual",]), shape = shape, rate = rate)
#   ll=dgamma(modDat$exp.rate[modDat$treat=="actual"], shape = mu^2/sigma2, rate = mu/sigma2, log = T)
#   return(sum(ll))
# }
# 
# 
# 
# prior=createUniformPrior(lower=c(0,0), upper = c(20,20))

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="actual"])),sd(log(modDat$exp.rate[modDat$treat=="actual"]))),
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.actual=createBayesianSetup(uLL.a, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)

startT=Sys.time()
set.seed(10)
expR.actual=runMCMC(bayesianSetup = setup.actual, sampler = "DEzs", settings = settings)
endT=Sys.time() # takes about 2 mins

#### NW ####
uLL.nw=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="noWinter",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="noWinter"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="noWinter"])),sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))), 
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.nw=createBayesianSetup(uLL.nw, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
set.seed(10)
expR.nw=runMCMC(bayesianSetup = setup.nw, sampler = "DEzs", settings = settings)

summary(expR.nw)
marginalPlot(expR.nw, prior = T)

#### MA ####

uLL.ma=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="mayAug",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="mayAug"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="mayAug"])),sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))), 
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.ma=createBayesianSetup(uLL.ma, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
set.seed(10)
expR.ma=runMCMC(bayesianSetup = setup.ma, sampler = "DEzs", settings = settings)

summary(expR.ma)
marginalPlot(expR.ma, prior = T)

#### WD25 ####

uLL.wd25=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="wd25",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="wd25"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="wd25"])),sd(log(modDat$exp.rate[modDat$treat=="wd25"]))), 
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.wd25=createBayesianSetup(uLL.wd25, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
set.seed(10)
expR.25wd=runMCMC(bayesianSetup = setup.wd25, sampler = "DEzs", settings = settings)

summary(expR.25wd)
marginalPlot(expR.25wd, prior = T)

#### WD50 ####
uLL.wd50=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="wd50",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="wd50"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="wd50"])),sd(log(modDat$exp.rate[modDat$treat=="wd50"]))), 
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.wd50=createBayesianSetup(uLL.wd50, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
set.seed(10)
expR.50wd=runMCMC(bayesianSetup = setup.wd50, sampler = "DEzs", settings = settings)

summary(expR.50wd)
marginalPlot(expR.50wd, prior = T)

#### WE50 ####
uLL.we50=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="we50",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="we50"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="we50"])),sd(log(modDat$exp.rate[modDat$treat=="we50"]))), 
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.we50=createBayesianSetup(uLL.we50, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
set.seed(10)
expR.50we=runMCMC(bayesianSetup = setup.we50, sampler = "DEzs", settings = settings)

summary(expR.50we)
marginalPlot(expR.50we, prior = T)



# code to summarize model outputs, I don't think I should be using bayes factor since the models are fit to different datasets so I'm not really trying to decide if one model fits the same data better than another. Unless I were to use the output parms from each scenario and fit them to the actual data and show that the fit is somehow worse than the full data model fit.

# looking to see if parm estimates produce data that visually at least looks like the observed data for that scenario
pars.a=getSample(expR.actual)
pars.nw=getSample(expR.nw)
pars.ma=getSample(expR.ma)
pars.wd25=getSample(expR.25wd)
pars.wd50=getSample(expR.50wd)
pars.we50=getSample(expR.50we)
set.seed(10)
aComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                         meanlog = median(pars.a[,1]),
                                                                         sdlog = median(pars.a[,2]))),
                    treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))
# aComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
#                                                                     shape1 = median(pars.a[,1]),
#                                                                     shape2 = median(pars.a[,2]))),
#                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))
nwComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="noWinter"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="noWinter"]),
                                                                    meanlog = median(pars.nw[,1]),
                                                                    sdlog = median(pars.nw[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="noWinter"])),rep("pred",length(modDat$exp.rate[modDat$treat=="noWinter"]))))

maComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="mayAug"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="mayAug"]),
                                                                    meanlog = median(pars.ma[,1]),
                                                                    sdlog = median(pars.ma[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="mayAug"])),rep("pred",length(modDat$exp.rate[modDat$treat=="mayAug"]))))

wd25Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="wd25"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd25"]),
                                                                    meanlog = median(pars.wd25[,1]),
                                                                    sdlog = median(pars.wd25[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="wd25"])),rep("pred",length(modDat$exp.rate[modDat$treat=="wd25"]))))

wd50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="wd50"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd50"]),
                                                                    meanlog = median(pars.wd50[,1]),
                                                                    sdlog = median(pars.wd50[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="wd50"])),rep("pred",length(modDat$exp.rate[modDat$treat=="wd50"]))))

we50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="we50"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="we50"]),
                                                                    meanlog = median(pars.we50[,1]),
                                                                    sdlog = median(pars.we50[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="we50"])),rep("pred",length(modDat$exp.rate[modDat$treat=="we50"]))))

a.p=ggplot(aComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "Actual", fill=element_blank())
nw.p=ggplot(nwComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "No Winter Data (April - October)", fill=element_blank())
ma.p=ggplot(maComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "May - August Data Only", fill=element_blank())
wd25.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "25% of Weekday Creel Visits/Month Removed", fill=element_blank())
wd50.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "50% of Weekday Creel Visits/Month Removed", fill=element_blank())
we50.p=ggplot(we50Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "50% of Weekdend Creel Visits/Month Removed", fill=element_blank())

ggarrange(a.p,nw.p,ma.p,wd25.p,wd50.p,we50.p, common.legend = T)

# does data simulated from the scenario-specific models look like the actual data? Only the actual and no winter plots should line up according to the ks tests below
set.seed(10)
aComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    meanlog = median(pars.a[,1]),
                                                                    sdlog = median(pars.a[,2]))),
                 treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

nwComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                       meanlog = median(pars.nw[,1]),
                                                                       sdlog = median(pars.nw[,2]))),
                  treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

maComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                     meanlog = median(pars.ma[,1]),
                                                                     sdlog = median(pars.ma[,2]))),
                  treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

wd25Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                     meanlog = median(pars.wd25[,1]),
                                                                     sdlog = median(pars.wd25[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

wd50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                     meanlog = median(pars.wd50[,1]),
                                                                     sdlog = median(pars.wd50[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

we50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                     meanlog = median(pars.we50[,1]),
                                                                     sdlog = median(pars.we50[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

a.p=ggplot(aComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "Actual", fill=element_blank())
nw.p=ggplot(nwComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "No Winter Data (April - October)", fill=element_blank())
ma.p=ggplot(maComp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "May - August Data Only", fill=element_blank())
wd25.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "25% of Weekday Creel Visits/Month Removed", fill=element_blank())
wd50.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "50% of Weekday Creel Visits/Month Removed", fill=element_blank())
we50.p=ggplot(we50Comp)+theme_classic()+
  geom_density(aes(x=log(u),fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Log(Exploitation Rate)", y="Density", title = "50% of Weekdend Creel Visits/Month Removed", fill=element_blank())

ggarrange(a.p,nw.p,ma.p,wd25.p,wd50.p,we50.p, common.legend = T) # shows models differ when you take data out and exploitation rates would probably increase on average if creel data collection was reduced.


### MODEL CHECKNG ####

gelmanDiagnostics(expR.actual, plot = T) # converged
gelmanDiagnostics(expR.nw, plot = T) # converged
gelmanDiagnostics(expR.ma, plot = T) # converged
gelmanDiagnostics(expR.25wd, plot = T) # converged
gelmanDiagnostics(expR.50wd, plot = T) # converged
gelmanDiagnostics(expR.50we, plot = T) # converged

# bayesian p-value for each model

#### Pb ACTUAL ####
pval.actual=data.frame(alpha=rep(NA,nrow(pars.a)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.a)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                 meanlog = pars.a[i,1],
                 sdlog = pars.a[i,2])
  pval.actual$alpha[i]=pars.a[i,1]
  pval.actual$beta[i]=pars.a[i,2]
  pval.actual$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.actual$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.actual$cvExceed=0
pval.actual$sdExceed=0

actual.cv=sd(log(modDat$exp.rate[modDat$treat=="actual"]))/mean(log(modDat$exp.rate[modDat$treat=="actual"]))
actual.sd=sd(log(modDat$exp.rate[modDat$treat=="actual"]))

pval.actual$cvExceed[pval.actual$cv>actual.cv]=1
pval.actual$sdExceed[pval.actual$sd>actual.sd]=1

sum(pval.actual$cvExceed==1)/nrow(pval.actual) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.actual$sdExceed==1)/nrow(pval.actual) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

#### Pb NO WINTER ####
pval.noWinter=data.frame(alpha=rep(NA,nrow(pars.nw)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.nw)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="noWinter"]),
                 meanlog = pars.nw[i,1],
                 sdlog = pars.nw[i,2])
  pval.noWinter$alpha[i]=pars.nw[i,1]
  pval.noWinter$beta[i]=pars.nw[i,2]
  pval.noWinter$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.noWinter$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.noWinter$cvExceed=0
pval.noWinter$sdExceed=0

noWinter.cv=sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))/mean(log(modDat$exp.rate[modDat$treat=="noWinter"]))
noWinter.sd=sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))

pval.noWinter$cvExceed[pval.noWinter$cv>noWinter.cv]=1
pval.noWinter$sdExceed[pval.noWinter$sd>noWinter.sd]=1

sum(pval.noWinter$cvExceed==1)/nrow(pval.noWinter) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.noWinter$sdExceed==1)/nrow(pval.noWinter) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

#### Pb MAYAUGUST ####
pval.mayAug=data.frame(alpha=rep(NA,nrow(pars.ma)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.ma)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="mayAug"]),
                 meanlog = pars.ma[i,1],
                 sdlog = pars.ma[i,2])
  pval.mayAug$alpha[i]=pars.ma[i,1]
  pval.mayAug$beta[i]=pars.ma[i,2]
  pval.mayAug$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.mayAug$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.mayAug$cvExceed=0
pval.mayAug$sdExceed=0

mayAug.cv=sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))/mean(log(modDat$exp.rate[modDat$treat=="mayAug"]))
mayAug.sd=sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))

pval.mayAug$cvExceed[pval.mayAug$cv>mayAug.cv]=1
pval.mayAug$sdExceed[pval.mayAug$sd>mayAug.sd]=1

sum(pval.mayAug$cvExceed==1)/nrow(pval.mayAug) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.mayAug$sdExceed==1)/nrow(pval.mayAug) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

#### Pb WD25 ####
pval.wd25=data.frame(alpha=rep(NA,nrow(pars.wd25)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.wd25)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd25"]),
                 meanlog = pars.wd25[i,1],
                 sdlog = pars.wd25[i,2])
  pval.wd25$alpha[i]=pars.wd25[i,1]
  pval.wd25$beta[i]=pars.wd25[i,2]
  pval.wd25$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.wd25$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd25$cvExceed=0
pval.wd25$sdExceed=0

wd25.cv=sd(log(modDat$exp.rate[modDat$treat=="wd25"]))/mean(log(modDat$exp.rate[modDat$treat=="wd25"]))
wd25.sd=sd(log(modDat$exp.rate[modDat$treat=="wd25"]))

pval.wd25$cvExceed[pval.wd25$cv>wd25.cv]=1
pval.wd25$sdExceed[pval.wd25$sd>wd25.sd]=1

sum(pval.wd25$cvExceed==1)/nrow(pval.wd25) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.wd25$sdExceed==1)/nrow(pval.wd25) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

#### Pb WD50 ####
pval.wd50=data.frame(alpha=rep(NA,nrow(pars.wd50)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.wd50)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd50"]),
                 meanlog = pars.wd50[i,1],
                 sdlog = pars.wd50[i,2])
  pval.wd50$alpha[i]=pars.wd50[i,1]
  pval.wd50$beta[i]=pars.wd50[i,2]
  pval.wd50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.wd50$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd50$cvExceed=0
pval.wd50$sdExceed=0

wd50.cv=sd(log(modDat$exp.rate[modDat$treat=="wd50"]))/mean(log(modDat$exp.rate[modDat$treat=="wd50"]))
wd50.sd=sd(log(modDat$exp.rate[modDat$treat=="wd50"]))

pval.wd50$cvExceed[pval.wd50$cv>wd50.cv]=1
pval.wd50$sdExceed[pval.wd50$sd>wd50.sd]=1

sum(pval.wd50$cvExceed==1)/nrow(pval.wd50) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.wd50$sdExceed==1)/nrow(pval.wd50) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

#### Pb WE50 ####
pval.we50=data.frame(alpha=rep(NA,nrow(pars.we50)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.we50)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="we50"]),
                 meanlog = pars.we50[i,1],
                 sdlog = pars.we50[i,2])
  pval.we50$alpha[i]=pars.we50[i,1]
  pval.we50$beta[i]=pars.we50[i,2]
  pval.we50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.we50$sd[i]=sd(log(tempdat))
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.we50$cvExceed=0
pval.we50$sdExceed=0

we50.cv=sd(log(modDat$exp.rate[modDat$treat=="we50"]))/mean(log(modDat$exp.rate[modDat$treat=="we50"]))
we50.sd=sd(log(modDat$exp.rate[modDat$treat=="we50"]))

pval.we50$cvExceed[pval.we50$cv>we50.cv]=1
pval.we50$sdExceed[pval.we50$sd>we50.sd]=1

sum(pval.we50$cvExceed==1)/nrow(pval.we50) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal
sum(pval.we50$sdExceed==1)/nrow(pval.we50) # if this value is < 0.1 for >0.9 that would suggest model is not fitting well. 0.5 is ideal

## p-value tests comparing the scenarios to actual to show that some scenarios approximate the actual quite well.

bpval.comp=data.frame(scenario=c("Actual", "No Winter", "May-August","25% weeday removal","50% weekday removal","50% weekend removal"),
                      coef.var.pval=NA,
                      sd.pval=NA)
pval.actual$cvComp=0
pval.actual$sdComp=0
pval.actual$cvComp[pval.actual$cv>actual.cv]=1
pval.actual$sdComp[pval.actual$sd>actual.sd]=1

pval.noWinter$cvComp=0
pval.noWinter$sdComp=0
pval.noWinter$cvComp[pval.noWinter$cv>actual.cv]=1
pval.noWinter$sdComp[pval.noWinter$sd>actual.sd]=1

pval.mayAug$cvComp=0
pval.mayAug$sdComp=0
pval.mayAug$cvComp[pval.mayAug$cv>actual.cv]=1
pval.mayAug$sdComp[pval.mayAug$sd>actual.sd]=1

pval.wd25$cvComp=0
pval.wd25$sdComp=0
pval.wd25$cvComp[pval.wd25$cv>actual.cv]=1
pval.wd25$sdComp[pval.wd25$sd>actual.sd]=1

pval.wd50$cvComp=0
pval.wd50$sdComp=0
pval.wd50$cvComp[pval.wd50$cv>actual.cv]=1
pval.wd50$sdComp[pval.wd50$sd>actual.sd]=1

pval.we50$cvComp=0
pval.we50$sdComp=0
pval.we50$cvComp[pval.we50$cv>actual.cv]=1
pval.we50$sdComp[pval.we50$sd>actual.sd]=1

bpval.comp$coef.var.pval=c(sum(pval.actual$cvComp)/nrow(pval.actual),
                           sum(pval.noWinter$cvComp)/nrow(pval.noWinter),
                           sum(pval.mayAug$cvComp)/nrow(pval.mayAug),
                           sum(pval.wd25$cvComp)/nrow(pval.wd25),
                           sum(pval.wd50$cvComp)/nrow(pval.wd50),
                           sum(pval.we50$cvComp)/nrow(pval.we50))

bpval.comp$sd.pval=c(sum(pval.actual$sdComp)/nrow(pval.actual),
                           sum(pval.noWinter$sdComp)/nrow(pval.noWinter),
                           sum(pval.mayAug$sdComp)/nrow(pval.mayAug),
                           sum(pval.wd25$sdComp)/nrow(pval.wd25),
                           sum(pval.wd50$sdComp)/nrow(pval.wd50),
                           sum(pval.we50$sdComp)/nrow(pval.we50))

# ## some kolmogorov smirnov tests as separate line of evidence
# actual.nw=ks.test(ttrExp$exp.rate[ttrExp$treat=="actual"],ttrExp$exp.rate[ttrExp$treat=="noWinter"]) # no sig
# actual.ma=ks.test(ttrExp$exp.rate[ttrExp$treat=="actual"],ttrExp$exp.rate[ttrExp$treat=="mayAug"]) # sig diff
# actual.we50=ks.test(ttrExp$exp.rate[ttrExp$treat=="actual"],ttrExp$exp.rate[ttrExp$treat=="we50"]) # no sig diff
# actual.wd25=ks.test(ttrExp$exp.rate[ttrExp$treat=="actual"],ttrExp$exp.rate[ttrExp$treat=="wd25"]) # no sig diff
# actual.wd50=ks.test(ttrExp$exp.rate[ttrExp$treat=="actual"],ttrExp$exp.rate[ttrExp$treat=="wd50"]) # no sig diff
# 
# # friedman test could be better as an n-way nonparametric analysis of variance in this case if I want to get deeper into this frequentist based analysis


# MODELING EFFECTS OF DATA REDUCTION ON INDIVIDUAL YEARS

# Tom and Joe expressed concern that even though the whole population distribution of exploitation rates doesn't change much there could be meaningful effects on the u estimates for individual lakes

# I think what I need to do is loop through each year and model just that year's data, calculate the p-values and store that information

# first a couple exploratory plots of u distribution by year

ggplot(ttrExp)+theme_classic()+
  geom_density(aes(log(exp.rate),fill=treat),alpha=0.2)+
  facet_wrap(~year,scales = 'free_y')+scale_fill_viridis_d()

# likelihoods to fit
uLL.we50=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="we50",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="we50"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
uLL.wd50=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="wd50",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="wd50"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
uLL.wd25=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="wd25",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="wd25"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
uLL.ma=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="mayAug",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="mayAug"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
uLL.nw=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="noWinter",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="noWinter"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
uLL.a=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(tdat[tdat$treat=="actual",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(tdat$exp.rate[tdat$treat=="actual"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}

loopY=sort(unique(ttrExp$year))

bpval.comp.y=data.frame(year=NA,
                      scenario=NA,
                      coef.var.pval=NA,
                      sd.pval=NA)

bpval.self.y=data.frame(year=NA,
                        scenario=NA,
                        coef.var.pval=NA,
                        sd.pval=NA)

for(y in 1:length(loopY)){
  #first get to year-specific data
  tdat=ttrExp[ttrExp$year==loopY[y],]
  # removing 0s,
  tdat=tdat[tdat$exp.rate!=0,]
  # Bayesian Model Fitting
  #ACTUAL

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="actual"])),sd(log(modDat$exp.rate[modDat$treat=="actual"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.a=createBayesianSetup(uLL.a, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.a=runMCMC(bayesianSetup = setup.a, sampler = "DEzs", settings = settings)
  #NW

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="noWinter"])),sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.nw=createBayesianSetup(uLL.nw, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.nw=runMCMC(bayesianSetup = setup.nw, sampler = "DEzs", settings = settings)
  #MA

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="mayAug"])),sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.ma=createBayesianSetup(uLL.ma, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.ma=runMCMC(bayesianSetup = setup.ma, sampler = "DEzs", settings = settings)
  #WD.25

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="wd25"])),sd(log(modDat$exp.rate[modDat$treat=="wd25"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.wd25=createBayesianSetup(uLL.wd25, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.25wd=runMCMC(bayesianSetup = setup.wd25, sampler = "DEzs", settings = settings)
  #WD.50

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="wd50"])),sd(log(modDat$exp.rate[modDat$treat=="wd50"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.wd50=createBayesianSetup(uLL.wd50, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.50wd=runMCMC(bayesianSetup = setup.wd50, sampler = "DEzs", settings = settings)
  
  #WE.50

  prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="we50"])),sd(log(modDat$exp.rate[modDat$treat=="we50"]))), 
                                   sd=c(1,1),
                                   lower = c(-15,0),
                                   upper = c(15,5))
  
  setup.we50=createBayesianSetup(uLL.we50, prior = prior)
  
  settings=list(iterations=10000, nrChains=3, message=F, burnin=5000)
  set.seed(10)
  t.50we=runMCMC(bayesianSetup = setup.we50, sampler = "DEzs", settings = settings)
  
  ## BAYESIAN P-VALUE CALCS
  
  pars.a=getSample(t.a)
  pars.nw=getSample(t.nw)
  pars.ma=getSample(t.ma)
  pars.wd25=getSample(t.25wd)
  pars.wd50=getSample(t.50wd)
  pars.we50=getSample(t.50we)
  
  #### Pb ACTUAL ####
  pval.actual=data.frame(alpha=rep(NA,nrow(pars.a)),
                         beta=NA,
                         cv=NA,
                         sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.a)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="actual"]),
                   meanlog = pars.a[i,1],
                   sdlog = pars.a[i,2])
    pval.actual$alpha[i]=pars.a[i,1]
    pval.actual$beta[i]=pars.a[i,2]
    pval.actual$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.actual$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.actual$cvExceed=0
  pval.actual$sdExceed=0
  
  actual.cv=sd(log(tdat$exp.rate[tdat$treat=="actual"]))/mean(log(tdat$exp.rate[tdat$treat=="actual"]))
  actual.sd=sd(log(tdat$exp.rate[tdat$treat=="actual"]))
  
  pval.actual$cvExceed[pval.actual$cv>actual.cv]=1
  pval.actual$sdExceed[pval.actual$sd>actual.sd]=1
  
  #### Pb NO WINTER ####
  pval.noWinter=data.frame(alpha=rep(NA,nrow(pars.nw)),
                           beta=NA,
                           cv=NA,
                           sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.nw)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="noWinter"]),
                   meanlog = pars.nw[i,1],
                   sdlog = pars.nw[i,2])
    pval.noWinter$alpha[i]=pars.nw[i,1]
    pval.noWinter$beta[i]=pars.nw[i,2]
    pval.noWinter$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.noWinter$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.noWinter$cvExceed=0
  pval.noWinter$sdExceed=0
  
  noWinter.cv=sd(log(tdat$exp.rate[tdat$treat=="noWinter"]))/mean(log(tdat$exp.rate[tdat$treat=="noWinter"]))
  noWinter.sd=sd(log(tdat$exp.rate[tdat$treat=="noWinter"]))
  
  pval.noWinter$cvExceed[pval.noWinter$cv>noWinter.cv]=1
  pval.noWinter$sdExceed[pval.noWinter$sd>noWinter.sd]=1
  
  #### Pb MAYAUGUST ####
  pval.mayAug=data.frame(alpha=rep(NA,nrow(pars.ma)),
                         beta=NA,
                         cv=NA,
                         sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.ma)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="mayAug"]),
                   meanlog = pars.ma[i,1],
                   sdlog = pars.ma[i,2])
    pval.mayAug$alpha[i]=pars.ma[i,1]
    pval.mayAug$beta[i]=pars.ma[i,2]
    pval.mayAug$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.mayAug$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.mayAug$cvExceed=0
  pval.mayAug$sdExceed=0
  
  mayAug.cv=sd(log(tdat$exp.rate[tdat$treat=="mayAug"]))/mean(log(tdat$exp.rate[tdat$treat=="mayAug"]))
  mayAug.sd=sd(log(tdat$exp.rate[tdat$treat=="mayAug"]))
  
  pval.mayAug$cvExceed[pval.mayAug$cv>mayAug.cv]=1
  pval.mayAug$sdExceed[pval.mayAug$sd>mayAug.sd]=1
  
  #### Pb WD25 ####
  pval.wd25=data.frame(alpha=rep(NA,nrow(pars.wd25)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.wd25)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="wd25"]),
                   meanlog = pars.wd25[i,1],
                   sdlog = pars.wd25[i,2])
    pval.wd25$alpha[i]=pars.wd25[i,1]
    pval.wd25$beta[i]=pars.wd25[i,2]
    pval.wd25$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.wd25$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.wd25$cvExceed=0
  pval.wd25$sdExceed=0
  
  wd25.cv=sd(log(tdat$exp.rate[tdat$treat=="wd25"]))/mean(log(tdat$exp.rate[tdat$treat=="wd25"]))
  wd25.sd=sd(log(tdat$exp.rate[tdat$treat=="wd25"]))
  
  pval.wd25$cvExceed[pval.wd25$cv>wd25.cv]=1
  pval.wd25$sdExceed[pval.wd25$sd>wd25.sd]=1
  
  #### Pb WD50 ####
  pval.wd50=data.frame(alpha=rep(NA,nrow(pars.wd50)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.wd50)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="wd50"]),
                   meanlog = pars.wd50[i,1],
                   sdlog = pars.wd50[i,2])
    pval.wd50$alpha[i]=pars.wd50[i,1]
    pval.wd50$beta[i]=pars.wd50[i,2]
    pval.wd50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.wd50$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.wd50$cvExceed=0
  pval.wd50$sdExceed=0
  
  wd50.cv=sd(log(tdat$exp.rate[tdat$treat=="wd50"]))/mean(log(tdat$exp.rate[tdat$treat=="wd50"]))
  wd50.sd=sd(log(tdat$exp.rate[tdat$treat=="wd50"]))
  
  pval.wd50$cvExceed[pval.wd50$cv>wd50.cv]=1
  pval.wd50$sdExceed[pval.wd50$sd>wd50.sd]=1
  
  #### Pb WE50 ####
  pval.we50=data.frame(alpha=rep(NA,nrow(pars.we50)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
  set.seed(10)
  for(i in 1:nrow(pars.we50)){
    tempdat=rlnorm(n=length(tdat$exp.rate[tdat$treat=="we50"]),
                   meanlog = pars.we50[i,1],
                   sdlog = pars.we50[i,2])
    pval.we50$alpha[i]=pars.we50[i,1]
    pval.we50$beta[i]=pars.we50[i,2]
    pval.we50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
    pval.we50$sd[i]=sd(log(tempdat))
  }
  
  # now calculate the number of times the cv or sd exceeds that of the real data
  
  pval.we50$cvExceed=0
  pval.we50$sdExceed=0
  
  we50.cv=sd(log(tdat$exp.rate[tdat$treat=="we50"]))/mean(log(tdat$exp.rate[tdat$treat=="we50"]))
  we50.sd=sd(log(tdat$exp.rate[tdat$treat=="we50"]))
  
  pval.we50$cvExceed[pval.we50$cv>we50.cv]=1
  pval.we50$sdExceed[pval.we50$sd>we50.sd]=1
  
  #df to hold pvals for model comparison to self, a way of knowing the model fit the data well
  t.pself=data.frame(year=rep(loopY[y],6),
                     scenario=c("Actual", "No Winter", "May-August","25% weeday removal","50% weekday removal","50% weekend removal"),
                     coef.var.pval=NA,
                     sd.pval=NA)
  # adding self comparison pvals
  t.pself$coef.var.pval=c(sum(pval.actual$cvExceed)/nrow(pval.actual),
                          sum(pval.noWinter$cvExceed)/nrow(pval.noWinter),
                          sum(pval.mayAug$cvExceed)/nrow(pval.mayAug),
                          sum(pval.wd25$cvExceed)/nrow(pval.wd25),
                          sum(pval.wd50$cvExceed)/nrow(pval.wd50),
                          sum(pval.we50$cvExceed)/nrow(pval.we50))
  t.pself$sd.pval=c(sum(pval.actual$sdExceed)/nrow(pval.actual),
                          sum(pval.noWinter$sdExceed)/nrow(pval.noWinter),
                          sum(pval.mayAug$sdExceed)/nrow(pval.mayAug),
                          sum(pval.wd25$sdExceed)/nrow(pval.wd25),
                          sum(pval.wd50$sdExceed)/nrow(pval.wd50),
                          sum(pval.we50$sdExceed)/nrow(pval.we50))
  bpval.self.y=rbind(bpval.self.y,t.pself)
  ## p-value tests comparing the scenarios to actual to show that some scenarios approximate the actual quite well.
  
  t.pcomp=data.frame(year=rep(loopY[y],6),
                     scenario=c("Actual", "No Winter", "May-August","25% weeday removal","50% weekday removal","50% weekend removal"),
                     coef.var.pval=NA,
                     sd.pval=NA)
  
  pval.actual$cvComp=0
  pval.actual$sdComp=0
  pval.actual$cvComp[pval.actual$cv>actual.cv]=1
  pval.actual$sdComp[pval.actual$sd>actual.sd]=1
  
  pval.noWinter$cvComp=0
  pval.noWinter$sdComp=0
  pval.noWinter$cvComp[pval.noWinter$cv>actual.cv]=1
  pval.noWinter$sdComp[pval.noWinter$sd>actual.sd]=1
  
  pval.mayAug$cvComp=0
  pval.mayAug$sdComp=0
  pval.mayAug$cvComp[pval.mayAug$cv>actual.cv]=1
  pval.mayAug$sdComp[pval.mayAug$sd>actual.sd]=1
  
  pval.wd25$cvComp=0
  pval.wd25$sdComp=0
  pval.wd25$cvComp[pval.wd25$cv>actual.cv]=1
  pval.wd25$sdComp[pval.wd25$sd>actual.sd]=1
  
  pval.wd50$cvComp=0
  pval.wd50$sdComp=0
  pval.wd50$cvComp[pval.wd50$cv>actual.cv]=1
  pval.wd50$sdComp[pval.wd50$sd>actual.sd]=1
  
  pval.we50$cvComp=0
  pval.we50$sdComp=0
  pval.we50$cvComp[pval.we50$cv>actual.cv]=1
  pval.we50$sdComp[pval.we50$sd>actual.sd]=1
  
  t.pcomp$coef.var.pval=c(sum(pval.actual$cvComp)/nrow(pval.actual),
                             sum(pval.noWinter$cvComp)/nrow(pval.noWinter),
                             sum(pval.mayAug$cvComp)/nrow(pval.mayAug),
                             sum(pval.wd25$cvComp)/nrow(pval.wd25),
                             sum(pval.wd50$cvComp)/nrow(pval.wd50),
                             sum(pval.we50$cvComp)/nrow(pval.we50))
  
  t.pcomp$sd.pval=c(sum(pval.actual$sdComp)/nrow(pval.actual),
                       sum(pval.noWinter$sdComp)/nrow(pval.noWinter),
                       sum(pval.mayAug$sdComp)/nrow(pval.mayAug),
                       sum(pval.wd25$sdComp)/nrow(pval.wd25),
                       sum(pval.wd50$sdComp)/nrow(pval.wd50),
                       sum(pval.we50$sdComp)/nrow(pval.we50))
  bpval.comp.y=rbind(bpval.comp.y,t.pcomp)
}

# now to look at the output
bpval.self.y=bpval.self.y[!is.na(bpval.self.y$year),]
bpval.comp.y=bpval.comp.y[!is.na(bpval.comp.y$year),]

#model fit 
ggplot(bpval.self.y)+theme_classic()+
  geom_point(aes(x=year, y=coef.var.pval,color=scenario))+
  geom_hline(yintercept = c(0.9,0.5,0.1),color="red",linetype=c(2,1,2))+
  scale_color_viridis_d()+
  coord_cartesian(ylim=c(0,1))

ggplot(bpval.self.y)+theme_classic()+
  geom_point(aes(x=year, y=coef.var.pval))+facet_wrap(~scenario)+
  geom_hline(yintercept = c(0.9,0.5,0.1),color="red")+
  coord_cartesian(ylim=c(0,1))

ggplot(bpval.self.y)+theme_classic()+
  geom_point(aes(x=year, y=sd.pval,color=scenario))+
  geom_hline(yintercept = c(0.9,0.5,0.1),color="red",linetype=c(2,1,2))+
  scale_color_viridis_d()+
  coord_cartesian(ylim=c(0,1))


# comparison to actual
ggplot(bpval.comp.y)+theme_classic()+
  geom_point(aes(x=year, y=coef.var.pval,color=scenario))+
  geom_hline(yintercept = c(0.9,0.5,0.1),color="red",linetype=c(2,1,2))+
  scale_color_viridis_d()+
  coord_cartesian(ylim=c(0,1))

ggplot(bpval.comp.y)+theme_classic()+
  geom_point(aes(x=year, y=sd.pval,color=scenario))+
  geom_hline(yintercept = c(0.9,0.5,0.1),color="red",linetype=c(2,1,2))+
  scale_color_viridis_d()+
  coord_cartesian(ylim=c(0,1))


# from steph, are angler harvest, effort, exploitation rate, etc. related to tribal harvest? Does knowing tribal harvest occurred draw anglers in or keep them away? Look at a few tribal harvest metrics, harvest/acre, % of declaration filled, etc.