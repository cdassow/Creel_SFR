# work on fitting beta and gamma distributions to the creel data instead of the lognormal distribution since I think it will help me better incorporate 0's
# still working on exploitation rate data only
# CJD 1.31.2024


rm(list=ls())
library(wdnr.fmdb)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(BayesianTools)
setwd("C:/Users/dassocju/Documents/OAS_git_repos/Creel_SFR")

call=readRDS("creelDataSet_all.RData")
# unlist, etc. to get back to individual dfs
cserv=call[[1]]
cvis=call[[2]]
ccou=call[[3]]
cint=call[[4]]
cfish=call[[5]]
cfish.i=call[[6]]

ceff=calc_creel_effort(creel_count_data=ccou,creel_int_data=cint) # don't use the grouping argument here, seems to break it

charv=calc_creel_harvest(creel_count_data = ccou,creel_int_data = cint,creel_fish_data = cfish) # don't use the grouping argument here, seems to break it

charvR=calc_creel_harvest_rates(cfish)

## CALC EXPLOITATION RATES ####
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

ang.exp=ang.exp%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

## CREATING REDUCED DATASETS ####
# now I have 'actual' exploitation rates

# next I want to make a few scenarios with 'reduced' data, a 'noWinter' scenario, 'May-August' scenario, and 3 % removals (0.25 weekdays, 0.5 weekdays, 0.5 weekend)

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


## FITTING ALTERNATIVE DISTRIBUTIONS ####
# combining actual and reduced dataframes into one big one for plotting

ttrExp=rbind(cbind(ang.exp,treat=rep("actual",nrow(ang.exp))),
             cbind(ang.exp.nw, treat=rep("noWinter",nrow(ang.exp.nw))),
             cbind(ang.exp.ma, treat=rep("mayAug",nrow(ang.exp.ma))),
             cbind(ang.exp.25wd, treat=rep("wd25",nrow(ang.exp.25wd))),
             cbind(ang.exp.50wd, treat=rep("wd50",nrow(ang.exp.50wd))),
             cbind(ang.exp.50we, treat=rep("we50",nrow(ang.exp.50we))))

colnames(ttrExp)=c("survey.seq.no","exp.rate","exp.rate.sd","treat")

ggplot(ttrExp)+theme_classic()+
  geom_density(aes(x=exp.rate, fill=treat),alpha=0.2)

modDat=ttrExp[ttrExp$exp.rate!=0,] # version of the above without 0s, models don't fit with 0s in there

uLL.a.beta=function(param){
  alpha=param[1]
  beta=param[2]

  us=rbeta(nrow(modDat[modDat$treat=="actual",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="actual"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.actual=createBayesianSetup(uLL.a.beta, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

startT=Sys.time()
set.seed(10)
expR.actual.beta=runMCMC(bayesianSetup = setup.actual, sampler = "DEzs", settings = settings)
endT=Sys.time() # takes about 1 sec


uLL.a.gamma=function(param){
  shape=param[1]
  rate=param[2]

  us=rgamma(nrow(modDat[modDat$treat=="actual",]), shape = shape, rate = rate)
  mu=mean(us,na.rm = T)
  sigma2=var(us,na.rm = T)
  
  ll=dgamma(modDat$exp.rate[modDat$treat=="actual"], shape = mu^2/sigma2, rate = mu/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.actual=createBayesianSetup(uLL.a.gamma, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

startT=Sys.time()
set.seed(10)
expR.actual.gamma=runMCMC(bayesianSetup = setup.actual, sampler = "DEzs", settings = settings)
endT=Sys.time() # takes about 2 sec

uLL.a.lnorm=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rlnorm(nrow(modDat[modDat$treat=="actual",]),meanlog = alpha, sdlog = beta)
  ll=dlnorm(modDat$exp.rate[modDat$treat=="actual"], meanlog = mean(log(us)), sdlog = sd(log(us)), log = T)
  return(sum(ll))
}
prior=createTruncatedNormalPrior(mean=c(mean(log(modDat$exp.rate[modDat$treat=="actual"])),sd(log(modDat$exp.rate[modDat$treat=="actual"]))),
                                 sd=c(1,1),
                                 lower = c(-15,0),
                                 upper = c(15,5))

setup.actual=createBayesianSetup(uLL.a.lnorm, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

startT=Sys.time()
set.seed(10)
expR.actual.lnorm=runMCMC(bayesianSetup = setup.actual, sampler = "DEzs", settings = settings)
endT=Sys.time() # takes about 2 seconds

# evaluating model fit

pars.a.lnorm=getSample(expR.actual.lnorm)
pars.a.beta=getSample(expR.actual.beta)
pars.a.gamma=getSample(expR.actual.gamma)

set.seed(10)
aComp1=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    meanlog = median(pars.a.lnorm[,1]),
                                                                    sdlog = median(pars.a.lnorm[,2]))),
                 treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))
aComp2=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.a.beta[,1]),
                                                                    shape2 = median(pars.a.beta[,2]))),
                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))
aComp3=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rgamma(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape = median(pars.a.gamma[,1]),
                                                                    rate = median(pars.a.gamma[,2]))),
                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))

ln=ggplot(aComp1)+theme_classic()+
  geom_density(aes(x=u, fill=treat),alpha=0.3)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate",title = "Lognormal")+
  theme(legend.position = c(0.75,0.75))
bta=ggplot(aComp2)+theme_classic()+
  geom_density(aes(x=u, fill=treat),alpha=0.3)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate",title = "Beta")+
  theme(legend.position = c(0.75,0.75))
gma=ggplot(aComp2)+theme_classic()+
  geom_density(aes(x=u, fill=treat),alpha=0.3)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate",title = "Gamma")+
  theme(legend.position = c(0.75,0.75))
ggarrange(ln,bta,gma)

## I think based on the definitions of the lognormal, gamma, and beta distributions that beta best fits with the type of data I'm dealing with. Though the analysis above would suggest there isn't really a difference between them in terms of results.

# working through rest of the reduced data sets and then calculating pvalues
#### NW ####
uLL.nw=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rbeta(nrow(modDat[modDat$treat=="noWinter",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="noWinter"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.nw=createBayesianSetup(uLL.nw, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

set.seed(10)
expR.nw=runMCMC(bayesianSetup = setup.nw, sampler = "DEzs", settings = settings)

summary(expR.nw)
marginalPlot(expR.nw, prior = T)

#### MA ####

uLL.ma=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rbeta(nrow(modDat[modDat$treat=="mayAug",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="mayAug"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.ma=createBayesianSetup(uLL.ma, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

set.seed(10)
expR.ma=runMCMC(bayesianSetup = setup.ma, sampler = "DEzs", settings = settings)

summary(expR.ma)
marginalPlot(expR.ma, prior = T)

#### WD25 ####

uLL.wd25=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rbeta(nrow(modDat[modDat$treat=="wd25",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="wd25"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
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
  
  us=rbeta(nrow(modDat[modDat$treat=="wd50",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="wd50"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.wd50=createBayesianSetup(uLL.wd50, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

set.seed(10)
expR.50wd=runMCMC(bayesianSetup = setup.wd50, sampler = "DEzs", settings = settings)

summary(expR.50wd)
marginalPlot(expR.50wd, prior = T)

#### WE50 ####
uLL.we50=function(param){
  alpha=param[1]
  beta=param[2]
  
  us=rbeta(nrow(modDat[modDat$treat=="we50",]), shape1 = alpha, shape2 = beta)
  mu=mean(us)
  sigma2=var(us)
  
  ll=dbeta(modDat$exp.rate[modDat$treat=="we50"], shape1 = ((mu^2)-(mu^3)-(mu*sigma2))/sigma2, shape2 = (mu-(2*(mu^2))+(mu^3)-sigma2+(mu*sigma2))/sigma2, log = T)
  return(sum(ll))
}

prior=createUniformPrior(lower=c(0,0), upper = c(200,200))
setup.we50=createBayesianSetup(uLL.we50, prior = prior)

settings=list(iterations=10000, nrChains=3, message=F, burnin=1000)

set.seed(10)
expR.50we=runMCMC(bayesianSetup = setup.we50, sampler = "DEzs", settings = settings)

summary(expR.50we)
marginalPlot(expR.50we, prior = T)

# looking to see if parm estimates produce data that visually at least looks like the observed data for that scenario
pars.a=getSample(expR.actual.beta)
pars.nw=getSample(expR.nw)
pars.ma=getSample(expR.ma)
pars.wd25=getSample(expR.25wd)
pars.wd50=getSample(expR.50wd)
pars.we50=getSample(expR.50we)
set.seed(10)
aComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.a.beta[,1]),
                                                                    shape2 = median(pars.a.beta[,2]))),
                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="actual"])),rep("pred",length(modDat$exp.rate[modDat$treat=="actual"]))))
nwComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="noWinter"],rbeta(n=length(modDat$exp.rate[modDat$treat=="noWinter"]),
                                                                      shape1 = median(pars.nw[,1]),
                                                                      shape2 = median(pars.nw[,2]))),
                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="noWinter"])),rep("pred",length(modDat$exp.rate[modDat$treat=="noWinter"]))))

maComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="mayAug"],rbeta(n=length(modDat$exp.rate[modDat$treat=="mayAug"]),
                                                                    shape1 = median(pars.ma[,1]),
                                                                    shape2 = median(pars.ma[,2]))),
                  treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="mayAug"])),rep("pred",length(modDat$exp.rate[modDat$treat=="mayAug"]))))

wd25Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="wd25"],rbeta(n=length(modDat$exp.rate[modDat$treat=="wd25"]),
                                                                    shape1 = median(pars.wd25[,1]),
                                                                    shape2 = median(pars.wd25[,2]))),
                    treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="wd25"])),rep("pred",length(modDat$exp.rate[modDat$treat=="wd25"]))))

wd50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="wd50"],rbeta(n=length(modDat$exp.rate[modDat$treat=="wd50"]),
                                                                    shape1 = median(pars.wd50[,1]),
                                                                    shape2 = median(pars.wd50[,2]))),
                    treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="wd50"])),rep("pred",length(modDat$exp.rate[modDat$treat=="wd50"]))))

we50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="we50"],rbeta(n=length(modDat$exp.rate[modDat$treat=="we50"]),
                                                                    shape1 = median(pars.we50[,1]),
                                                                    shape2 = median(pars.we50[,2]))),
                    treat=c(rep("observed",length(modDat$exp.rate[modDat$treat=="we50"])),rep("pred",length(modDat$exp.rate[modDat$treat=="we50"]))))

a.p=ggplot(aComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "Actual", fill=element_blank())
nw.p=ggplot(nwComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "No Winter Data (April - October)", fill=element_blank())
ma.p=ggplot(maComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "May - August Data Only", fill=element_blank())
wd25.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "25% of Weekday Creel Visits/Month Removed", fill=element_blank())
wd50.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "50% of Weekday Creel Visits/Month Removed", fill=element_blank())
we50.p=ggplot(we50Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "50% of Weekdend Creel Visits/Month Removed", fill=element_blank())

ggarrange(a.p,nw.p,ma.p,wd25.p,wd50.p,we50.p, common.legend = T)

# comparison to actual
set.seed(10)
aComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                   shape1 = median(pars.a.beta[,1]),
                                                                   shape2 = median(pars.a.beta[,2]))),
                 treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))
nwComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                      shape1 = median(pars.nw[,1]),
                                                                      shape2 = median(pars.nw[,2]))),
                  treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

maComp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.ma[,1]),
                                                                    shape2 = median(pars.ma[,2]))),
                  treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

wd25Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.wd25[,1]),
                                                                    shape2 = median(pars.wd25[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

wd50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.wd50[,1]),
                                                                    shape2 = median(pars.wd50[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))

we50Comp=data.frame(u=c(modDat$exp.rate[modDat$treat=="actual"],rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                                                                    shape1 = median(pars.we50[,1]),
                                                                    shape2 = median(pars.we50[,2]))),
                    treat=c(rep("actual",length(modDat$exp.rate[modDat$treat=="actual"])),rep("model",length(modDat$exp.rate[modDat$treat=="actual"]))))
a.p=ggplot(aComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "Actual", fill=element_blank())
nw.p=ggplot(nwComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "No Winter Data (April - October)", fill=element_blank())
ma.p=ggplot(maComp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "May - August Data Only", fill=element_blank())
wd25.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "25% of Weekday Creel Visits/Month Removed", fill=element_blank())
wd50.p=ggplot(wd25Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "50% of Weekday Creel Visits/Month Removed", fill=element_blank())
we50.p=ggplot(we50Comp)+theme_classic()+
  geom_density(aes(x=u,fill=treat),alpha=0.2)+
  scale_fill_viridis_d()+labs(x="Exploitation Rate", y="Density", title = "50% of Weekdend Creel Visits/Month Removed", fill=element_blank())

ggarrange(a.p,nw.p,ma.p,wd25.p,wd50.p,we50.p, common.legend = T)

### MODEL CHECKNG ####

gelmanDiagnostics(expR.actual.beta, plot = T) # converged
gelmanDiagnostics(expR.nw, plot = T) # converged
gelmanDiagnostics(expR.ma, plot = T) # converged
gelmanDiagnostics(expR.25wd, plot = T) # converged
gelmanDiagnostics(expR.50wd, plot = T) # converged
gelmanDiagnostics(expR.50we, plot = T) # converged

# bayesian p-value for each model

#### Pb ACTUAL ####
pval.actual=data.frame(alpha=rep(NA,nrow(pars.a.beta)),
                       beta=NA,
                       cv=NA,
                       sd=NA)
set.seed(10)
for(i in 1:nrow(pars.a.beta)){
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                 shape1 = pars.a.beta[i,1],
                 shape2 = pars.a.beta[i,2])
  pval.actual$alpha[i]=pars.a.beta[i,1]
  pval.actual$beta[i]=pars.a.beta[i,2]
  pval.actual$cv[i]=sd(tempdat)/mean(tempdat)
  pval.actual$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.actual$cvExceed=0
pval.actual$sdExceed=0

actual.cv=sd(modDat$exp.rate[modDat$treat=="actual"])/mean(modDat$exp.rate[modDat$treat=="actual"])
actual.sd=sd(modDat$exp.rate[modDat$treat=="actual"])

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
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="noWinter"]),
                shape1 = pars.nw[i,1],
                shape2 = pars.nw[i,2])
  pval.noWinter$alpha[i]=pars.nw[i,1]
  pval.noWinter$beta[i]=pars.nw[i,2]
  pval.noWinter$cv[i]=sd(tempdat)/mean(tempdat)
  pval.noWinter$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.noWinter$cvExceed=0
pval.noWinter$sdExceed=0

noWinter.cv=sd(modDat$exp.rate[modDat$treat=="noWinter"])/mean(modDat$exp.rate[modDat$treat=="noWinter"])
noWinter.sd=sd(modDat$exp.rate[modDat$treat=="noWinter"])

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
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="mayAug"]),
                shape1 = pars.ma[i,1],
                shape2 = pars.ma[i,2])
  pval.mayAug$alpha[i]=pars.ma[i,1]
  pval.mayAug$beta[i]=pars.ma[i,2]
  pval.mayAug$cv[i]=sd(tempdat)/mean(tempdat)
  pval.mayAug$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.mayAug$cvExceed=0
pval.mayAug$sdExceed=0

mayAug.cv=sd(modDat$exp.rate[modDat$treat=="mayAug"])/mean(modDat$exp.rate[modDat$treat=="mayAug"])
mayAug.sd=sd(modDat$exp.rate[modDat$treat=="mayAug"])

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
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="wd25"]),
                shape1 = pars.wd25[i,1],
                shape2 = pars.wd25[i,2])
  pval.wd25$alpha[i]=pars.wd25[i,1]
  pval.wd25$beta[i]=pars.wd25[i,2]
  pval.wd25$cv[i]=sd(tempdat)/mean(tempdat)
  pval.wd25$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd25$cvExceed=0
pval.wd25$sdExceed=0

wd25.cv=sd(modDat$exp.rate[modDat$treat=="wd25"])/mean(modDat$exp.rate[modDat$treat=="wd25"])
wd25.sd=sd(modDat$exp.rate[modDat$treat=="wd25"])

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
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="wd50"]),
                shape1 = pars.wd50[i,1],
                shape2 = pars.wd50[i,2])
  pval.wd50$alpha[i]=pars.wd50[i,1]
  pval.wd50$beta[i]=pars.wd50[i,2]
  pval.wd50$cv[i]=sd(tempdat)/mean(tempdat)
  pval.wd50$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd50$cvExceed=0
pval.wd50$sdExceed=0

wd50.cv=sd(modDat$exp.rate[modDat$treat=="wd50"])/mean(modDat$exp.rate[modDat$treat=="wd50"])
wd50.sd=sd(modDat$exp.rate[modDat$treat=="wd50"])

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
  tempdat=rbeta(n=length(modDat$exp.rate[modDat$treat=="we50"]),
                shape1 = pars.we50[i,1],
                shape2 = pars.we50[i,2])
  pval.we50$alpha[i]=pars.we50[i,1]
  pval.we50$beta[i]=pars.we50[i,2]
  pval.we50$cv[i]=sd(tempdat)/mean(tempdat)
  pval.we50$sd[i]=sd(tempdat)
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.we50$cvExceed=0
pval.we50$sdExceed=0

we50.cv=sd(modDat$exp.rate[modDat$treat=="we50"])/mean(modDat$exp.rate[modDat$treat=="we50"])
we50.sd=sd(modDat$exp.rate[modDat$treat=="we50"])

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



# this pval work suggests the beta distribution is not a good choice. I think this work shows that the lognormal is likely more appropriate for modeling this data. Here I'm making comparison to self so a lack of fit means that the model can't fit the data. I am not looking at comparison to actual.