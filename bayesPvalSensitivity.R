# exploring the sensitivity of my conclusions to the pval criteria I pick.
# CJD 2.2.2024

rm(list=ls())
library(wdnr.fmdb)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(BayesianTools)
library(moments)
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

# making a single data set to work with
ttrExp=rbind(cbind(ang.exp,treat=rep("actual",nrow(ang.exp))),
             cbind(ang.exp.nw, treat=rep("noWinter",nrow(ang.exp.nw))),
             cbind(ang.exp.ma, treat=rep("mayAug",nrow(ang.exp.ma))),
             cbind(ang.exp.25wd, treat=rep("wd25",nrow(ang.exp.25wd))),
             cbind(ang.exp.50wd, treat=rep("wd50",nrow(ang.exp.50wd))),
             cbind(ang.exp.50we, treat=rep("we50",nrow(ang.exp.50we))))

colnames(ttrExp)=c("survey.seq.no","exp.rate","exp.rate.sd","treat")

modDat=ttrExp[ttrExp$exp.rate!=0,] # version of the above without 0s, models don't fit with 0s in there

#### ACTUAL ####
uLL.a=function(param){
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
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.a)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),
                 meanlog = pars.a[i,1],
                 sdlog = pars.a[i,2])
  pval.actual$alpha[i]=pars.a[i,1]
  pval.actual$beta[i]=pars.a[i,2]
  pval.actual$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.actual$sd[i]=sd(log(tempdat))
  pval.actual$med[i]=median(log(tempdat))
  pval.actual$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="actual"], rescale.p = T)
  pval.actual$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="actual"]))
  pval.actual$f[i]=f.test$statistic
  
}

# now calculate the number of times the test statistic exceeds that of the real data

pval.actual$cvExceed=0
pval.actual$sdExceed=0
pval.actual$medExceed=0
pval.actual$kurtExceed=0
pval.actual$x2SigDiff=0
pval.actual$fStatExceed=0


actual.cv=sd(log(modDat$exp.rate[modDat$treat=="actual"]))/mean(log(modDat$exp.rate[modDat$treat=="actual"]))
actual.sd=sd(log(modDat$exp.rate[modDat$treat=="actual"]))
actual.med=median(log(modDat$exp.rate[modDat$treat=="actual"]))
actual.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="actual"]))
actual.x2=chisq.test(modDat$exp.rate[modDat$treat=="actual"],p=modDat$exp.rate[modDat$treat=="actual"],rescale.p = T)
actual.f=var.test(log(modDat$exp.rate[modDat$treat=="actual"]),log(modDat$exp.rate[modDat$treat=="actual"]))

pval.actual$cvExceed[pval.actual$cv>actual.cv]=1
pval.actual$sdExceed[pval.actual$sd>actual.sd]=1
pval.actual$medExceed[pval.actual$med>actual.med]=1
pval.actual$kurtExceed[pval.actual$kurt>actual.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.actual$x2SigDiff[pval.actual$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the actual data
pval.actual$fStatExceed[pval.actual$f>actual.f$statistic]=1 # how often the f statistic is bigger than the actual

# # this is just random stuff to work with the x2 test to make sure I did it right
# hist(tempdat)
# hist(plnorm(tempdat, meanlog = mean(log(tempdat)), sdlog = sd(log(tempdat))))
# df=data.frame(tempdat=tempdat,
#               pdist=plnorm(tempdat,
#                            meanlog = mean(log(modDat$exp.rate[modDat$treat=="actual"])),
#                            sdlog = sd(log(modDat$exp.rate[modDat$treat=="actual"]))),
#               actual=modDat$exp.rate[modDat$treat=="actual"],
#               actualP=plnorm(modDat$exp.rate[modDat$treat=="actual"],
#                                    meanlog = mean(log(modDat$exp.rate[modDat$treat=="actual"])),
#                                    sdlog = sd(log(modDat$exp.rate[modDat$treat=="actual"]))))
# simD=rlnorm(n=length(modDat$exp.rate[modDat$treat=="actual"]),meanlog = median(pars.a[,1]), sdlog = median(pars.a[,2]))
# realD=modDat$exp.rate[modDat$treat=="actual"]
# probs=plnorm(realD,meanlog = mean(log(realD)), sdlog = sd(log(realD)))
# x2=chisq.test(simD, p=probs,rescale.p = T,simulate.p.value = T)

#### Pb NoWinter ####
pval.noWinter=data.frame(alpha=rep(NA,nrow(pars.nw)),
                       beta=NA,
                       cv=NA,
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.nw)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="noWinter"]),
                 meanlog = pars.nw[i,1],
                 sdlog = pars.nw[i,2])
  pval.noWinter$alpha[i]=pars.nw[i,1]
  pval.noWinter$beta[i]=pars.nw[i,2]
  pval.noWinter$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.noWinter$sd[i]=sd(log(tempdat))
  pval.noWinter$med[i]=median(log(tempdat))
  pval.noWinter$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="noWinter"], rescale.p = T)
  pval.noWinter$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="noWinter"]))
  pval.noWinter$f[i]=f.test$statistic
  
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.noWinter$cvExceed=0
pval.noWinter$sdExceed=0
pval.noWinter$medExceed=0
pval.noWinter$kurtExceed=0
pval.noWinter$x2SigDiff=0
pval.noWinter$fStatExceed=0


noWinter.cv=sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))/mean(log(modDat$exp.rate[modDat$treat=="noWinter"]))
noWinter.sd=sd(log(modDat$exp.rate[modDat$treat=="noWinter"]))
noWinter.med=median(log(modDat$exp.rate[modDat$treat=="noWinter"]))
noWinter.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="noWinter"]))
noWinter.x2=chisq.test(modDat$exp.rate[modDat$treat=="noWinter"],p=modDat$exp.rate[modDat$treat=="noWinter"],rescale.p = T)
noWinter.f=var.test(log(modDat$exp.rate[modDat$treat=="noWinter"]),log(modDat$exp.rate[modDat$treat=="noWinter"]))

pval.noWinter$cvExceed[pval.noWinter$cv>noWinter.cv]=1
pval.noWinter$sdExceed[pval.noWinter$sd>noWinter.sd]=1
pval.noWinter$medExceed[pval.noWinter$med>noWinter.med]=1
pval.noWinter$kurtExceed[pval.noWinter$kurt>noWinter.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.noWinter$x2SigDiff[pval.noWinter$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the noWinter data
pval.noWinter$fStatExceed[pval.noWinter$f>noWinter.f$statistic]=1 # how often the f statistic is bigger than the noWinter

#### Pb MayAug ####
pval.mayAug=data.frame(alpha=rep(NA,nrow(pars.ma)),
                       beta=NA,
                       cv=NA,
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.ma)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="mayAug"]),
                 meanlog = pars.ma[i,1],
                 sdlog = pars.ma[i,2])
  pval.mayAug$alpha[i]=pars.ma[i,1]
  pval.mayAug$beta[i]=pars.ma[i,2]
  pval.mayAug$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.mayAug$sd[i]=sd(log(tempdat))
  pval.mayAug$med[i]=median(log(tempdat))
  pval.mayAug$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="mayAug"], rescale.p = T)
  pval.mayAug$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="mayAug"]))
  pval.mayAug$f[i]=f.test$statistic
  
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.mayAug$cvExceed=0
pval.mayAug$sdExceed=0
pval.mayAug$medExceed=0
pval.mayAug$kurtExceed=0
pval.mayAug$x2SigDiff=0
pval.mayAug$fStatExceed=0


mayAug.cv=sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))/mean(log(modDat$exp.rate[modDat$treat=="mayAug"]))
mayAug.sd=sd(log(modDat$exp.rate[modDat$treat=="mayAug"]))
mayAug.med=median(log(modDat$exp.rate[modDat$treat=="mayAug"]))
mayAug.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="mayAug"]))
mayAug.x2=chisq.test(modDat$exp.rate[modDat$treat=="mayAug"],p=modDat$exp.rate[modDat$treat=="mayAug"],rescale.p = T)
mayAug.f=var.test(log(modDat$exp.rate[modDat$treat=="mayAug"]),log(modDat$exp.rate[modDat$treat=="mayAug"]))

pval.mayAug$cvExceed[pval.mayAug$cv>mayAug.cv]=1
pval.mayAug$sdExceed[pval.mayAug$sd>mayAug.sd]=1
pval.mayAug$medExceed[pval.mayAug$med>mayAug.med]=1
pval.mayAug$kurtExceed[pval.mayAug$kurt>mayAug.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.mayAug$x2SigDiff[pval.mayAug$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the mayAug data
pval.mayAug$fStatExceed[pval.mayAug$f>mayAug.f$statistic]=1 # how often the f statistic is bigger than the mayAug

#### Pb WD25 ####
pval.wd25=data.frame(alpha=rep(NA,nrow(pars.wd25)),
                       beta=NA,
                       cv=NA,
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.wd25)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd25"]),
                 meanlog = pars.wd25[i,1],
                 sdlog = pars.wd25[i,2])
  pval.wd25$alpha[i]=pars.wd25[i,1]
  pval.wd25$beta[i]=pars.wd25[i,2]
  pval.wd25$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.wd25$sd[i]=sd(log(tempdat))
  pval.wd25$med[i]=median(log(tempdat))
  pval.wd25$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="wd25"], rescale.p = T)
  pval.wd25$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="wd25"]))
  pval.wd25$f[i]=f.test$statistic
  
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd25$cvExceed=0
pval.wd25$sdExceed=0
pval.wd25$medExceed=0
pval.wd25$kurtExceed=0
pval.wd25$x2SigDiff=0
pval.wd25$fStatExceed=0


wd25.cv=sd(log(modDat$exp.rate[modDat$treat=="wd25"]))/mean(log(modDat$exp.rate[modDat$treat=="wd25"]))
wd25.sd=sd(log(modDat$exp.rate[modDat$treat=="wd25"]))
wd25.med=median(log(modDat$exp.rate[modDat$treat=="wd25"]))
wd25.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="wd25"]))
wd25.x2=chisq.test(modDat$exp.rate[modDat$treat=="wd25"],p=modDat$exp.rate[modDat$treat=="wd25"],rescale.p = T)
wd25.f=var.test(log(modDat$exp.rate[modDat$treat=="wd25"]),log(modDat$exp.rate[modDat$treat=="wd25"]))

pval.wd25$cvExceed[pval.wd25$cv>wd25.cv]=1
pval.wd25$sdExceed[pval.wd25$sd>wd25.sd]=1
pval.wd25$medExceed[pval.wd25$med>wd25.med]=1
pval.wd25$kurtExceed[pval.wd25$kurt>wd25.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.wd25$x2SigDiff[pval.wd25$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the wd25 data
pval.wd25$fStatExceed[pval.wd25$f>wd25.f$statistic]=1 # how often the f statistic is bigger than the wd25

#### Pb WD50 ####
pval.wd50=data.frame(alpha=rep(NA,nrow(pars.wd50)),
                       beta=NA,
                       cv=NA,
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.wd50)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="wd50"]),
                 meanlog = pars.wd50[i,1],
                 sdlog = pars.wd50[i,2])
  pval.wd50$alpha[i]=pars.wd50[i,1]
  pval.wd50$beta[i]=pars.wd50[i,2]
  pval.wd50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.wd50$sd[i]=sd(log(tempdat))
  pval.wd50$med[i]=median(log(tempdat))
  pval.wd50$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="wd50"], rescale.p = T)
  pval.wd50$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="wd50"]))
  pval.wd50$f[i]=f.test$statistic
  
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.wd50$cvExceed=0
pval.wd50$sdExceed=0
pval.wd50$medExceed=0
pval.wd50$kurtExceed=0
pval.wd50$x2SigDiff=0
pval.wd50$fStatExceed=0


wd50.cv=sd(log(modDat$exp.rate[modDat$treat=="wd50"]))/mean(log(modDat$exp.rate[modDat$treat=="wd50"]))
wd50.sd=sd(log(modDat$exp.rate[modDat$treat=="wd50"]))
wd50.med=median(log(modDat$exp.rate[modDat$treat=="wd50"]))
wd50.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="wd50"]))
wd50.x2=chisq.test(modDat$exp.rate[modDat$treat=="wd50"],p=modDat$exp.rate[modDat$treat=="wd50"],rescale.p = T)
wd50.f=var.test(log(modDat$exp.rate[modDat$treat=="wd50"]),log(modDat$exp.rate[modDat$treat=="wd50"]))

pval.wd50$cvExceed[pval.wd50$cv>wd50.cv]=1
pval.wd50$sdExceed[pval.wd50$sd>wd50.sd]=1
pval.wd50$medExceed[pval.wd50$med>wd50.med]=1
pval.wd50$kurtExceed[pval.wd50$kurt>wd50.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.wd50$x2SigDiff[pval.wd50$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the wd50 data
pval.wd50$fStatExceed[pval.wd50$f>wd50.f$statistic]=1 # how often the f statistic is bigger than the wd50

#### Pb WE50 ####
pval.we50=data.frame(alpha=rep(NA,nrow(pars.we50)),
                       beta=NA,
                       cv=NA,
                       sd=NA,
                       med=NA,
                       kurt=NA,
                       x2=NA,
                       f=NA)
set.seed(10)
for(i in 1:nrow(pars.we50)){
  tempdat=rlnorm(n=length(modDat$exp.rate[modDat$treat=="we50"]),
                 meanlog = pars.we50[i,1],
                 sdlog = pars.we50[i,2])
  pval.we50$alpha[i]=pars.we50[i,1]
  pval.we50$beta[i]=pars.we50[i,2]
  pval.we50$cv[i]=sd(log(tempdat))/mean(log(tempdat))
  pval.we50$sd[i]=sd(log(tempdat))
  pval.we50$med[i]=median(log(tempdat))
  pval.we50$kurt[i]=kurtosis(log(tempdat))
  ct=chisq.test(tempdat, p=modDat$exp.rate[modDat$treat=="we50"], rescale.p = T)
  pval.we50$x2[i]=ct$p.value
  f.test=var.test(log(tempdat),log(modDat$exp.rate[modDat$treat=="we50"]))
  pval.we50$f[i]=f.test$statistic
  
}

# now calculate the number of times the cv or sd exceeds that of the real data

pval.we50$cvExceed=0
pval.we50$sdExceed=0
pval.we50$medExceed=0
pval.we50$kurtExceed=0
pval.we50$x2SigDiff=0
pval.we50$fStatExceed=0


we50.cv=sd(log(modDat$exp.rate[modDat$treat=="we50"]))/mean(log(modDat$exp.rate[modDat$treat=="we50"]))
we50.sd=sd(log(modDat$exp.rate[modDat$treat=="we50"]))
we50.med=median(log(modDat$exp.rate[modDat$treat=="we50"]))
we50.kurt=kurtosis(log(modDat$exp.rate[modDat$treat=="we50"]))
we50.x2=chisq.test(modDat$exp.rate[modDat$treat=="we50"],p=modDat$exp.rate[modDat$treat=="we50"],rescale.p = T)
we50.f=var.test(log(modDat$exp.rate[modDat$treat=="we50"]),log(modDat$exp.rate[modDat$treat=="we50"]))

pval.we50$cvExceed[pval.we50$cv>we50.cv]=1
pval.we50$sdExceed[pval.we50$sd>we50.sd]=1
pval.we50$medExceed[pval.we50$med>we50.med]=1
pval.we50$kurtExceed[pval.we50$kurt>we50.kurt]=1 # kurt value great that observed data means there are more outliers, or thicker tails than the observed data
pval.we50$x2SigDiff[pval.we50$x2<0.05]=1 # looking at how often we would say the simmed data did not come from the probabilit distribution of the we50 data
pval.we50$fStatExceed[pval.we50$f>we50.f$statistic]=1 # how often the f statistic is bigger than the we50


### summarizing pvalue output
# reminder this is all comparison to self

# making a list object with all the pval objects
self.pvals=list(pval.actual, pval.noWinter, pval.mayAug, pval.wd25, pval.wd50, pval.we50)

all.self.pvals=data.frame(dataSet=c(rep("actual",6),rep("noWinter",6),rep("mayAug",6),rep("wd25",6),rep("wd50",6),rep("we50",6)),
                          measure=rep(c("cv","sd","med","kurt","x2Sig","f"),6),
                          pval=NA)

for(i in 1:length(self.pvals)){
  tp=self.pvals[[i]]
  datSet=unique(all.self.pvals$dataSet)[i]
  
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="cv"]=sum(tp$cvExceed==1)/nrow(tp)
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="sd"]=sum(tp$sdExceed==1)/nrow(tp)
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="med"]=sum(tp$medExceed==1)/nrow(tp)
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="kurt"]=sum(tp$kurtExceed==1)/nrow(tp)
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="x2Sig"]=sum(tp$x2SigDiff==1)/nrow(tp)
  all.self.pvals$pval[all.self.pvals$dataSet==datSet & all.self.pvals$measure=="f"]=sum(tp$fStatExceed==1)/nrow(tp)
}

ggplot(all.self.pvals)+theme_classic()+
  geom_point(aes(x=measure, y=pval), size=2)+facet_wrap(~dataSet)+
  geom_hline(yintercept = c(0.1,0.9), linetype=2)+
  geom_hline(yintercept = 0.5, linetype=4)

# kurtosis result suggests the model often produced a distribution of exploitation rates that has slimmer tails (in other words fewer outliers) than the observed data. I do feel confident about my calculation of this.

# x2 result points towards the x2 statistic for the simmed data being larger than the observed data. This isn't a shock since the test statistic being calculated for the observed data is comparing the exact data to itself. I think that's biasing the pvalue for this measure high. This is assuming I'm doing the x2 part of this right and that's not a gurantee, since I've gone back and forth on whether I should be comparing the test statistic or the p-value from the test. I think the test statistic is right since I don't think it's smart to use a frequentist p-value inside a bayesian p value test and I'm not sure it makes theoretical sense for that pvalue to be above or below the observed data p value 50% of the time.

# the f test (Fisher's Test) is perhaps the most appropriate of the three (x2, kurtosis, f) and I do feel confident about my calculation of this.

# it also looks like aside from the kurt and x2 tests, the cutoff value chosen won't really matter unless it gets very narrow like 0.45-0.55.
