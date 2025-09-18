# script to work through with Olaf on how each of the 5 fishery metics is calculated from the thinned data for weekend and weekday strata
## CJD 9.18.25

rm(list=ls())

library(tidyverse)

# reading the in the creel data from WDNR database 
setwd("C:/Users/dassocju/Documents/OAS_git_repos/Creel_SFR")

call=readRDS("creelDataSet_all.RData") # pulled on 10.25.2023
# unlist, etc. to get back to individual dfs, filtering to completed trips only
cserv=call[[1]] # creel survey info
cvis=call[[2]] # creel visit info
ccou=call[[3]] # creel count info
cint=call[[4]]; cint=cint[cint$trip.completed=='y',] # creel interview info
cfish=call[[5]]; cfish=cfish[cfish$trip.complete=='y',] # creel catch and harvest info
cfish.i=call[[6]]; cfish.i=cfish.i[cfish.i$trip.complete.flag=='y',] # creel individual fish length and mark info


# baseline effort, harvest, and harvest rate calcualtions from the data
# accomplished using the dnr database package. The code to make these calcuations follows the original DNR SAS code used to produce the official metrics DNR uses.
library(wdnr.fmdb)

ceff=calc_creel_effort(creel_count_data=ccou, creel_int_data=cint) 

charv=calc_creel_harvest(creel_count_data = ccou, creel_int_data = cint,creel_fish_data = cfish) 

charvR=calc_creel_harvest_rates(creel_fish_data=cfish)

## CALC EXPLOITATION RATES ####
#lchar=get_fmdb_lakechar() # pulls basic lake characteristics from dnr database
#write.csv(lchar, 'lake_characteristics.csv', row.names = F)
lchar=read.csv('lake_characteristics.csv')
ctwiWBIC=lchar[lchar$trtystat==1 & !is.na(lchar$trtystat),] # pulling list of wbics for just Ceded Territory Lakes
ctwiWBIC.creel=ctwiWBIC$wbic[ctwiWBIC$wbic%in%cserv$wbic] # wbics in CTWI that have been creeled

tagdat=read.csv("tags_and_marks.csv") # marking data from dnr databse also pulled on 10.25.2023
# cleaning up some names
tagdat$County=standardize_county_names(tagdat$County)
tagdat$Gear=standardize_string(tagdat$Gear)
tagdat$Waterbody.Name=standardize_waterbody_names(tagdat$Waterbody.Name)

fndat=tagdat%>% # this is just in case we care what type of mark is there, but I'm going to overwrite this for now and just get total number marked. We'd only care about this if we wanted to know about marks from different years of marking efforts. These are rare in the data as surveys don't usually happen often enough for old marks to still be identifiable by creel clerks.
  # many surveys have a primary and secondary fin clip, the primary is for adults and unknowns >= 15". The secondary clip (usually TC) is for juveniles (unknowns < 15" and >= 7"). 
  filter(WBIC%in%ctwiWBIC.creel & Species.Code=="X22" & Gear=="fyke_net", !is.na(Mark.Given) & Mark.Given!="")%>%
  group_by(WBIC, Waterbody.Name,Survey.Year,Survey.Begin.Date,Survey.End.Date,Survey.Seq.No,Mark.Given)%>%
  summarise(nFish=sum(Number.of.Fish))

# now summing across all types of marks. Creel clerks are recording both primary and secondary clips they find.
fndat=tagdat%>% 
  filter(WBIC%in%ctwiWBIC.creel & Species.Code=="X22" & Gear=="fyke_net", !is.na(Mark.Given) & Mark.Given!="")%>%
  group_by(WBIC, Waterbody.Name,Survey.Year,Survey.Begin.Date,Survey.End.Date,Survey.Seq.No)%>%
  summarise(nFish=sum(Number.of.Fish))%>%
  mutate(Survey.Begin.Date=lubridate::mdy(Survey.Begin.Date),
         Survey.End.Date=lubridate::mdy(Survey.End.Date),
         begin.md=format(Survey.Begin.Date, "%m-%d"),
         end.md=format(Survey.End.Date, "%m-%d"))%>%
  filter(begin.md<"06-01") # getting rid of any marking surveys that may have taken place well after fishing season opened.

fdat=fndat%>% # now that I have only surveys I want, pooling across start and end dates within a year to create an annual total number marked for comparison to creel data
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

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
harvestEstimates=charv%>%
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested
markHarvEst=crRecap%>%
  left_join(harvestEstimates)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

ang.exp=markHarvEst%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked) 
naExps=ang.exp[is.na(ang.exp$exp.rate),] # looking at the lake-years without data to see if there's a problem with my code or just missing data
# looks like it's either cases were no fish were marked in spring fyking for that creel survey or no walleye were harvested or no marked walleye were harvested, either of those will throw an NA in the exploitation rate calculation

# there are 8 observation where the number of marks returned was higher than what was marked, I'm throwing these out of the analysis.
ang.exp=ang.exp[!is.na(ang.exp$exp.rate) & ang.exp$exp.rate<1.0,]

ang.exp=ang.exp%>% # this creates a survey level exploitation rate
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))

# now I have exploitation rates for the creel data as it currently exists.




# Here's the code that I wrote to remove 25% of weekdays and recalc the creel metric, in this case exploitation rate

ifish=cfish.i%>% # making a data frame to add my groupings too so I can leave cfish.i alone
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  mutate(daytype=ifelse(wday(sample.date)%in%c(1,7),"weekend","weekday"),
         month=month(sample.date),
         season=ifelse(month%in%4:10,"openwater","winter"))


###### 25% WEEKDAYS ####

rm.fish.seq.no=NA # fish seq. nos. to remove, basically fish IDs
rm.visit.fish.seq.no=NA # visit IDs to remove
ifish.surv=unique(ifish$survey.seq.no) # list of the inidvidual creel surveys to loop through since I want the reduction done independently for each creel survey.

set.seed(3)
# this for loop randomly selects the weekday creel clerk visits to remove by month
for(i in 1:length(ifish.surv)){ # survey loop
  full=ifish[ifish$survey.seq.no==ifish.surv[i] & ifish$daytype=="weekday",] # inidividual fish data from the weekday creel visits
  ms=unique(full$month) # months to loop through
  if(nrow(full)>0){# if there's fish data that month to reduce, not always the case
    for(m in 1:length(ms)){ # month loop
      visits=unique(full$visit.fish.seq.no[full$month==ms[m]]) # vector of all the unqique weekday visits by a creel clerk for that month and lake (i.e. survey.seq.no.)
      visit.rm=visits[c(round(runif(round(0.25*length(visits)),min=1,max = length(visits))))] # randomly selecting weekday visits for removal
      t.rm=full$fish.data.seq.no[full$visit.fish.seq.no%in%visit.rm]# fish seq nos to remove that associated with the vist seq nos selected for removal
      
      rm.visit.fish.seq.no=c(rm.visit.fish.seq.no,visit.rm) # visit fish seq nos to use to reduce the interview, count, and fish data needed to get harvest estimates
      rm.fish.seq.no=c(rm.fish.seq.no,t.rm) # vector of fish to remove from individual fish data
      
    }
  }
}

# these next few lines do the removal after the loops above selects the correct sequence numbers for removal
ifish.25wd=ifish[!(ifish$fish.data.seq.no%in%rm.fish.seq.no),] # reduced individual fish data
iint.25wd=cint%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced interview data
icou.25wd=ccou%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced count data
ifishAg.25wd=cfish%>%
  filter(wbic%in%ctwiWBIC.creel & !(visit.fish.seq.no%in%rm.visit.fish.seq.no)) # reduced aggregate fish data

# from  here down I'm using the same code as the I do for the full creel data set but with the reduced data set I just made as the new input.
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

# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is observations where # marks in creel exceeds # marks at large)
ang.exp.25wd=ang.exp.25wd[!is.na(ang.exp.25wd$exp.rate) & ang.exp.25wd$exp.rate<1.0,]

# creating survey-level exploitation rates
ang.exp.25wd=ang.exp.25wd%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))


# I reuse the same code but swap weekdays for weekends below

###### 50% WEEKENDS ####

rm.fish.seq.no=NA
rm.visit.fish.seq.no=NA
ifish.surv=unique(ifish$survey.seq.no)

set.seed(3)
for(i in 1:length(ifish.surv)){ # survey loop
  full=ifish[ifish$survey.seq.no==ifish.surv[i] & ifish$daytype=="weekend",]
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

# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is observations where # marks in creel exceeds # marks at large)
ang.exp.50we=ang.exp.50we[!is.na(ang.exp.50we$exp.rate) & ang.exp.50we$exp.rate<1.0,]

# creating survey-level exploitation rates
ang.exp.50we=ang.exp.50we%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))
