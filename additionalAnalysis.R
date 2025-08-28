# making some quick plots to explore the suggestions from olaf
## CJD 7.22.25

# this script is set up to work off the output of the rmd files that hold the main analysis

# P v O DFs ####
widetrL=pivot_wider(ttrExp, names_from = treat, values_from = c(exp.rate,exp.rate.sd))
widetrL=pivot_wider(ttrExp, names_from = treat, values_from = total.catch)
widetrL=pivot_wider(ttrExp, names_from = treat, values_from = total.eff)
widetrL=pivot_wider(ttrExp, names_from = treat, values_from = total.harv)
widetrL=pivot_wider(ttrExp, names_from = treat, values_from = c(meanHarvR,sdHarvR))

# there are several rows in the resulting data frame that have NA for both the seasonal reductions and not the percentage reductions. I have thoroughly (but not exhaustively) search these surveys and in every instance it was the case that the seasonal filter of the creel data removed all the examined walleye from the survey (i.e. the only walleye examined for marks in the survey were during the months cut from each scenario).
# where there are NAs for all 5 scenarios, it was the case that so few walleye were examined in the survey that all of the reduced data scenarios led to those fish being removed and thus the NA for exploitation rate.
# NAs also occur in some cases where a small number of fish were examined by no marks were found which means the exploitation rate calculation is NA marks harvest/n marks in water. These are NA and not 0 because the n marks harvested is actually an estimate based on the proportion of marks in the number of fish harvested so we don't really know if 0 marks were harvested and we don't have any data estimate the number of marks harvested because none were observed.

# if there's a systematic way to distinguish between these scenarios I can make some of them 0 where it's the case that the observations of fish were filtered out by the sampling design. Where fish were still examined but no marks found, those should stay NA and not be made 0s. 
# ultimately this doesn't impact the bayesian modeling results because 0's aren't included, but for those other summaries olaf mentioned, including that would be meaningful.

## Plotting ####
#### exploitation rate plots ####

#nw
plot(widetrL$exp.rate_actual,widetrL$exp.rate_noWinter,pch=16)
lines(0:1,0:1)
plot(widetrL$exp.rate_actual,widetrL$exp.rate_noWinter,pch=16, xlim = c(0,0.05), ylim = c(0,0.05))
lines(0:1,0:1)

#mayAug
plot(widetrL$exp.rate_actual,widetrL$exp.rate_mayAug,pch=16)
lines(0:1,0:1)
plot(widetrL$exp.rate_actual,widetrL$exp.rate_mayAug,pch=16, xlim = c(0,0.05), ylim = c(0,0.05))
lines(0:1,0:1)

#wd25
plot(widetrL$exp.rate_actual,widetrL$exp.rate_wd25,pch=16)
lines(0:1,0:1)
plot(widetrL$exp.rate_actual,widetrL$exp.rate_wd25,pch=16, xlim = c(0,0.05), ylim = c(0,0.05))
lines(0:1,0:1)

#wd50
plot(widetrL$exp.rate_actual,widetrL$exp.rate_wd50,pch=16)
lines(0:1,0:1)
plot(widetrL$exp.rate_actual,widetrL$exp.rate_wd50,pch=16, xlim = c(0,0.05), ylim = c(0,0.05))
lines(0:1,0:1)

#we50
plot(widetrL$exp.rate_actual,widetrL$exp.rate_we50,pch=16)
lines(0:1,0:1)
plot(widetrL$exp.rate_actual,widetrL$exp.rate_we50,pch=16, xlim = c(0,0.05), ylim = c(0,0.05))
lines(0:1,0:1)

#### total catch plots ####

#nw
plot(widetrL$actual,widetrL$noWinter,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$noWinter,pch=16, xlim = c(0,15), ylim = c(0,15))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#mayAug
plot(widetrL$actual,widetrL$mayAug,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$mayAug,pch=16, xlim = c(0,15), ylim = c(0,15))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd25
plot(widetrL$actual,widetrL$wd25,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd25,pch=16, xlim = c(0,15), ylim = c(0,15))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd50
plot(widetrL$actual,widetrL$wd50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd50,pch=16, xlim = c(0,15), ylim = c(0,15))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#we50
plot(widetrL$actual,widetrL$we50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$we50,pch=16, xlim = c(0,15), ylim = c(0,15))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#### harvest plots ####

#nw
plot(widetrL$actual,widetrL$noWinter,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$noWinter,pch=16, xlim = c(0,3), ylim = c(0,3))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#mayAug
plot(widetrL$actual,widetrL$mayAug,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$mayAug,pch=16, xlim = c(0,3), ylim = c(0,3))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd25
plot(widetrL$actual,widetrL$wd25,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd25,pch=16, xlim = c(0,3), ylim = c(0,3))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd50
plot(widetrL$actual,widetrL$wd50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd50,pch=16, xlim = c(0,3), ylim = c(0,3))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#we50
plot(widetrL$actual,widetrL$we50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$we50,pch=16, xlim = c(0,3), ylim = c(0,3))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#### harvest rate plots ####

#nw
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_noWinter,pch=16)
lines(0:1,0:1)
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_noWinter,pch=16, xlim = c(0,0.3), ylim = c(0,0.3))
lines(0:1,0:1)

#mayAug
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_mayAug,pch=16)
lines(0:1,0:1)
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_mayAug,pch=16, xlim = c(0,0.3), ylim = c(0,0.3))
lines(0:1,0:1)

#wd25
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_wd25,pch=16)
lines(0:1,0:1)
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_wd25,pch=16, xlim = c(0,0.3), ylim = c(0,0.3))
lines(0:1,0:1)

#wd50
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_wd50,pch=16)
lines(0:1,0:1)
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_wd50,pch=16, xlim = c(0,0.3), ylim = c(0,0.3))
lines(0:1,0:1)

#we50
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_we50,pch=16)
lines(0:1,0:1)
plot(widetrL$meanHarvR_actual,widetrL$meanHarvR_we50,pch=16, xlim = c(0,0.3), ylim = c(0,0.3))
lines(0:1,0:1)


#### effort plot ####

#nw
plot(widetrL$actual,widetrL$noWinter,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$noWinter,pch=16, xlim = c(0,200), ylim = c(0,200))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#mayAug
plot(widetrL$actual,widetrL$mayAug,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$mayAug,pch=16, xlim = c(0,200), ylim = c(0,200))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd25
plot(widetrL$actual,widetrL$wd25,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd25,pch=16, xlim = c(0,200), ylim = c(0,200))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#wd50
plot(widetrL$actual,widetrL$wd50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$wd50,pch=16, xlim = c(0,200), ylim = c(0,200))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

#we50
plot(widetrL$actual,widetrL$we50,pch=16)
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))
plot(widetrL$actual,widetrL$we50,pch=16, xlim = c(0,200), ylim = c(0,200))
lines(0:max(widetrL$actual,na.rm = T),0:max(widetrL$actual,na.rm = T))

## RMSE calcs ####

#### exploitation rate rmse ####
# NAs, where the actual was non-zero should be changed to 0 for the treatment, I want this comparison of non-zero to zero to be included in the RMSE calc. NAs where the actual was 0 would still be 0, so they can be changed too, not sure if that's giving me a bonus on the RMSE I shouldn't get. 
widetrL.0=widetrL # copy to change NAs to 0s to
survRecaps=crRecap%>% # creating new version of crRecaps that is summarized to the survey level to deal with multiple entries for a servey.seq.no because of weekend/weekday/month strata.
  group_by(survey.seq.no)%>%
  summarise(totalFishExamined=sum(totalFishExamined),
            n.marks.recapped=sum(n.marks.recapped))

# logic loop to categorize the 0,NA situations described at the top of this script
widetrL.0$note=NA

for(i in 1:nrow(widetrL.0)){
  if(is.na(widetrL.0$exp.rate_noWinter[i])){
    if(is.na(widetrL.0$exp.rate_noWinter[i]) & widetrL.0$exp.rate_actual[i]!=0){ # actual data had harvest and harvest of marks, but reduction eliminates those observations
      widetrL.0$note[i]='harvest recorded, data reduction eliminates these observations'

    }else{
      if(survRecaps$totalFishExamined[survRecaps$survey.seq.no==widetrL.0$survey.seq.no[i]]!=0 & widetrL.0$exp.rate_actual[i]==0){ # fish caught but none were marks
        widetrL.0$note[i]='harvest recorded, but no marks found, not true 0'
      }else{
        if(survRecaps$totalFishExamined[survRecaps$survey.seq.no==widetrL.0$survey.seq.no[i]]==0 & widetrL.0$exp.rate_actual[i]==0){ # no harvest came through creel
          widetrL.0$note[i]='no harvest recorded, true 0'
        }
      }
    }
  }
}

# now setting the appropriates ones to 0

widetrL.0$exp.rate_noWinter[is.na(widetrL.0$exp.rate_noWinter) & 
                              widetrL.0$note%in%c('harvest recorded, data reduction eliminates these observations',
                                                  'no harvest recorded, true 0')]=0
widetrL.0$exp.rate_mayAug[is.na(widetrL.0$exp.rate_mayAug) & 
                            widetrL.0$note%in%c('harvest recorded, data reduction eliminates these observations',
                                                'no harvest recorded, true 0')]=0
widetrL.0$exp.rate_wd25[is.na(widetrL.0$exp.rate_wd25) & 
                          widetrL.0$note%in%c('harvest recorded, data reduction eliminates these observations',
                                              'no harvest recorded, true 0')]=0
widetrL.0$exp.rate_wd50[is.na(widetrL.0$exp.rate_wd50) & 
                          widetrL.0$note%in%c('harvest recorded, data reduction eliminates these observations',
                                              'no harvest recorded, true 0')]=0
widetrL.0$exp.rate_we50[is.na(widetrL.0$exp.rate_we50) & 
                          widetrL.0$note%in%c('harvest recorded, data reduction eliminates these observations',
                                              'no harvest recorded, true 0')]=0


# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$exp.rate_noWinter-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_noWinter))
ma.rmse=sqrt(mean((widetrL.0$exp.rate_mayAug-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_mayAug))
wd25.rmse=sqrt(mean((widetrL.0$exp.rate_wd25-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_wd25))
wd50.rmse=sqrt(mean((widetrL.0$exp.rate_wd50-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_wd50))
we50.rmse=sqrt(mean((widetrL.0$exp.rate_we50-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_we50))
rmse=sqrt(mean((widetrL.0$exp.rate_actual-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_actual))

nw.rmse
ma.rmse
wd25.rmse
wd50.rmse
we50.rmse
rmse

### total catch rmse ####

# total catch where the actual ==0 can be set to 0 for the reduced scenarios. Total catch where the actual is >0 can be set to 0 still for the reduced scenarios. There's nothing to worry about as far as the catch calculation goes that would result in an NA meaning some walleye non-zero number of walleye were caught (at least as far as the numbers are able to say).

widetrL.0=widetrL # copy to change NAs to 0s to
sum(is.na(widetrL$we50))
sum(is.na(widetrL$wd50))
sum(is.na(widetrL$wd25))
sum(is.na(widetrL$mayAug))
sum(is.na(widetrL$noWinter))

widetrL.0$noWinter[is.na(widetrL.0$noWinter)]=0
widetrL.0$mayAug[is.na(widetrL.0$mayAug)]=0


# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$noWinter))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$mayAug))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd25))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd50))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$we50))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$actual))

nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse
ma.rmse
wd25.rmse
wd50.rmse
we50.rmse
rmse

#### harvest rmse ####

widetrL.0=widetrL # copy to change NAs to 0s to

sum(is.na(widetrL$we50))
sum(is.na(widetrL$wd50))
sum(is.na(widetrL$wd25))
sum(is.na(widetrL$mayAug))
sum(is.na(widetrL$noWinter))
sum(is.na(widetrL.0$actual))
sum(widetrL.0$actual==0)

sum(!is.na(widetrL.0$noWinter) & is.na(widetrL.0$mayAug)) # every Na in noWinter is also an NA in mayAug + 7 may aug NAs that are not NA for noWinter, makes some sense, more data is cut out, should create more NAs

make0=which(widetrL.0$actual==0 & is.na(widetrL.0$noWinter))
widetrL.0$noWinter[make0]=0 #0 in the original data, still 0
widetrL.0$mayAug[make0]=0
new0=which(widetrL.0$actual!=0 & is.na(widetrL.0$noWinter))
widetrL.0$noWinter[new0]=0 # non 0 in original data, 0 with seasonal reduction
widetrL.0$mayAug[new0]=0
widetrL.0$actual[is.na(widetrL.0$mayAug)] # just checking on the last 7 mayAugs that are NA
widetrL.0$mayAug[is.na(widetrL.0$mayAug)]=0

# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$noWinter))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$mayAug))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd25))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd50))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$we50))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$actual))

nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse
ma.rmse
wd25.rmse
wd50.rmse
we50.rmse
rmse

#### harvest rate rmse ####

widetrL.0=widetrL # copy to change NAs to 0s to

sum(is.na(widetrL.0$meanHarvR_noWinter))
sum(is.na(widetrL.0$meanHarvR_mayAug))
sum(is.na(widetrL.0$meanHarvR_wd25))
sum(is.na(widetrL.0$meanHarvR_wd50))
sum(is.na(widetrL.0$meanHarvR_we50))
sum(is.na(widetrL.0$meanHarvR_actual))

make0=which(widetrL.0$meanHarvR_actual==0 & is.na(widetrL.0$meanHarvR_noWinter)) # 0 in actual data, can be make 0 now
new0=which(widetrL.0$meanHarvR_actual!=0 & is.na(widetrL.0$meanHarvR_noWinter)) # non 0 in acutal data but reduction makes it 0

widetrL.0$meanHarvR_noWinter[is.na(widetrL.0$meanHarvR_noWinter)]=0 # all no Winter NAs are covered by the above situations
widetrL.0$meanHarvR_mayAug[is.na(widetrL.0$meanHarvR_mayAug)]=0 # all mayAUg NAs are covered the same as nowinter NA, plus 7 extras due to further data reductions

# throwing out one survey they has NAN for the acutal and all the subsets, due to walleye catch but no harvest and no time spent fishing for walleye.
widetrL.0=widetrL.0[widetrL.0$survey.seq.no!=515094142,]


# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$meanHarvR_noWinter-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_noWinter))
ma.rmse=sqrt(mean((widetrL.0$meanHarvR_mayAug-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_mayAug))
wd25.rmse=sqrt(mean((widetrL.0$meanHarvR_wd25-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_wd25))
wd50.rmse=sqrt(mean((widetrL.0$meanHarvR_wd50-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_wd50))
we50.rmse=sqrt(mean((widetrL.0$meanHarvR_we50-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_we50))
rmse=sqrt(mean((widetrL.0$meanHarvR_actual-widetrL.0$meanHarvR_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$meanHarvR_actual))

nw.rmse=sqrt(mean((widetrL.0$meanHarvR_noWinter-widetrL.0$meanHarvR_actual)^2,na.rm = T))
ma.rmse=sqrt(mean((widetrL.0$meanHarvR_mayAug-widetrL.0$meanHarvR_actual)^2,na.rm = T))
wd25.rmse=sqrt(mean((widetrL.0$meanHarvR_wd25-widetrL.0$meanHarvR_actual)^2,na.rm = T))
wd50.rmse=sqrt(mean((widetrL.0$meanHarvR_wd50-widetrL.0$meanHarvR_actual)^2,na.rm = T))
we50.rmse=sqrt(mean((widetrL.0$meanHarvR_we50-widetrL.0$meanHarvR_actual)^2,na.rm = T))
rmse=sqrt(mean((widetrL.0$meanHarvR_actual-widetrL.0$meanHarvR_actual)^2,na.rm = T))

nw.rmse
ma.rmse
wd25.rmse
wd50.rmse
we50.rmse
rmse

#### effort rmse ####

# there are no observations of 0 effort in the actual data, it's all >0. So all the reduced surveys that are now NA can be changed to 0 because the seasonal removal of the data eliminated the only effort observations for that survey. There are also no 0 eff obs for the % reductions further solidifying the point.

widetrL.0=widetrL # copy to change NAs to 0s to

# now setting the appropriates ones to 0

widetrL.0$noWinter[is.na(widetrL.0$noWinter)]=0
widetrL.0$mayAug[is.na(widetrL.0$mayAug)]=0

# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$noWinter))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$mayAug))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd25))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd50))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$we50))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$actual))

nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse
ma.rmse
wd25.rmse
wd50.rmse
we50.rmse
rmse


#### other stuff ####
## exploring my season reduction calcs a little more to see if I've made any mistakes
# here I'm calculating exploitation rates by filtering the count, interview, and fish data down before calculating harvest. previously I did the filtering for the seasonal reductions after the harvest calculation. I found no difference between the two methods. 
# the calculation for the 'no winter' season reduction is shown below.
t.ccou=ccou;t.ccou$month=month(t.ccou$sample.date)
t.cint=cint;t.cint$month=month(t.cint$sample.date)
t.cfish=cfish;t.cfish$month=month(t.cfish$sample.date)

t.ifish=ifish
t.nw.harv=calc_creel_harvest(creel_count_data = t.ccou[t.ccou$month%in%c(4:10),],
                             creel_int_data = t.cint[t.cint$month%in%c(4:10),],
                             creel_fish_data = t.cfish[t.cfish$month%in%c(4:10),])
t.nw.fish=t.ifish%>%
  filter(month%in%c(4:10))%>%
  group_by(wbic, year,survey.seq.no,survey.begin.date,survey.end.date, mark.found,month,daytype)%>%
  summarise(nfish=sum(fish.count)) # number of fish of each mark typed returned in that creel

t.nw.fish=t.nw.fish%>% # now throwing out mark type and just taking overall number of marks recapped that month for that survey.
  group_by(wbic, year, survey.seq.no, survey.begin.date, survey.end.date,month,daytype)%>%
  summarise(n.marks.recapped=sum(nfish[!is.na(mark.found)]),
            totalFishExamined=sum(nfish))

# spp.harvest is the column that has the estimate of the harvest for that species in that strata
t.harvestEstimates.nw=t.nw.harv%>%
  filter(species.code=="X22" & wbic%in%ctwiWBIC.creel)%>%
  select(year,wbic,waterbody.name,survey.seq.no, month, daytype,spp.harvest,harvest,total.harvest,total.spp.harvest)

# combining harvest estimates with marked proportion estimate to get total number of marks harvested

t.markHarvEst.nw=t.nw.fish%>%
  left_join(t.harvestEstimates.nw)%>%
  mutate(markHarvEstimate=(n.marks.recapped/totalFishExamined)*spp.harvest)

t.ang.exp.nw=t.markHarvEst.nw%>%
  left_join(fdat, by=c("wbic"="WBIC","year"="Survey.Year","waterbody.name"="Waterbody.Name"))%>%
  mutate(exp.rate=markHarvEstimate/nFN.marked)

# Throwing out NA u's and those >1 (NAs from lack of walleye harvest per strata or missing FN marks, >1's is 6 observations where # marks in creel exceeds # marks at large)
t.2.ang.exp.nw=t.ang.exp.nw[!is.na(t.ang.exp.nw$exp.rate) & t.ang.exp.nw$exp.rate<1.0,]
# creating survey-level exploitation rates
t.3.ang.exp.nw=t.2.ang.exp.nw%>%
  group_by(survey.seq.no)%>%
  summarise(meanU=mean(exp.rate,na.rm = T),
            exp.rate.sd=sd(exp.rate,na.rm = T))



