# making some quick plots to explore the suggestions from olaf
## CJD 7.22.25

# this script is set up to work off the output of the rmd files that hold the main analysis

# saving ttrExp object from each script to make the load ins easier later on
# saveRDS(ttrExp, 'exploitationRate_ttrExp.RData')
#saveRDS(ttrExp, 'harvestRate_ttrExp.RData')
#saveRDS(ttrExp, 'harvest_ttrExp.RData')
#saveRDS(ttrExp, 'catch_ttrExp.RData')
#saveRDS(ttrExp, 'effort_ttrExp.RData')



# P v O DFs ####
widetrL.exp=pivot_wider(readRDS('exploitationRate_ttrExp.RData'), names_from = treat, values_from = c(exp.rate,exp.rate.sd))
widetrL.catch=pivot_wider(readRDS('catch_ttrExp.RData'), names_from = treat, values_from = total.catch)
widetrL.eff=pivot_wider(readRDS('effort_ttrExp.RData'), names_from = treat, values_from = total.eff)
widetrL.harv=pivot_wider(readRDS('harvest_ttrExp.RData'), names_from = treat, values_from = total.harv)
widetrL.harvR=pivot_wider(readRDS('harvestRate_ttrExp.RData'), names_from = treat, values_from = c(meanHarvR,sdHarvR))

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


# multipanel plot for the text

# I'm going to make five ggplots that are each a column of 5 plots (one for each reduction scenario) then all 5 columns (one for each fishery metric) will be combined with ggarrange to make the final plot

lm.exp.nw=lm(exp.rate_noWinter~exp.rate_actual, data=widetrL.exp, offset=exp.rate_actual)
pv.exp.nw=signif(summary(lm.exp.nw)$coefficients[2,4],3)
lm.exp.ma=lm(exp.rate_mayAug~exp.rate_actual, data=widetrL.exp, offset=exp.rate_actual)
pv.exp.ma=signif(summary(lm.exp.ma)$coefficients[2,4],3)
lm.exp.wd25=lm(exp.rate_wd25~exp.rate_actual, data=widetrL.exp, offset=exp.rate_actual)
pv.exp.wd25=signif(summary(lm.exp.wd25)$coefficients[2,4],3)
lm.exp.wd50=lm(exp.rate_wd50~exp.rate_actual, data=widetrL.exp, offset=exp.rate_actual)
pv.exp.wd50=signif(summary(lm.exp.wd50)$coefficients[2,4],3)
lm.exp.we50=lm(exp.rate_we50~exp.rate_actual, data=widetrL.exp, offset=exp.rate_actual)
pv.exp.we50=signif(summary(lm.exp.we50)$coefficients[2,4],3)

lm.harvR.nw=lm(meanHarvR_noWinter~meanHarvR_actual, data=widetrL.harvR, offset=meanHarvR_actual)
pv.harvR.nw=signif(summary(lm.harvR.nw)$coefficients[2,4],3)
lm.harvR.ma=lm(meanHarvR_mayAug~meanHarvR_actual, data=widetrL.harvR, offset=meanHarvR_actual)
pv.harvR.ma=signif(summary(lm.harvR.ma)$coefficients[2,4],3)
lm.harvR.wd25=lm(meanHarvR_wd25~meanHarvR_actual, data=widetrL.harvR, offset=meanHarvR_actual)
pv.harvR.wd25=signif(summary(lm.harvR.wd25)$coefficients[2,4],3)
lm.harvR.wd50=lm(meanHarvR_wd50~meanHarvR_actual, data=widetrL.harvR, offset=meanHarvR_actual)
pv.harvR.wd50=signif(summary(lm.harvR.wd50)$coefficients[2,4],3)
lm.harvR.we50=lm(meanHarvR_we50~meanHarvR_actual, data=widetrL.harvR, offset=meanHarvR_actual)
pv.harvR.we50=signif(summary(lm.harvR.we50)$coefficients[2,4],3)

lm.harv.nw=lm(noWinter~actual, data=widetrL.harv, offset=actual)
pv.harv.nw=signif(summary(lm.harv.nw)$coefficients[2,4],3)
lm.harv.ma=lm(mayAug~actual, data=widetrL.harv, offset=actual)
pv.harv.ma=signif(summary(lm.harv.ma)$coefficients[2,4],3)
lm.harv.wd25=lm(wd25~actual, data=widetrL.harv, offset=actual)
pv.harv.wd25=signif(summary(lm.harv.wd25)$coefficients[2,4],3)
lm.harv.wd50=lm(wd50~actual, data=widetrL.harv, offset=actual)
pv.harv.wd50=signif(summary(lm.harv.wd50)$coefficients[2,4],3)
lm.harv.we50=lm(we50~actual, data=widetrL.harv, offset=actual)
pv.harv.we50=signif(summary(lm.harv.we50)$coefficients[2,4],3)

lm.catch.nw=lm(noWinter~actual, data=widetrL.catch, offset=actual)
pv.catch.nw=signif(summary(lm.catch.nw)$coefficients[2,4],3)
lm.catch.ma=lm(mayAug~actual, data=widetrL.catch, offset=actual)
pv.catch.ma=signif(summary(lm.catch.ma)$coefficients[2,4],3)
lm.catch.wd25=lm(wd25~actual, data=widetrL.catch, offset=actual)
pv.catch.wd25=signif(summary(lm.catch.wd25)$coefficients[2,4],3)
lm.catch.wd50=lm(wd50~actual, data=widetrL.catch, offset=actual)
pv.catch.wd50=signif(summary(lm.catch.wd50)$coefficients[2,4],3)
lm.catch.we50=lm(we50~actual, data=widetrL.catch, offset=actual)
pv.catch.we50=signif(summary(lm.catch.we50)$coefficients[2,4],3)

lm.eff.nw=lm(noWinter~actual, data=widetrL.eff, offset=actual)
pv.eff.nw=signif(summary(lm.eff.nw)$coefficients[2,4],3)
lm.eff.ma=lm(mayAug~actual, data=widetrL.eff, offset=actual)
pv.eff.ma=signif(summary(lm.eff.ma)$coefficients[2,4],3)
lm.eff.wd25=lm(wd25~actual, data=widetrL.eff, offset=actual)
pv.eff.wd25=signif(summary(lm.eff.wd25)$coefficients[2,4],3)
lm.eff.wd50=lm(wd50~actual, data=widetrL.eff, offset=actual)
pv.eff.wd50=signif(summary(lm.eff.wd50)$coefficients[2,4],3)
lm.eff.we50=lm(we50~actual, data=widetrL.eff, offset=actual)
pv.eff.we50=signif(summary(lm.eff.we50)$coefficients[2,4],3)

ep.nw=ggplot(widetrL.exp, aes(x=exp.rate_actual,y=exp.rate_noWinter))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(nw.rmse.exp,4),sep='='), x=0.3,y=0.1)+
  annotate('text',label=paste('p-value',pv.exp.nw,sep='='), x=0.3,y=0.05)+
  labs(x="",y='No Winter', title = 'Exploitation Rate')
ep.ma=ggplot(widetrL.exp, aes(x=exp.rate_actual,y=exp.rate_mayAug))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(ma.rmse.exp,4),sep='='), x=0.3,y=0.1)+
  annotate('text',label=paste('p-value',pv.exp.ma,sep='='), x=0.3,y=0.05)+
  labs(x="",y='May - August')
ep.wd25=ggplot(widetrL.exp, aes(x=exp.rate_actual,y=exp.rate_wd25))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd25.rmse.exp,4),sep='='), x=0.3,y=0.1)+
  annotate('text',label=paste('p-value',pv.exp.wd25,sep='='), x=0.3,y=0.05)+
  labs(x="",y='25% Weekday removed')
ep.wd50=ggplot(widetrL.exp, aes(x=exp.rate_actual,y=exp.rate_wd50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd50.rmse.exp,4),sep='='), x=0.3,y=0.1)+
  annotate('text',label=paste('p-value',pv.exp.wd50,sep='='), x=0.3,y=0.05)+
  labs(x="",y='50% Weekday removed')
ep.we50=ggplot(widetrL.exp, aes(x=exp.rate_actual,y=exp.rate_we50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(we50.rmse.exp,4),sep='='), x=0.3,y=0.1)+
  annotate('text',label=paste('p-value',pv.exp.we50,sep='='), x=0.3,y=0.05)+
  labs(x="Full Creel",y='50% Weekend removed')

e.plots=ggpubr::ggarrange(ep.nw,ep.ma,ep.wd25,ep.wd50,ep.we50, nrow = 5)


hrp.nw=ggplot(widetrL.harvR, aes(x=meanHarvR_actual,y=meanHarvR_noWinter))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(nw.rmse.harvR,4),sep='='), x=0.4,y=0.15)+
  annotate('text',label=paste('p-value',pv.harvR.nw,sep='='), x=0.4,y=0.05)+
  labs(x="",y='No Winter', title = 'Harvest Rate')
hrp.ma=ggplot(widetrL.harvR, aes(x=meanHarvR_actual,y=meanHarvR_mayAug))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(ma.rmse.harvR,4),sep='='), x=0.4,y=0.15)+
  annotate('text',label=paste('p-value',pv.harvR.ma,sep='='), x=0.4,y=0.05)+
  labs(x="",y='May - August')
hrp.wd25=ggplot(widetrL.harvR, aes(x=meanHarvR_actual,y=meanHarvR_wd25))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd25.rmse.harvR,4),sep='='), x=0.4,y=0.15)+
  annotate('text',label=paste('p-value',pv.harvR.wd25,sep='='), x=0.4,y=0.05)+
  labs(x="",y='25% Weekday removed')
hrp.wd50=ggplot(widetrL.harvR, aes(x=meanHarvR_actual,y=meanHarvR_wd50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd50.rmse.harvR,4),sep='='), x=0.4,y=0.15)+
  annotate('text',label=paste('p-value',pv.harvR.wd50,sep='='), x=0.4,y=0.05)+
  labs(x="",y='50% Weekday removed')
hrp.we50=ggplot(widetrL.harvR, aes(x=meanHarvR_actual,y=meanHarvR_we50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(we50.rmse.harvR,4),sep='='), x=0.4,y=0.15)+
  annotate('text',label=paste('p-value',pv.harvR.we50,sep='='), x=0.4,y=0.05)+
  labs(x="Full Creel",y='50% Weekend removed')

hr.plots=ggpubr::ggarrange(hrp.nw,hrp.ma,hrp.wd25,hrp.wd50,hrp.we50, nrow = 5)


hp.nw=ggplot(widetrL.harv, aes(x=actual,y=noWinter))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(nw.rmse.harv,4),sep='='), x=7,y=2.5)+
  annotate('text',label=paste('p-value',pv.harv.nw,sep='='), x=7,y=1)+
  labs(x="",y='No Winter', title = 'Harvest')
hp.ma=ggplot(widetrL.harv, aes(x=actual,y=mayAug))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(ma.rmse.harv,4),sep='='), x=7,y=2.5)+
  annotate('text',label=paste('p-value',pv.harv.ma,sep='='), x=7,y=1)+
  labs(x="",y='May - August')
hp.wd25=ggplot(widetrL.harv, aes(x=actual,y=wd25))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd25.rmse.harv,4),sep='='), x=7,y=2.5)+
  annotate('text',label=paste('p-value',pv.harv.wd25,sep='='), x=7,y=1)+
  labs(x="",y='25% Weekday removed')
hp.wd50=ggplot(widetrL.harv, aes(x=actual,y=wd50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd50.rmse.harv,4),sep='='), x=7,y=2.5)+
  annotate('text',label=paste('p-value',pv.harv.wd50,sep='='), x=7,y=1)+
  labs(x="",y='50% Weekday removed')
hp.we50=ggplot(widetrL.harv, aes(x=actual,y=we50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(we50.rmse.harv,4),sep='='), x=7,y=2.5)+
  annotate('text',label=paste('p-value',pv.harv.we50,sep='='), x=7,y=1)+
  labs(x="Full Creel",y='50% Weekend removed')

h.plots=ggpubr::ggarrange(hp.nw,hp.ma,hp.wd25,hp.wd50,hp.we50, nrow = 5)

cp.nw=ggplot(widetrL.catch, aes(x=actual,y=noWinter))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(nw.rmse.catch,4),sep='='), x=25,y=7)+
  annotate('text',label=paste('p-value',pv.catch.nw,sep='='), x=25,y=4)+
  labs(x="",y='No Winter', title = 'Catch')
cp.ma=ggplot(widetrL.catch, aes(x=actual,y=mayAug))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(ma.rmse.catch,4),sep='='), x=25,y=7)+
  annotate('text',label=paste('p-value',pv.catch.ma,sep='='), x=25,y=4)+
  labs(x="",y='May - August')
cp.wd25=ggplot(widetrL.catch, aes(x=actual,y=wd25))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd25.rmse.catch,4),sep='='), x=25,y=7)+
  annotate('text',label=paste('p-value',pv.catch.wd25,sep='='), x=25,y=4)+
  labs(x="",y='25% Weekday removed')
cp.wd50=ggplot(widetrL.catch, aes(x=actual,y=wd50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(wd50.rmse.catch,4),sep='='), x=25,y=7)+
  annotate('text',label=paste('p-value',pv.catch.wd50,sep='='), x=25,y=4)+
  labs(x="",y='50% Weekday removed')
cp.we50=ggplot(widetrL.catch, aes(x=actual,y=we50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercept = 0)+
  annotate('text',label=paste('RMSE',signif(we50.rmse.catch,4),sep='='), x=25,y=7)+
  annotate('text',label=paste('p-value',pv.catch.we50,sep='='), x=25,y=4)+
  labs(x="Full Creel",y='50% Weekend removed')

catch.plots=ggpubr::ggarrange(cp.nw,cp.ma,cp.wd25,cp.wd50,cp.we50, nrow = 5)

efp.nw=ggplot(widetrL.eff, aes(x=actual,y=noWinter))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercefpt = 0)+
  annotate('text',label=paste('RMSE',signif(nw.rmse.eff,4),sep='='), x=1200,y=250)+
  annotate('text',label=paste('p-value',pv.eff.nw,sep='='), x=1200,y=150)+
  labs(x="",y='No Winter', title = 'Effort')
efp.ma=ggplot(widetrL.eff, aes(x=actual,y=mayAug))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercefpt = 0)+
  annotate('text',label=paste('RMSE',signif(ma.rmse.eff,4),sep='='), x=1200,y=250)+
  annotate('text',label=paste('p-value',pv.eff.ma,sep='='), x=1200,y=150)+
  labs(x="",y='May - August')
efp.wd25=ggplot(widetrL.eff, aes(x=actual,y=wd25))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercefpt = 0)+
  annotate('text',label=paste('RMSE',signif(wd25.rmse.eff,4),sep='='), x=1200,y=250)+
  annotate('text',label=paste('p-value',pv.eff.wd25,sep='='), x=1200,y=150)+
  labs(x="",y='25% Weekday removed')
efp.wd50=ggplot(widetrL.eff, aes(x=actual,y=wd50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercefpt = 0)+
  annotate('text',label=paste('RMSE',signif(wd50.rmse.eff,4),sep='='), x=1200,y=250)+
  annotate('text',label=paste('p-value',pv.eff.wd50,sep='='), x=1200,y=150)+
  labs(x="",y='50% Weekday removed')
efp.we50=ggplot(widetrL.eff, aes(x=actual,y=we50))+theme_classic()+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_abline(slope = 1, intercefpt = 0)+
  annotate('text',label=paste('RMSE',signif(we50.rmse.eff,4),sep='='), x=1200,y=250)+
  annotate('text',label=paste('p-value',pv.eff.we50,sep='='), x=1200,y=150)+
  labs(x="Full Creel",y='50% Weekend removed')

effort.plots=ggpubr::ggarrange(efp.nw,efp.ma,efp.wd25,efp.wd50,efp.we50, nrow = 5)

bigPanel=ggpubr::ggarrange(e.plots,hr.plots,h.plots,catch.plots,effort.plots, ncol = 5)
bigPanel

## RMSE calcs ####

#### exploitation rate rmse ####
# NAs, where the actual was non-zero should be changed to 0 for the treatment, I want this comparison of non-zero to zero to be included in the RMSE calc. NAs where the actual was 0 would still be 0, so they can be changed too, not sure if that's giving me a bonus on the RMSE I shouldn't get. 
widetrL.0=widetrL.exp # copy to change NAs to 0s to
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
nw.rmse.exp=sqrt(mean((widetrL.0$exp.rate_noWinter-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_noWinter))
ma.rmse.exp=sqrt(mean((widetrL.0$exp.rate_mayAug-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_mayAug))
wd25.rmse.exp=sqrt(mean((widetrL.0$exp.rate_wd25-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_wd25))
wd50.rmse.exp=sqrt(mean((widetrL.0$exp.rate_wd50-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_wd50))
we50.rmse.exp=sqrt(mean((widetrL.0$exp.rate_we50-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_we50))
rmse.exp=sqrt(mean((widetrL.0$exp.rate_actual-widetrL.0$exp.rate_actual)^2,na.rm = T))/sum(!is.na(widetrL.0$exp.rate_actual))

nw.rmse.exp
ma.rmse.exp
wd25.rmse.exp
wd50.rmse.exp
we50.rmse.exp
rmse.exp

### total catch rmse ####

# total catch where the actual ==0 can be set to 0 for the reduced scenarios. Total catch where the actual is >0 can be set to 0 still for the reduced scenarios. There's nothing to worry about as far as the catch calculation goes that would result in an NA meaning some walleye non-zero number of walleye were caught (at least as far as the numbers are able to say).

widetrL.0=widetrL.catch # copy to change NAs to 0s to
sum(is.na(widetrL.0$we50))
sum(is.na(widetrL.0$wd50))
sum(is.na(widetrL.0$wd25))
sum(is.na(widetrL.0$mayAug))
sum(is.na(widetrL.0$noWinter))

widetrL.0$noWinter[is.na(widetrL.0$noWinter)]=0
widetrL.0$mayAug[is.na(widetrL.0$mayAug)]=0


# rmse, samples sizes are different after accounting for NAs and 0s for each scenario, then they should be standardized.
nw.rmse=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$noWinter))
ma.rmse=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$mayAug))
wd25.rmse=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd25))
wd50.rmse=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$wd50))
we50.rmse=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$we50))
rmse=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))/sum(!is.na(widetrL.0$actual))

nw.rmse.catch=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse.catch=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse.catch=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse.catch=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse.catch=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse.catch=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse.catch
ma.rmse.catch
wd25.rmse.catch
wd50.rmse.catch
we50.rmse.catch
rmse.catch

#### harvest rmse ####

widetrL.0=widetrL.harv # copy to change NAs to 0s to

sum(is.na(widetrL.0$we50))
sum(is.na(widetrL.0$wd50))
sum(is.na(widetrL.0$wd25))
sum(is.na(widetrL.0$mayAug))
sum(is.na(widetrL.0$noWinter))
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

nw.rmse.harv=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse.harv=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse.harv=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse.harv=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse.harv=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse.harv=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse.harv
ma.rmse.harv
wd25.rmse.harv
wd50.rmse.harv
we50.rmse.harv
rmse.harv

#### harvest rate rmse ####

widetrL.0=widetrL.harvR # copy to change NAs to 0s to

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

nw.rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_noWinter-widetrL.0$meanHarvR_actual)^2,na.rm = T))
ma.rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_mayAug-widetrL.0$meanHarvR_actual)^2,na.rm = T))
wd25.rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_wd25-widetrL.0$meanHarvR_actual)^2,na.rm = T))
wd50.rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_wd50-widetrL.0$meanHarvR_actual)^2,na.rm = T))
we50.rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_we50-widetrL.0$meanHarvR_actual)^2,na.rm = T))
rmse.harvR=sqrt(mean((widetrL.0$meanHarvR_actual-widetrL.0$meanHarvR_actual)^2,na.rm = T))

nw.rmse.harvR
ma.rmse.harvR
wd25.rmse.harvR
wd50.rmse.harvR
we50.rmse.harvR
rmse.harvR

#### effort rmse ####

# there are no observations of 0 effort in the actual data, it's all >0. So all the reduced surveys that are now NA can be changed to 0 because the seasonal removal of the data eliminated the only effort observations for that survey. There are also no 0 eff obs for the % reductions further solidifying the point.

widetrL.0=widetrL.eff # copy to change NAs to 0s to

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

nw.rmse.eff=sqrt(mean((widetrL.0$noWinter-widetrL.0$actual)^2,na.rm = T))
ma.rmse.eff=sqrt(mean((widetrL.0$mayAug-widetrL.0$actual)^2,na.rm = T))
wd25.rmse.eff=sqrt(mean((widetrL.0$wd25-widetrL.0$actual)^2,na.rm = T))
wd50.rmse.eff=sqrt(mean((widetrL.0$wd50-widetrL.0$actual)^2,na.rm = T))
we50.rmse.eff=sqrt(mean((widetrL.0$we50-widetrL.0$actual)^2,na.rm = T))
rmse.eff=sqrt(mean((widetrL.0$actual-widetrL.0$actual)^2,na.rm = T))

nw.rmse.eff
ma.rmse.eff
wd25.rmse.eff
wd50.rmse.eff
we50.rmse.eff
rmse.eff


#### Trudeau inspired plot#### 
d.exp=readRDS('exploitationRate_ttrExp.RData')
d.catch=readRDS('catch_ttrExp.RData')
d.eff=readRDS('effort_ttrExp.RData')
d.harv=readRDS('harvest_ttrExp.RData')
d.harvR=readRDS('harvestRate_ttrExp.RData')

d.exp2=d.exp%>%
  mutate(metric='Exploitation Rate',
         measure.normed=exp.rate/mean(exp.rate[treat=='actual'],na.rm=T))%>%
  select(survey.seq.no,measure.normed,treat,metric)
d.catch2=d.catch%>%
  mutate(metric='Catch',
         measure.normed=total.catch/mean(total.catch[treat=='actual'],na.rm=T))%>%
  select(survey.seq.no,measure.normed, treat,metric)
d.eff2=d.eff%>%
  mutate(metric='Effort',
         measure.normed=total.eff/mean(total.eff[treat=='actual'],na.rm=T))%>%
  select(survey.seq.no,measure.normed,treat,metric)
d.harv2=d.harv%>%
  mutate(metric='Harvest',
         measure.normed=total.harv/mean(total.harv[treat=='actual'],na.rm=T))%>%
  select(survey.seq.no,measure.normed,treat,metric)
d.harvR2=d.harvR%>%
  mutate(metric='Harvest Rate',
         measure.normed=meanHarvR/mean(meanHarvR[treat=='actual'],na.rm=T))%>%
  select(survey.seq.no,measure.normed,treat,metric)
  
d.all=bind_rows(d.exp2,d.catch2,d.eff2,d.harv2,d.harvR2)

d.all.sum=d.all%>%
  group_by(metric, treat)%>%
  summarise(mu=mean(measure.normed,na.rm=T),
            lcl=quantile(measure.normed, probs=0.025,digits=2,na.rm=T),
            ucl=quantile(measure.normed, probs=0.975,digits=2,na.rm=T),
            max=max(measure.normed,na.rm = T),
            min=min(measure.normed,na.rm = T),
            sd=sd(measure.normed,na.rm=T))

ggplot(d.exp)+theme_classic()+
  geom_pointrange(aes(x=treat, y=mean(exp.rate)/mean(exp.rate[treat=='actual']), 
                      ymin=quantile(exp.rate,probs=c(0.025),digits=2)/mean(exp.rate[treat=='actual']), 
                      ymax=quantile(exp.rate,probs=c(0.975),digits=2)/mean(exp.rate[treat=='actual']),color=treat))

ggplot(d.exp)+theme_classic()+
  geom_pointrange(aes(x=treat, y=mean(exp.rate/0.014), 
                      ymin=quantile(exp.rate/0.014,probs=c(0.025),digits=2), 
                      ymax=quantile(exp.rate/0.014,probs=c(0.975),digits=2),color=treat))

ggplot(d.all)+theme_classic()+
  geom_pointrange(aes(x=metric,y=mean(measure.normed,na.rm=T),
                      ymin=quantile(measure.normed, probs=0.025,digits=2,na.rm=T),
                      ymax=quantile(measure.normed, probs=0.975,digits=2,na.rm=T),
                      color=treat), position = position_dodge(width=0.3))+
  scale_y_continuous(limits = c(0,15))+
  scale_color_viridis_d()+
  labs(y='Normalized Creel Measure',x='Creel Metric', color='Data Set')

ggplot(d.all.sum)+theme_classic()+
  geom_pointrange(aes(x=metric,y=mu,
                      ymin=lcl,
                      ymax=ucl,
                      color=treat), position = position_dodge(width=0.3))+
  geom_hline(yintercept = 1,linetype=2)+
  scale_color_viridis_d(option = 'turbo')+
  labs(y='Normalized Creel Measure',x='Creel Metric', color='Data Set')

### parm values and uncertainty plot ####

# pars.all=bind_rows(as.data.frame(pars.a),
#                    as.data.frame(pars.nw),
#                    as.data.frame(pars.ma),
#                    as.data.frame(pars.wd25),
#                    as.data.frame(pars.wd50),
#                    as.data.frame(pars.we50))%>%
#   mutate(treat=c(rep('actual',nrow(pars.a)),
#                  rep('nw',nrow(pars.nw)),
#                  rep('ma',nrow(pars.ma)),
#                  rep('wd25',nrow(pars.wd25)),
#                  rep('wd50',nrow(pars.wd50)),
#                  rep('we50',nrow(pars.we50))),
#          metric='expR')
# saveRDS(pars.all, 'allPars_explR.RData')

# pars.all=bind_rows(as.data.frame(pars.a),
#                    as.data.frame(pars.nw),
#                    as.data.frame(pars.ma),
#                    as.data.frame(pars.wd25),
#                    as.data.frame(pars.wd50),
#                    as.data.frame(pars.we50))%>%
#   mutate(treat=c(rep('actual',nrow(pars.a)),
#                  rep('nw',nrow(pars.nw)),
#                  rep('ma',nrow(pars.ma)),
#                  rep('wd25',nrow(pars.wd25)),
#                  rep('wd50',nrow(pars.wd50)),
#                  rep('we50',nrow(pars.we50))),
#          metric='harvR')
# saveRDS(pars.all, 'allPars_harvR.RData')

# pars.all=bind_rows(as.data.frame(pars.a),
#                    as.data.frame(pars.nw),
#                    as.data.frame(pars.ma),
#                    as.data.frame(pars.wd25),
#                    as.data.frame(pars.wd50),
#                    as.data.frame(pars.we50))%>%
#   mutate(treat=c(rep('actual',nrow(pars.a)),
#                  rep('nw',nrow(pars.nw)),
#                  rep('ma',nrow(pars.ma)),
#                  rep('wd25',nrow(pars.wd25)),
#                  rep('wd50',nrow(pars.wd50)),
#                  rep('we50',nrow(pars.we50))),
#          metric='catch')
# saveRDS(pars.all, 'allPars_catch.RData')

# pars.all=bind_rows(as.data.frame(pars.a),
#                    as.data.frame(pars.nw),
#                    as.data.frame(pars.ma),
#                    as.data.frame(pars.wd25),
#                    as.data.frame(pars.wd50),
#                    as.data.frame(pars.we50))%>%
#   mutate(treat=c(rep('actual',nrow(pars.a)),
#                  rep('nw',nrow(pars.nw)),
#                  rep('ma',nrow(pars.ma)),
#                  rep('wd25',nrow(pars.wd25)),
#                  rep('wd50',nrow(pars.wd50)),
#                  rep('we50',nrow(pars.we50))),
#          metric='harv')
# saveRDS(pars.all, 'allPars_harv.RData')

# pars.all=bind_rows(as.data.frame(pars.a),
#                    as.data.frame(pars.nw),
#                    as.data.frame(pars.ma),
#                    as.data.frame(pars.wd25),
#                    as.data.frame(pars.wd50),
#                    as.data.frame(pars.we50))%>%
#   mutate(treat=c(rep('actual',nrow(pars.a)),
#                  rep('nw',nrow(pars.nw)),
#                  rep('ma',nrow(pars.ma)),
#                  rep('wd25',nrow(pars.wd25)),
#                  rep('wd50',nrow(pars.wd50)),
#                  rep('we50',nrow(pars.we50))),
#          metric='effort')
# saveRDS(pars.all, 'allPars_effort.RData')

p.exp=readRDS('allPars_explR.RData')
p.harvR=readRDS('allPars_harvR.RData')
p.harv=readRDS('allPars_harv.RData')
p.catch=readRDS('allPars_catch.RData')
p.eff=readRDS('allPars_effort.RData')

p.all=bind_rows(p.exp,
                p.harvR,
                p.harv,
                p.catch,
                p.eff)%>%
  rename(meanLogpar='par 1',
         sdLogpar='par 2')%>%
  group_by(metric,treat)%>%
  summarise(meanLog=median(meanLogpar),
            meanLog.lcl=quantile(meanLogpar,probs=0.025),
            meanLog.ucl=quantile(meanLogpar,probs=0.975),
            sdLog=median(sdLogpar),
            sdLog.lcl=quantile(sdLogpar,probs=0.025),
            sdLog.ucl=quantile(sdLogpar,probs=0.975))

metNames=c('catch'='Catch (walleye/acre)',
           'effort'='Effort (hours/acre)',
           'expR'='Exploitation Rate',
           'harv'='Harvest (wallye/acre)',
           'harvR'='Harvest Rate')

ggplot(p.all)+theme_classic()+
  geom_pointrange(aes(x=treat, y=meanLog,  ymin=meanLog.lcl, ymax=meanLog.ucl))+
  scale_x_discrete(label=c('Actual','May-August','No Winter','25% Weekday Removed','50% Weekday Removed','50% Weekend Removed'))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  facet_wrap(~metric, scales = 'free', labeller = as_labeller(metNames))+
  labs(y='Mean of Lognormal Distribution', x='Data Set')

ggplot(p.all)+theme_classic()+
  geom_pointrange(aes(x=treat, y=sdLog,  ymin=sdLog.lcl, ymax=sdLog.ucl))+
  scale_x_discrete(label=c('Actual','May-August','No Winter','25% Weekday Removed','50% Weekday Removed','50% Weekend Removed'))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  facet_wrap(~metric, scales = 'free', labeller = as_labeller(metNames))+
  labs(y='Stan.Dev. of Lognormal Distribution', x='Data Set')

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



