### the country-specific data file cannot be shared
### due to data protection
### The files are long-formatted data sets with colnames
### Year, Age, Sex, Region, Cause, Dx, Ex

setwd("")
CoD.out <- read.table("our cause-specific data")
# cause-specific data includes the causes
# Cancer, Lung Cancer, CVD, External, and Rest
allC.out <- read.table("our all-cause data")
# all-cause data refers to the aggregated file, i.e.,
# sums all cause-specific deaths

### smoothing and getting ASDR
the.ages <- unique(CoD.out$Age)

## Re-weighted standard ##
PSrw<-function(Wx=c(0.01+0.04+0.055, sum(rep(0.055,2)), 0.06, 0.06, 0.065, rep(0.07,4), 0.065,
                    0.06, 0.055, 0.05, 0.04, 0.025, 0.015, 0.01),
               scale=c(0,10, seq(20,90,5)), From=0, To=85){
  dta<-as.data.frame(cbind(scale,Wx))
  PS<-dta[dta$scale>=From&dta$scale<=To,2]
  TOT<-sum(PS)
  PSw<-round(PS/TOT*(1-TOT)+PS,3)
  return(PSw) }

W<-PSrw()
round(sum(PSrw()),2)

W.frame <- data.frame(W=W, Age=the.ages)

library(dplyr)
##
CoD.out <- CoD.out %>% left_join(., W.frame)
allC.out <- allC.out %>% left_join(., W.frame)

##
CoD.out$mx <- CoD.out$Dx / CoD.out$Ex
ASDR.raw.CoD <- CoD.out %>% group_by(Year, Sex, Cause, Region) %>%
  mutate(SDR = sum(mx * W)* 100000)

allC.out$mx <- allC.out$Dx / allC.out$Ex
ASDR.raw.allC <- allC.out %>% group_by(Year, Sex, Cause, Region) %>%
  mutate(SDR = sum(mx * W)* 100000)


library(MortalitySmooth)
CoD.out <- arrange(CoD.out, Year, Sex, Region, Cause, Age)

ASDR.smoothed.CoD <- CoD.out %>% group_by(Year, Sex, Region, Cause) %>%
  mutate(mx.s = exp(Mort1Dsmooth(Age, Dx/100, log(Ex/100), method=4, df=5)$logmortality),
         SDR.s = sum(mx.s * W) * 100000)

### plot to look whether the results make sense and seems reliable
# do for both, women and men
for (region in unique(ASDR.smoothed.CoD$Region)) {
  
  plot(unique(ASDR.smoothed.CoD$Year),
       c(filter(ASDR.smoothed.CoD, Sex=="f" & Age==0 & Region==region & Cause=="CVD")$SDR.s+
           filter(ASDR.smoothed.CoD, Sex=="f" & Age==0 & Region==region & Cause=="Cancer")$SDR.s+
           filter(ASDR.smoothed.CoD, Sex=="f" & Age==0 & Region==region & Cause=="LungC")$SDR.s+
           filter(ASDR.smoothed.CoD, Sex=="f" & Age==0 & Region==region & Cause=="External")$SDR.s+
           filter(ASDR.smoothed.CoD, Sex=="f" & Age==0 & Region==region & Cause=="Rest")$SDR.s), type="l")
  
  points(unique(ASDR.raw.CoD$Year),
         filter(ASDR.raw.allC, Sex=="f" & Age==0 & Region==region)$SDR)
  Sys.sleep(2)
}


### write file
write.table(ASDR.smoothed.CoD, "CountryCode_SDR_smoothed_CoD_Region.txt")
