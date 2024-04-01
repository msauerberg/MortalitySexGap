### From the previous code you will get
### 7 country-specific files with SDRs
setwd("")

library(dplyr)

## load data
FR <- read.table("FR_SDR_smoothed_CoD_NUTS3.txt")
AT <- read.table("AT_SDR_smoothed_CoD_NUTS2.txt")
CH <- read.table("CH_SDR_smoothed_CoD_NUTS2.txt")
DK <- read.table("DK_SDR_smoothed_CoD_NUTS2.txt")
SK <- read.table("SK_SDR_smoothed_CoD_NUTS3.txt")
CZ <- read.table("CZ_SDR_smoothed_CoD_NUTS2.txt")
DE <- read.table("DE_SDR_smoothed_CoD_ROR.txt")

### make data for Germany comparable as it was
### slightly different formatted (data could only be accessed through FDZ)
colnames(DE)[4] <- "FID"
DE.cause <- select(DE, c(Year, FID, Sex, Cause, SDR.s))
DE.cause$FID <- paste("DE", DE.cause$FID, sep="")

colnames(FR)[3] <- "FID"
colnames(AT)[4] <- "FID"
colnames(DK)[4] <- "FID"
colnames(CH)[4] <- "FID"
colnames(SK)[4] <- "FID"
colnames(CZ)[4] <- "FID"

allC <- rbind(FR, AT, CH, DK, CZ, SK)
allC <- filter(select(allC, c(Year, Age, Sex, FID, SDR.s, Cause)), Age==0)
allC <- select(allC, -Age)
allC <- rbind(allC, DE.cause)

AllC_all <- allC %>% group_by(Year, FID, Sex) %>%
  summarise(AllCause = sum(SDR.s))

AllC_m <- filter(AllC_all, Sex=="m")
colnames(AllC_m)[4] <- c("SDR_m")
AllC_f <- filter(AllC_all, Sex=="f")
colnames(AllC_f)[4] <- c("SDR_f")

Gap <- merge(select(AllC_f, -Sex), select(AllC_m, -Sex), by=c("Year","FID"), all=TRUE)
Gap$Gap <- Gap$SDR_m - Gap$SDR_f

cause.m <- select(filter(allC, Year %in% c(1996, 2017) & Sex=="m"), -Sex)
cause.f <- select(filter(allC, Year %in% c(1996, 2017) & Sex=="f"), -Sex)
colnames(cause.m)[3] <- c("SDR_m")
colnames(cause.f)[3] <- c("SDR_f")

cause <- merge(cause.m, cause.f, by=c("Year","FID","Cause"), all=TRUE)
cause$Gap <- cause$SDR_m - cause$SDR_f
gap1 <- select(filter(cause, Year==1996), c(Gap, FID, Cause))
gap2 <- select(filter(cause, Year==2017), c(Gap, FID, Cause))
colnames(gap1)[1] <- c("Gap_1996")
colnames(gap2)[1] <- c("Gap_2017")
gap <- merge(gap1, gap2, by=c("FID","Cause"), all=TRUE)
wide1 <- spread(gap[,c("FID","Cause","Gap_1996")], key = Cause, value = Gap_1996)
wide2 <- spread(gap[,c("FID","Cause","Gap_2017")], key = Cause, value = Gap_2017)
colnames(wide1)[-1] <- paste(colnames(wide1)[-1], "_1996", sep="")
colnames(wide2)[-1] <- paste(colnames(wide2)[-1], "_2017", sep="")
wide <- merge(wide1, wide2, by=c("FID"), all=TRUE)
wide$AllCn96 <- wide$Cancer_1996 + wide$LungC_1996
wide$AllCn17 <- wide$Cancer_2017 + wide$LungC_2017

write.table(AllC_all, "All_Cause_SDR.txt")
write.table(Gap, "Gap_SDR.txt")
write.table(wide, "GapCause_SDR.txt")

setwd("")
dta <- read.table("GapCause_SDR.txt") #regional sex gaps in cause-specific SDRs 
AllC_all <- read.table("All_Cause_SDR.txt") #All-cause SDRs by regions
Gap <- read.table("Gap_SDR.txt") #regional sex gaps in all-cause SDRs

AllC_all$Year <- as.numeric(AllC_all$Year)
allC$Year <- as.numeric(allC$Year)

### empty frame
final <- data.frame(Cause=NA, Contr=NA, FID=NA, Country=NA)
### loop over all regions in seven countries
for (i in unique(AllC_all$FID)) {
  
  the.regionCode <- i
  
  all.c <- c(arrange(filter(AllC_all, Sex=="m" & FID==i), Year)$AllCause-
               arrange(filter(AllC_all, Sex=="f" & FID==i), Year)$AllCause
  )
  
  years <- arrange(filter(AllC_all, Sex=="f" & FID==i), Year)$Year
  
  lung.c <- c(arrange(filter(allC, Sex=="m" & FID==i & Cause=="LungC"),Year)$SDR.s-
                arrange(filter(allC, Sex=="f" & FID==i & Cause=="LungC"),Year)$SDR.s
  )
  
  cancers.c <- c(arrange(filter(allC, Sex=="m" & FID==i & Cause=="Cancer"),Year)$SDR.s-
                   arrange(filter(allC, Sex=="f" & FID==i & Cause=="Cancer"),Year)$SDR.s
  )
  
  CVD.c <- c(arrange(filter(allC, Sex=="m" & FID==i & Cause=="CVD"),Year)$SDR.s-
               arrange(filter(allC, Sex=="f" & FID==i & Cause=="CVD"),Year)$SDR.s
  )
  
  External.c <- c(arrange(filter(allC, Sex=="m" & FID==i & Cause=="External"),Year)$SDR.s-
                    arrange(filter(allC, Sex=="f" & FID==i & Cause=="External"),Year)$SDR.s
  )
  
  rest.c <- c(arrange(filter(allC, Sex=="m" & FID==i & Cause=="Rest"),Year)$SDR.s-
                arrange(filter(allC, Sex=="f" & FID==i & Cause=="Rest"),Year)$SDR.s
  )
  
  beta.lung <- lm(lung.c ~ all.c)$coefficients[2]
  beta.cancers <- lm(cancers.c ~ all.c)$coefficients[2]
  beta.CVD <- lm(CVD.c ~ all.c)$coefficients[2]
  beta.external <- lm(External.c ~ all.c)$coefficients[2]
  beta.rest <- lm(rest.c ~ all.c)$coefficients[2]
  
  beta.lung + beta.cancers + beta.CVD + beta.external + beta.rest
  
  out <- data.frame(Cause = c("LungC", "Cancer", "CVD", "External", "Rest", "Sum"),
                    Contr = c(beta.lung, beta.cancers, beta.CVD, beta.external, beta.rest,
                              sum(c(beta.lung, beta.cancers, beta.CVD, beta.external, beta.rest)))*100)
  
  out$FID <- the.regionCode
  out$Country <- substr(the.regionCode,1,2)
  final <- rbind(final, out)
  
}

write.table(final, "Contributions.txt")
