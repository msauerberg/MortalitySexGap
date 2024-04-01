### functions we need...

# calculates e0 for women and men
LE.calculator <- function(Mx.men, Mx.women, dims) {
  
  dim(Mx.men) <- dims
  dim(Mx.women) <- dims
  
  Mx.m <- rowSums(Mx.men)
  Mx.w <- rowSums(Mx.women)
  
  c(LTabr(Mx.w), LTabr(Mx.m))
}

# calculates the gap in e0
Gap.calculator <- function(Mx.men, Mx.women) {
  
  Mx.m <- rowSums(Mx.men)
  Mx.w <- rowSums(Mx.women)
  
  LTabr(Mx.w)- LTabr(Mx.m)
}

# very simple life table function
LTabr <- function(Mx){
  x <- c(0, 10, 20, seq(25,85,5))
  m <- length(x)
  mx  <- Mx
  n <- c(diff(x), NA)
  ax <- n/2
  ax[m] <- 1 / mx[m]
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return(ex[1])
}

# for the decomposition
Mx.gap.fun <- function(Mx.both, dims) {
  
  n <- length(Mx.both)  
  half <- n/2
  Mx.men <- Mx.both[c(1:half)]
  Mx.women <- Mx.both[c(half+1):n]
  
  dim(Mx.men) <- dims
  dim(Mx.women) <- dims
  
  Gap.calculator(Mx.men, Mx.women)
}

library(DemoDecomp)#thanks Tim Riffe for writing this package and
# helping me applying it to this specific decomposition problem
library(dplyr)
library(openxlsx)

for (country_name in c("AT", "CH", "CZ", "DK", "FR", "SK")) {
  
  ### overall directory
  setwd("")
  
  dta <- read.table(paste(country_name,
                          "_SDR_smoothed_CoD.txt", sep=""))
  dta <- filter(select(dta, -c(Dx, Ex, W, mx, SDR.s)), Year %in% c(1996, 2017))
  reg.i <- !(colnames(dta) %in% c("Year", "Age", "Sex", "Cause", "mx.s"))
  colnames(dta)[reg.i] <- "Region"
  
  ### getting the list with matrix by region and sex
  list.men_t0 <- list()
  list.women_t0 <- list()
  
  list.men_t1 <- list()
  list.women_t1 <- list()
  
  the.regions <- unique(dta$Region)
  
  for (i in 1:length(the.regions)) {
    
    regions <- the.regions[i]
    
    the.matrix_t0_men <- matrix(filter(dta,
                                       Year==1996 & Sex=="m" & 
                                         Region==regions)$mx.s,
                                nrow = length(unique(dta$Age)),
                                ncol = length(unique(dta$Cause)))
    
    the.matrix_t1_men <- matrix(filter(dta,
                                       Year==2017 & Sex=="m" & 
                                         Region==regions)$mx.s,
                                nrow = length(unique(dta$Age)),
                                ncol = length(unique(dta$Cause)))
    
    the.matrix_t0_women <- matrix(filter(dta,
                                         Year==1996 & Sex=="f" & 
                                           Region==regions)$mx.s,
                                  nrow = length(unique(dta$Age)),
                                  ncol = length(unique(dta$Cause)))
    
    the.matrix_t1_women <- matrix(filter(dta,
                                         Year==2017 & Sex=="f" & 
                                           Region==regions)$mx.s,
                                  nrow = length(unique(dta$Age)),
                                  ncol = length(unique(dta$Cause)))
    
    list.men_t0[[i]] <- the.matrix_t0_men
    list.women_t0[[i]] <- the.matrix_t0_women
    
    list.men_t1[[i]] <- the.matrix_t1_men
    list.women_t1[[i]] <- the.matrix_t1_women
    
  }
  names(list.men_t0) <- the.regions
  names(list.men_t1) <- the.regions
  names(list.women_t0) <- the.regions
  names(list.women_t1) <- the.regions
  
  setwd(paste(".../", country_name, sep=""))
  
  ### loop over regions
  for (j in 1:length(the.regions)) {
    
    ### preparing inputs
    dims <- dim(list.men_t0[[j]])
    dim.names <- unique(dta$Cause)
    
    # in 1996
    Mx.men <- c(list.men_t0[[j]])
    Mx.women <- c(list.women_t0[[j]])
    Mx.both <- c(Mx.men, Mx.women)
    
    # in 2019
    Mx.men2 <- c(list.men_t1[[j]])
    Mx.women2 <- c(list.women_t1[[j]])
    Mx.both2 <- c(Mx.men2, Mx.women2)
    
    #e0 in t0 and t1 and gaps
    e0_t0 <- LE.calculator(Mx.men, Mx.women, dims=dims)
    e0_t1 <- LE.calculator(Mx.men2, Mx.women2, dims=dims)
    gap_t0 <- -diff(e0_t0)
    gap_t1 <- -diff(e0_t1)
    Gap.change <- diff(c(gap_t0, gap_t1))
    gap.frame <- data.frame(W_96to98 = e0_t0[1],
                            M_96to98 = e0_t0[2],
                            Gap_96to98 = gap_t0,
                            W_17to19 = e0_t1[1],
                            M_17to19 = e0_t1[2],
                            Gap_17to19 = gap_t1,
                            Gap_Change = gap_t0-gap_t1)
    rownames(gap.frame) <- c("Value")
    #decompo of change in gap by age and cause
    B <- stepwise_replacement(func = Mx.gap.fun,
                              pars1 = Mx.both, pars2 = Mx.both2, dims = dims,
                              symmetrical = TRUE, direction = "up")
    
    #check whether numbers add up
    if(round(sum(B)-Gap.change)!=0) {
      warning("Sums do not add up")
    }
    
    # contributions:
    contribution.men <- B[c(1:length(Mx.men))]
    dim(contribution.men) <- dims
    contribution.women <- B[c(length(Mx.women)+1):length(Mx.both)]
    dim(contribution.women) <- dims
    
    men.contri <- as.data.frame(contribution.men,
                                row.names = unique(dta$Age))
    colnames(men.contri) <- unique(dta$Cause)
    rownames(men.contri) <- unique(dta$Age)
    
    women.contri <- as.data.frame(contribution.women,
                                  row.names = unique(dta$Age))
    colnames(women.contri) <- unique(dta$Cause)
    rownames(women.contri) <- unique(dta$Age)
    
    all.ages.women <- colSums(women.contri)
    all.ages.women <- c(all.ages.women, sum(all.ages.women))
    names(all.ages.women)[6] <- "Total"
    
    all.ages.men <- colSums(men.contri)
    all.ages.men <- c(all.ages.men, sum(all.ages.men))
    names(all.ages.men)[6] <- "Total"
    
    all <- rbind(all.ages.women, all.ages.men)
    all <- rbind(all, colSums(all))
    rownames(all) <- c("Women", "Men", "Total")
    
    ### save outputs
    OUT <- createWorkbook()
    
    # Add some sheets to the workbook
    addWorksheet(OUT, "LE_and_Gap")
    addWorksheet(OUT, "Men_by_Age")
    addWorksheet(OUT, "Women_by_Age")
    addWorksheet(OUT, "All_Ages")
    
    # Write the data to the sheets
    writeData(OUT, sheet = "LE_and_Gap", x = gap.frame, rowNames = T)
    writeData(OUT, sheet = "Men_by_Age", x = men.contri, rowNames = T)
    writeData(OUT, sheet = "Women_by_Age", x = women.contri, rowNames = T)
    writeData(OUT, sheet = "All_Ages", x = all, rowNames = T)
    
    # Export the file
    saveWorkbook(OUT, paste("Output_", the.regions[j], ".xlsx",
                            sep=""))
    
  }
  
}


