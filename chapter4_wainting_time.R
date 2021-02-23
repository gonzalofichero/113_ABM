#### Importing libraries ####
library(tidyverse)
library (msm)  # for function rpexp
library (lubridate) # for function date_decimal
library(HMDHFDplus) # Read life tables from HMD


#### Reproducing figure 4.6 ####
country <- getHMDcountries()

##### Extracting data table from HMD for Lithuanian females ####
ltu_f <- readHMDweb(CNTRY="LTU",item="fltper_1x1",
                    username="gonzalo.fce@gmail.com",
                    password="fermat31416",
                    fixup=TRUE)


##### Age-specific death rates females 2016 in Lithuania, ages 0-110 #####
ltu_f %>% 
  filter(Year == 2016) %>% 
  select(mx) -> rates


lower <- 0:110
upper <- 1:111


##### Check that uniroot function runs correctly #####
uniroot(f=pw.root,
        interval=c(0,111), 
        u=runif(1), low=lower, up=upper, coef=rates)


##### Actually using uniroot function #####

# Empty vector
x_D <- vector(mode="numeric",length=1000)

# Using uniroot for each data point and saving results in previous vector
for (i in 1:1000) 
{ x_D[i] <- uniroot(f=pw.root,
                    interval=c(0,111), 
                    u=runif(1), 
                    low=lower, 
                    up=upper, 
                    coef=rates,
                    extendInt="yes")$root
}

# Checking that mean is the same as expected: Uniroot vs raw data (Expected value from life-table)
mean(x_D)  # e(0)=83.12 
ltu_f[ltu_f$Year==2016 & ltu_f$Age==0,]$ex


##### Histogram plot: figure 4.6 #####
hist(x_D, breaks=0:110, xlab="Age", main=" ", las=1, cex.axis=0.8)
box()
title(main="Histogram of simulated lifespans generated from age-specific death rates\n
Female population, Lithuania, 2016", cex.main=0.8)
# Please take into account that the (x,y) position of the figure is dependent of your data
# Manual change of this to look good...
text(x=20,y=45, labels=paste("Mean age at death =", round(mean(x_D),2), sep=" "))



#### Reproducing figure 4.7 ####




