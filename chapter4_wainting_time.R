#### Importing libraries ####
library(tidyverse)
library (msm)  # for function rpexp
library (lubridate) # for function date_decimal
library(HMDHFDplus) # Read life tables from HMD


##### HMD user and credentials, please ####
user.n <- userInput()
user.pass <- userInput()
##### Which Year you want to analyze? ####
refyear <- 2016
##### Which Country you want to analyze? ####
cty <- "LTU"



#### Reproducing figure 4.6 ####
country <- getHMDcountries()

##### Extracting data table from HMD for Lithuanian females ####
ltu_f <- readHMDweb(CNTRY= cty,item="fltper_1x1",
                    username= user.n,
                    password= user.pass,
                    fixup=TRUE)


##### Age-specific death rates females reference Year in Lithuania, ages 0-110 #####
ltu_f %>% 
  filter(Year == refyear) %>% 
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
mean(x_D)  # e(0) = 79.98 
ltu_f[ltu_f$Year==2016 & ltu_f$Age==0,]$ex


##### Histogram plot: figure 4.6 #####
hist(x_D, breaks=0:110, xlab="Age", main=" ", las=1, cex.axis=0.8)
box()
title(main= paste("Histogram of simulated lifespans generated from age-specific death rates\n
Female population, Lithuania, ", refyear),cex.main=0.8)
# Please take into account that the (x,y) position of the figure is dependent of your data
# Manual change of this to look good...
text(x=20,y=45, labels=paste("Mean age at death =", round(mean(x_D),2), sep=" "))



#### Reproducing figure 4.7 ####

##### Extracting data table from HMD for Lithuanian males ####
ltu_m <- readHMDweb(CNTRY= cty,item="mltper_1x1",
                    username= user.n,
                    password= user.pass,
                    fixup=TRUE)


##### Converting Ages to character: grouping the +110 years old ####
z <- as.character(ltu_m$Age)
zz <- ifelse(z == "110+", 110, z)
ltu_m$Age <- ltu_f$Age <- as.numeric(zz)


# Merge life tables for males, females into list file
# Why list file...?
ltu <- list(Males=ltu_m, Females=ltu_f)

##### Select mortality rates for reference year (males, females): mx data.frame ##### 


mx <- data.frame(
                  sapply(ltu,function(x)
                                        { 
                                        m=x$mx[x$Year==refyear]
                                        }
                         )
                 )

##### Keeping Ages available for ##### 
ages <- ltu$Males$Age[ltu$Males$Year==refyear]


##### Initialize data frame with info on simulated pop #####

###### Birth Cohort for Lithuania Reference Year ###### 

ltu_birth <- readHMDweb(CNTRY=cty,item="Births",
                    username= user.n,
                    password= user.pass,
                    fixup=TRUE)

###### First generation: number of births ######
ltu_birth %>% 
  filter(Year == refyear) %>% 
  select(Total) %>% 
  as.numeric() -> nsample

# Completely unnecessary to create another variable
year <- refyear


# Give ID
ID <- 1:nsample
data <- data.frame(ID=ID)


###### Randomly allocate sex to members of population ###### 
# sex <- rbinom(nsample,1,prob=1/2.05) # male=0; female=1

m <- sample(x=ID, 
            size=ltu_birth %>% 
                            filter(Year == refyear) %>% 
                            select(Male) %>% 
                            as.numeric(), 
            replace=FALSE)

###### Levels for factors ######
data$sex=1
data$sex[m] <- 0
data$sex <- factor(data$sex,levels=c(0,1),labels=c("Male","Female"))

# Check quantities
table(data$sex)

###### How many people sample from each sex? ######
nmales <- length(data$sex[data$sex=="Male"])
nfemales <- length(data$sex[data$sex=="Female"])
nsample_sex <- c(nmales,nfemales)


#####  Determine date of birth in reference year ##### 
data$bdate=NA
data$bdated=NA

# Generating random dates of birth for each individual sampled
data$bdated[data$sex=="Male"] <- year+runif(nmales)
data$bdated[data$sex=="Female"] <- year+runif(nfemales)
data$bdate <- format(lubridate::date_decimal(data$bdated), "%Y-%m-%d")  


#####  Determine ages at death that are consistent with ASDR #####
# Simulate length of life using age-specific death rates 
# and the rpexp function from the package msm 

set.seed(42)
#getseed <- .Random.seed

data$ddate <- NA
data$ddated <- NA
data$x_D <- NA

# Simulate from age_start (eg age on 1 January Reference Year) to highest age 
#    (see Lifespan_simul.r in r_pension)

age_start <- 0  # should be > than current age
n1 <- floor(age_start) + 1 # line number of age (=age+1 since age 0 is 1) 
n2 <- length(mx$Males)
nage2 <- n2-n1+1
rates <- mx$Males[n1:n2]
agess <- ages[0:nage2]

data$x_D[data$sex=="Male"] <- rpexp(n=nmales,rate=rates,agess)
data$x_D[data$sex=="Female"] <- rpexp(n=nfemales,rate=mx$Females,ages)

# Simulated age should be < 120
# Why 120?
data$x_D[data$x_D>120] <- 120
data$x_D <- round (data$x_D,3)

# Check ex for each sex
aggregate(data$x_D,by=list(data$sex),FUN="mean")

# Date of death (calendar date)
data$ddated <- data$bdated+data$x_D
data$ddate <- format(date_decimal(data$ddated), "%Y-%m-%d")



#####  Mean ages at death #####
 
eD <- vector(mode="numeric",length=2)
# Mean age at death of THOSE MEN ALIVE at starting data 
eD[1]  <- round (mean(data$x_D[data$sex=="Male"],na.rm=TRUE),2) 
# Mean age at death of THOSE WOMEN ALIVE at starting data 
eD[2]  <- round (mean(data$x_D[data$sex=="Female"],na.rm=TRUE),2)
# Mean age at death of THOSE ALIVE at starting data 
eD[3]  <- round (mean(data$x_D),2)


##### Plot ages at death: same as figure 4.6!!! #####

hist(x_D,breaks=0:110,xlab="Age",main=" ",las=1,cex.axis=0.8)
text(x=20,y=40,labels=paste("Mean age at death =",round(mean(x_D),3),sep=""))
box()
title(main= paste("Histogram of simulated lifespans generated from age-specific death rates\n
Female population, Lithuania, ", refyear),cex.main=0.8)


###### Ggplotting both sexes ###### 
data %>% 
  ggplot () +
  geom_density(data=data,aes(round (x_D,0),fill=sex,color=sex),alpha=0.3) +
  ggtitle (paste("Figure 4.7. Ages at death, Lithuania, ", refyear, " Simulated and LT")) +
  xlab("Age") + ylab("Density") +
  theme(legend.position="bottom") +
  scale_x_continuous (breaks=seq(0,110,by=10)) -> p


##### Adding density from life table for reference year #####
  
# Deaths by Sex
dm <- ltu$Male$dx[ltu$Male$Year== refyear]
df <- ltu$Female$dx[ltu$Male$Year== refyear]

# Manual density
density_m <- dm/sum(dm)
density_f <- df/sum(df)

# Into data.frame
density_m <- data.frame(x_D=ages,sexx="Malex",density=dm/sum(dm))
density_f <- data.frame(x_D=ages,sexx="Femalex",density=df/sum(df))

# All together
density <- rbind (density_m,density_f)

# Adding to ggplot new data
p +
  geom_line(data=density,aes(x=x_D,y=density,group=sexx,color=sexx)) +
  scale_colour_manual(values=c(Male="tomato1",Female="skyblue",Malex="red",Femalex="blue")) 


