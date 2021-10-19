# clear global environment

rm(list=ls())

# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# function to load libraries

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),pkgTest)
lapply(c("ggplot2"), pkgTest)
lapply(c("data.table"), pkgTest)
lapply(c("ggpubr"), pkgTest)
lapply(c("naniar"), pkgTest)
lapply(c("MASS"), pkgTest)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(naniar)
library(MASS)
library(RColorBrewer)
library(car)

# importing individual psid adulthood data  

indv_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2013INDIV.csv")

# importing childhood retrospective circumstances study data 

crcs_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2014CRCS.csv")

# merging two datasets by index id 

indv_data$INB_13 <- indv_data$ER53002
crcs_data$INB_13 <- crcs_data$ER34201

dat <- merge(x = indv_data, y = crcs_data, by="INB_13")

##### reg 1 #####

# outcome variable (ER57482): K-6 non-specific psychological distress scale score in adulthood 

summary(dat$ER57482)
table(dat$ER57482)

dat <- dat %>% 
  replace_with_na(replace = list(ER57482 = 99)) 

# predictor variable (CS14V285): retrospective rating of "how much affection mom gave" 

summary(dat$CS14V285)
table(dat$CS14V285)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V285 = c(0, 9)))

# predictor variable (CS14V298): retrospective rating of "how much affection dad gave" 

summary(dat$CS14V298)
table(dat$CS14V298)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V298 = c(0, 9)))

# multivariate linear regression 1

reg1 <- lm(ER57482 ~ CS14V285 + CS14V298 + CS14V285*CS14V298, data = dat)

summary(reg1)

d1 <- data.frame(x=dat$ER57482,
                Mom=dat$CS14V285,
                Dad=dat$CS14V298)
library(ggplot2) 
library(reshape2) ## for melt()

dm1 <- melt(d1,id.var=1)
plot1 <- ggplot(data=dm1,aes(x,value,colour=variable))+
  geom_point(alpha=0.2)+
  scale_colour_manual(values=c("red","blue"))+
  labs(x="Psychological Distress in Adulthood",y="Affection Provided by Parent in Childhood (1 = a lot, 4 = none at all)") + 
  geom_smooth(method='lm', formula= y~x)

##### reg 2 #####

# predictor variable: how strict was mom

summary(dat$CS14V284)
table(dat$CS14V284)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V284 = c(0, 9)))

# predictor variable: how strict was dad

summary(dat$CS14V297)
table(dat$CS14V297)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V297 = c(0, 9)))

# multivariate linear regression 

reg2 <- lm(ER57482 ~ CS14V284 + CS14V297 + CS14V284*CS14V297, data = dat)

summary(reg2)

d2 <- data.frame(x=dat$ER57482,
                Mom=dat$CS14V284,
                Dad=dat$CS14V297)
library(ggplot2) 
library(reshape2) ## for melt()

dm2  <- melt(d2,id.var=1)
plot2 <- ggplot(data=dm2,aes(x,value,colour=variable))+
  geom_point(alpha=0.2)+
  scale_colour_manual(values=c("red","blue"))+
  labs(x="Psychological Distress in Adulthood",y="Parent's Strictness in Childhood (1 = very strict, 4 = not at all strict)") + 
  geom_smooth(method='lm', formula= y~x)

##### reg 3 #####

# predictor variable: relationship status with mom

summary(dat$CS14V282)
table(dat$CS14V282)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V282 = c(0, 9)))

# predictor variable: relationship status with dad

summary(dat$CS14V295)
table(dat$CS14V295)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V295 = c(0, 9)))

# multivariate linear regression 

reg2 <- lm(ER57482 ~ CS14V282 + CS14V295 + CS14V282*CS14V295, data = dat)

summary(reg3)

d3 <- data.frame(x=dat$ER57482,
                 Mom=dat$CS14V282,
                 Dad=dat$CS14V295)
library(ggplot2) 
library(reshape2) ## for melt()

dm3  <- melt(d3,id.var=1)
plot3 <- ggplot(data=dm3,aes(x,value,colour=variable))+
  geom_point(alpha=0.2)+
  scale_colour_manual(values=c("red","blue"))+
  labs(x="Psychological Distress in Adulthood",y="Relationship Quality with Parent (1 = excellent, 5 = poor)") + 
  geom_smooth(method='lm', formula= y~x)

ggarrange(plot1 + rremove("legend"), plot2 + rremove("legend"), plot3, ncol = 3, nrow = 1)
