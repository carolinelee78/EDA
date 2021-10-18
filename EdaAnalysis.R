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
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(naniar)
library(data.table)
library(RColorBrewer)

# importing individual psid adulthood data  

indv_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2013INDIV.csv")

# importing childhood retrospective circumstances study data 

crcs_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2014CRCS.csv")

# merging two datasets by index id 

indv_data$INB_13 <- indv_data$ER53002
crcs_data$INB_13 <- crcs_data$ER34201

dat <- merge(x = indv_data, y = crcs_data, by="INB_13")

# outcome variable (ER57482): K-6 non-specific psychological distress scale score in adulthood 

summary(dat$ER57482)

dat <- dat %>% 
  replace_with_na(replace = list(ER57482 = 99)) 

# predictor variable (CS14V285): retrospective rating of "how much affection mom gave" 

summary(dat$CS14V285)

dat <- dat %>% 
  replace_with_na(replace = list(CS14V285 = 9)) 

# predictor variable (CS14V298): retrospective rating of "how much affection dad gave" 

summary(dat$CS14V298)

# multivariate linear regression 

reg <- lm(ER57482 ~ CS14V285 + CS14V298, data = dat)

summary(reg)








