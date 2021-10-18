# There are three ways to collapse/expand sections: 
# (1) Go to the 'Edit' tab, and select 'Folding' to collapse/expand certain sections or collapse/expand all sections 
# (2) Shortcuts: collapse (option + command + L); expand (option + shift + command + L); collapse all (option + command + O); expand all (option + shift + command + O)
# (3) Click on the small upside down triangles to the left of each section marker to collapse/expand each section 

######################## Setup Section ######################## 

# This section downloads the necessary R packages and functions we need to run the analysis and create visuals. 

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

ChildData <- fread("https://raw.githubusercontent.com/carolinelee78/QTM-302W/main/Project%20Data/J298010.csv")

ChildData$PSID_ID <- (ChildData$ER30001 * 1000) + ChildData$ER30002

nrow(ChildData)

ChildData <- ChildData %>% 
  replace_with_na(replace = list(AGEATCH = c(0, 999.9)))

ChildData$Age_1997 <- ChildData$AGEATCH / 12

ChildData <- subset(ChildData, !is.na(Age_1997))

head(ChildData)

summary(ChildData$Age_1997)

HouseholdData <- fread("https://raw.githubusercontent.com/carolinelee78/QTM-302W/main/Project%20Data/J298010PCG.csv")

HouseholdData$PSID_ID <- (HouseholdData$ER30001 * 1000) + HouseholdData$ER30002

head(HouseholdData, 10)

nrow(HouseholdData)

HouseholdData_BMO <- filter(HouseholdData, PCG97 == 1.0) # primary caregiver is biological mother 
head(HouseholdData_BMO)

nrow(HouseholdData_BMO)

PCG_Child_Map <- fread("https://raw.githubusercontent.com/carolinelee78/QTM-302W/main/Project%20Data/M298010.csv")

PCG_Child_Map <- select(PCG_Child_Map, "ER30001", "ER30002", "PCGID_97", "PCGPN_97")

PCG_Child_Map$Child_PSID_ID <- (PCG_Child_Map$ER30001 * 1000) + PCG_Child_Map$ER30002

PCG_Child_Map$Parent_PSID_ID <- (PCG_Child_Map$PCGID_97 * 1000) + PCG_Child_Map$PCGPN_97

head(PCG_Child_Map)

nrow(PCG_Child_Map)

# Being parent is hard (Q2A29A): "Thinking about your child(ren), please indicate on a scale from 
# 1 (not at all true) to 5 (completely true) the number that best describes how true each statement is. 
# ...Being a parent is harder than I thought it would be" 
# Values: 1-5 (Not at all true - Completely true); 8 (DK); 9 (NA/refused)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(Q2A29A = 9))

table(HouseholdData$Q2A29A)

parent_diff <- HouseholdData %>% 
  filter(Q2A29A %in% c(1,2,3,4,5))

summary(parent_diff$Q2A29A)
sd(parent_diff$Q2A29A)
length(which(!is.na(parent_diff$Q2A29A)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(parent_diff$Q2A29A)


HouseholdData$Q2A29A <- as.factor(HouseholdData$Q2A29A)

# colored barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29A)), aes(x=Q2A29A)) + 
  geom_bar(stat="count", fill = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) + 
  labs(title = "CDS 1997", x = "Being a parent is harder than expected", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

# b&g barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29A)), aes(x=Q2A29A)) + 
  geom_bar(stat="count") + 
  labs(title = "Being a parent is hard", x = "Being a parent is harder than expected", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

# Feel trapped as parent (Q2A29B): "Thinking about your child(ren), please indicate on a scale from 
# 1 (not at all true) to 5 (completely true) the number that best describes how true each statement is. 
# ... I feel trapped by my responsibilities as a parent" 
# Values: 1-5 (Not at all true - Completely true); 8 (DK); 9 (NA/refused)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(Q2A29B = 9))

table(HouseholdData$Q2A29B)

HouseholdData$Q2A29B <- as.factor(HouseholdData$Q2A29B)

# colored barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29B)), aes(x=Q2A29B)) + 
  geom_bar(stat="count", fill = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) + 
  labs(title = "CDS 1997", x = "Feel trapped as a parent", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

# b&g barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29B)), aes(x=Q2A29B)) + 
  geom_bar(stat="count") + 
  labs(title = "Feel trapped by responsibilities as a parent", x = "Feel trapped as a parent", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

parent_trap <- HouseholdData %>% 
  filter(Q2A29B %in% c(1,2,3,4,5))

summary(parent_trap$Q2A29B)
sd(parent_trap$Q2A29B)
length(which(!is.na(parent_trap$Q2A29B)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(parent_trap$Q2A29B)

# Children are work (Q2A29C): "Thinking about your child(ren), please indicate on a scale from 
# 1 (not at all true) to 5 (completely true) the number that best describes how true each statement is. 
# ... I find that taking care of my child(ren) is much more work than pleasure" 
# Values: 1-5 (Not at all true - Completely true); 8 (DK); 9 (NA/refused)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(Q2A29C = 9))

table(HouseholdData$Q2A29C)

HouseholdData$Q2A29C <- as.factor(HouseholdData$Q2A29C)

# colored barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29C)), aes(x=Q2A29C)) + 
  geom_bar(stat="count", fill = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) + 
  labs(title = "CDS 1997", x = "Children are more work than pleasure", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

# b&g barplot 

ggplot(data=subset(HouseholdData, !is.na(Q2A29C)), aes(x=Q2A29C)) + 
  geom_bar(stat="count") + 
  labs(title = "Children are more work than pleasure", x = "Children are work", y = "Count") +
  scale_x_discrete(breaks = 1:5, labels=c("Not at all true","Somewhat true", "Moderately true", "Very true", "Completely true")) 

parent_cwork <- HouseholdData %>% 
  filter(Q2A29C %in% c(1,2,3,4,5))

summary(parent_cwork$Q2A29C)
sd(parent_cwork$Q2A29C)
length(which(!is.na(parent_cwork$Q2A29C)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(parent_cwork$Q2A29C)

table(HouseholdData$Q2A37A)

decis_man <- HouseholdData %>% 
  filter(Q2A37A %in% c(1,2,3,4))

summary(decis_man$Q2A37A)
sd(decis_man$Q2A37A)
length(which(!is.na(decis_man$Q2A37A)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(decis_man$Q2A37A)

table(HouseholdData$Q2A37X)

dad_invlv <- HouseholdData %>% 
  filter(Q2A37X %in% c(1,2,3,4))

summary(dad_invlv$Q2A37X)
sd(dad_invlv$Q2A37X)
length(which(!is.na(dad_invlv$Q2A37X)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(dad_invlv$Q2A37X)

table(HouseholdData$Q2A37W)

man_feel <- HouseholdData %>% 
  filter(Q2A37W %in% c(1,2,3,4))

summary(man_feel$Q2A37W)
sd(man_feel$Q2A37W)
length(which(!is.na(man_feel$Q2A37W)))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(man_feel$Q2A37W)

## Outcome variables 

# MIDUS Subscale - Psychological (M9-M14): Total Average for Frequency of (1) Feeling Good at Managing Daily Responsibility; (2) Feeling Has Trusting Relationships with Others; 
# (3) Feeling Challenged to Grow; (4) Feeling Confident of Own Ideas; (5) Feeling Liked Own Personality; (6) Feeling Life Had Direction 
# Possible Score: 1-6 (Actual value); 9 (At least one component is DK/NA/refused)

table(HouseholdData$TA050937)

ggplot(data=subset(HouseholdData, !is.na(TA050937)), aes(x=TA050937)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  labs(title = "Psychological Well-being (TIAS 2005)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA070918)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA070918 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA070918)), aes(x=TA070918)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  labs(title = "Psychological Well-being (TIAS 2007)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA090982)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA090982 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA090982)), aes(x=TA090982)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  scale_x_continuous(breaks = seq(1, 6, by = 1)) + 
  labs(title = "Psychological Well-being (TIAS 2009)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA111124)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA111124 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA111124)), aes(x=TA111124)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  scale_x_continuous(breaks = seq(1, 6, by = 1)) + 
  labs(title = "Psychological Well-being (TIAS 2011)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA131216)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA131216 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA131216)), aes(x=TA131216)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  scale_x_continuous(breaks = seq(1, 6, by = 1)) + 
  labs(title = "Psychological Well-being (TIAS 2013)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA151276)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA151276 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA151276)), aes(x=TA151276)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  labs(title = "Psychological Well-being (TIAS 2015)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

table(HouseholdData$TA171974)

HouseholdData <- HouseholdData %>% 
  replace_with_na(replace = list(TA171974 = 9))

ggplot(data=subset(HouseholdData, !is.na(TA171974)), aes(x=TA171974)) + 
  geom_histogram(binwidth=1, color="black", fill="white") + 
  scale_x_continuous(breaks = seq(1, 6, by = 1)) + 
  labs(title = "Psychological Well-being (TIAS 2017)", x = "Score on the Psychological Well-being MIDUS Subscale", y = "Count") 

### analysis 


