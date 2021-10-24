##### Setup Section ##### 

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
lapply(c("psych"), pkgTest)
lapply(c("likert"), pkgTest)
lapply(c("plyr"), pkgTest)
lapply(c("stargazer"), pkgTest)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(naniar)
library(RColorBrewer)
library(car)
library(reshape2) ## for melt()
library(psych)
library(likert)
library(plyr)
library(stargazer)

# set working directory 

setwd("/Users/carolinelee/Desktop/QTM302W_EDA")

##### Importing Data #####

# importing individual psid adulthood data  

indv_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2013INDIV.csv")

# importing childhood retrospective circumstances study data 

crcs_data <- fread("https://raw.githubusercontent.com/carolinelee78/EDA/main/Project%20Data/2014CRCS.csv")

# merging two datasets by index id 

indv_data$INB_13 <- indv_data$ER53002
crcs_data$INB_13 <- crcs_data$ER34201

dat <- merge(x = indv_data, y = crcs_data, by="INB_13")

##### Regression - Predictor Variable 1 #####

# outcome variable (ER57482): K-6 non-specific psychological distress scale score in adulthood 

dat <- dat %>% 
  replace_with_na(replace = list(ER57482 = 99)) # cleaning data

# predictor variable (CS14V285): retrospective rating of affection provided by mom

dat <- dat %>% 
  replace_with_na(replace = list(CS14V285 = c(0, 9))) # cleaning data

# predictor variable (CS14V298): retrospective rating of affection provided by dad

dat <- dat %>% 
  replace_with_na(replace = list(CS14V298 = c(0, 9))) # cleaning data 

# multivariate linear regression 1

reg1 <- lm(ER57482 ~ CS14V285 + CS14V298 + CS14V285*CS14V298, data = dat) 

summary(reg1) # viewing regression output 

stargazer(reg1, type="html", out="reg1.txt")

# residuals vs. fitted plot to check the linearity assumption of regression 

plot(reg1, which=1) 

# Q-Q plot of the studentized residuals to check the normality assumption 

qqPlot(reg1)

# scale-location plot to check the constant variance assumption of regression 

plot(reg1, which=3) 

# checking for influential plots by creating a bubble plot with hat values and studentized residuals

influencePlot(reg1, col=rainbow(10), pch=19)

# running the outlier test 

outlierTest(reg1)

outliertest_1 <- capture.output(outlierTest(reg1))

cat(outliertest_1, file="outliertest1.txt", sep="\n")

d1 <- data.frame(x=dat$ER57482,
                Mom=dat$CS14V285,
                Dad=dat$CS14V298)

dm1 <- melt(d1,id.var=1)

# likert summary statistics figures 

col1 <- d1[c("Mom","Dad")]

df1 <- as.data.frame(col1)

sink(file = "tbl1.txt")
describe(cbind(df1$Mom, df1$Dad))
sink(file = NULL)

t.test(df1$Mom, df1$Dad, alternative = "two.sided", var.equal = FALSE)

df1$Mom = factor(df1$Mom, levels = c("1", "2", "3", "4"), ordered = TRUE)

df1$Dad = factor(df1$Dad, levels = c("1", "2", "3", "4"), ordered = TRUE)

num_result1 <- likert(df1)

sumplot1a <- plot(num_result1, type = "density", facet = TRUE, bw = 0.5)

df1$Mom <- mapvalues(df1$Mom, from = c("1", "2", "3", "4"), to = c("A lot", "Some", "A little", "None at all"))

levels(df1$Mom)

df1$Dad <- mapvalues(df1$Dad, from = c("1", "2", "3", "4"), to = c("A lot", "Some", "A little", "None at all"))

levels(df1$Dad)

text_result1 <- likert(df1)

sumplot1b <- plot(text_result1, type="bar", group.order = names(df1)) + 
  labs(title = "Parental Affection Provided in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank())

ggarrange(sumplot1b, sumplot1a, ncol = 2, nrow = 1) # 1500 x 500

# psychological distress in adulthood by low vs. high maternal affection in childhood 

levdf1_mom <- d1[c("Mom")]

levdf1_mom$MomAffection <- dplyr::recode(levdf1_mom$Mom, "1" = "High", "2" = "High", "3" = "Low", "4" = "Low")

levdf1_mom$PsyDist <- d1$x

levdf1_mom <- na.omit(levdf1_mom)

histogram_1a <- ggplot(levdf1_mom, aes(x=PsyDist, color=MomAffection)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count", title = "Psychological Distress in Adulthood by Low vs. High Maternal Affection in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank()) +
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf1_mom$PsyDist)

# psychological distress in adulthood by low vs. high paternal affection in childhood 

levdf1_dad <- d1[c("Dad")]

levdf1_dad$DadAffection <- dplyr::recode(levdf1_dad$Dad, "1" = "High", "2" = "High", "3" = "Low", "4" = "Low")

levdf1_dad$PsyDist <- d1$x

levdf1_dad <- na.omit(levdf1_dad)

histogram_1b <- ggplot(levdf1_dad, aes(x=PsyDist, color=DadAffection)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count",  title = "Psychological Distress in Adulthood by Low vs. High Paternal Affection in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank())+
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf1_dad$PsyDist)

# psychological distress in adulthood by low vs. high parental affection in childhood 
# export dimensions: width 1500 x height 500

ggarrange(histogram_1a, histogram_1b, ncol = 2, nrow = 1, labels=c("A", "B"))

##### Regression - Predictor Variable 2 #####

# outcome variable (ER57482): K-6 non-specific psychological distress scale score in adulthood 

dat <- dat %>% 
  replace_with_na(replace = list(ER57482 = 99)) # cleaning data

# predictor variable (CS14V284): retrospective rating of maternal strictness 

dat <- dat %>% 
  replace_with_na(replace = list(CS14V284 = c(0, 9))) # cleaning data 

# predictor variable (CS14V297): retrospective rating of paternal strictness 

dat <- dat %>% 
  replace_with_na(replace = list(CS14V297 = c(0, 9))) # cleaning data 

# multivariate linear regression 2 

reg2 <- lm(ER57482 ~ CS14V284 + CS14V297 + CS14V284*CS14V297, data = dat) 

summary(reg2) # viewing regression output 

stargazer(reg2, type="html", out="reg2.txt")

# residuals vs. fitted plot to check the linearity assumption of regression 

plot(reg2, which=1) 

# scale-location plot to check the constant variance assumption of regression 

plot(reg2, which=3) 

# Q-Q plot of the studentized residuals to check the normality assumption 

qqPlot(reg2)

# running the outlier test 

outlierTest(reg2)

outliertest_2 <- capture.output(outlierTest(reg2))

cat(outliertest_2, file="outliertest2.txt", sep="\n")

# checking for influential plots by creating a bubble plot with hat values and studentized residuals

influencePlot(reg2, col=rainbow(10), pch=19)

d2 <- data.frame(x=dat$ER57482,
                Mom=dat$CS14V284,
                Dad=dat$CS14V297)

dm2  <- melt(d2,id.var=1)

# likert summary statistics figures 

col2 <- d2[c("Mom","Dad")]

df2 <- as.data.frame(col2)

sink(file = "tbl2.txt")
describe(cbind(df2$Mom, df2$Dad))
sink(file = NULL)

t.test(df2$Mom, df2$Dad, alternative = "two.sided", var.equal = FALSE)

df2$Mom = factor(df2$Mom, levels = c("1", "2", "3", "4"), ordered = TRUE)

df2$Dad = factor(df2$Dad, levels = c("1", "2", "3", "4"), ordered = TRUE)

num_result2 <- likert(df2)

sumplot2a <- plot(num_result2, type = "density", facet = TRUE, bw = 0.5)

df2$Mom <- mapvalues(df2$Mom, from = c("1", "2", "3", "4"), to = c("Very", "Somewhat", "Not very", "Not at all"))

levels(df2$Mom)

df2$Dad <- mapvalues(df2$Dad, from = c("1", "2", "3", "4"), to = c("Very", "Somewhat", "Not very", "Not at all"))

levels(df2$Dad)

text_result2 <- likert(df2)

sumplot2b <- plot(text_result2, type="bar", group.order = names(df2)) + 
  labs(title = "How Strict Was Parent in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank())

ggarrange(sumplot2b, sumplot2a, ncol = 2, nrow = 1) # 1500 x 500

# psychological distress in adulthood by low vs. high maternal strictness in childhood 

levdf2_mom <- d2[c("Mom")]

levdf2_mom$MomStrictness <- dplyr::recode(levdf2_mom$Mom, "1" = "High", "2" = "High", "3" = "Low", "4" = "Low")

levdf2_mom$PsyDist <- d2$x

levdf2_mom <- na.omit(levdf2_mom)

histogram_2a <- ggplot(levdf2_mom, aes(x=PsyDist, color=MomStrictness)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count", title = "Psychological Distress in Adulthood by Low vs. High Strictness of Mom in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank()) +
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf2_mom$PsyDist)

# psychological distress in adulthood by low vs. high paternal strictness in childhood 

levdf2_dad <- d2[c("Dad")]

levdf2_dad$DadStrictness <- dplyr::recode(levdf2_dad$Dad, "1" = "High", "2" = "High", "3" = "Low", "4" = "Low")

levdf2_dad$PsyDist <- d2$x

levdf2_dad <- na.omit(levdf2_dad)

histogram_2b <- ggplot(levdf2_dad, aes(x=PsyDist, color=DadStrictness)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count",  title = "Psychological Distress in Adulthood by Low vs. High Strictness of Dad in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank())+
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf2_dad$PsyDist)

# psychological distress in adulthood by low vs. high parental strictness in childhood 
# export dimensions: width 1500 x height 500

ggarrange(histogram_2a, histogram_2b, ncol = 2, nrow = 1)

##### Regression - Predictor Variable 3 #####

# outcome variable (ER57482): K-6 non-specific psychological distress scale score in adulthood 

dat <- dat %>% 
  replace_with_na(replace = list(ER57482 = 99)) # cleaning data

# predictor variable (CS14V282): relationship status with mom

dat <- dat %>% 
  replace_with_na(replace = list(CS14V282 = c(0, 9))) # cleaning data 

# predictor variable (CS14V295): relationship status with dad

dat <- dat %>% 
  replace_with_na(replace = list(CS14V295 = c(0, 9))) # cleaning data 

# multivariate linear regression 3

reg3 <- lm(ER57482 ~ CS14V282 + CS14V295 + CS14V282*CS14V295, data = dat)

summary(reg3) # viewing regression output

stargazer(reg3, type="html", out="reg3.txt")

# residuals vs. fitted plot to check the linearity assumption of regression 

plot(reg3, which=1) 

# scale-location plot to check the constant variance assumption of regression 

plot(reg3, which=3) 

# Q-Q plot of the studentized residuals to check the normality assumption 

qqPlot(reg3)

# checking for outliers 

outlierTest(reg3)

outliertest_3 <- capture.output(outlierTest(reg3))

cat(outliertest_3, file="outliertest3.txt", sep="\n")

# checking for influential plots by creating a bubble plot with hat values and studentized residuals

influencePlot(reg3, col=rainbow(10), pch=19)

# creating plot 

d3 <- data.frame(x=dat$ER57482,
                 Mom=dat$CS14V282,
                 Dad=dat$CS14V295)

dm3  <- melt(d3,id.var=1)

# likert summary statistics figures 

col3 <- d3[c("Mom","Dad")]

df3 <- as.data.frame(col3)

sink(file = "tbl3.txt")
describe(cbind(df3$Mom, df3$Dad))
sink(file = NULL)

t.test(df3$Mom, df3$Dad, alternative = "two.sided", var.equal = FALSE)

df3$Mom = factor(df3$Mom, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)

df3$Dad = factor(df3$Dad, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)

num_result3 <- likert(df3)

sumplot3a <- plot(num_result3, type = "density", facet = TRUE, bw = 0.5)

df3$Mom <- mapvalues(df3$Mom, from = c("1", "2", "3", "4", "5"), to = c("Excellent", "Very Good", "Good", "Fair", "Poor"))

levels(df3$Mom)

df3$Dad <- mapvalues(df3$Dad, from = c("1", "2", "3", "4", "5"), to = c("Excellent", "Very Good", "Good", "Fair", "Poor"))

levels(df3$Dad)

text_result3 <- likert(df3)

sumplot3b <- plot(text_result3, type="bar", neutral.color = "#f7eed5", group.order = names(df3)) + 
  labs(title = "Relationship with Parent in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank()) 

ggarrange(sumplot3b, sumplot3a, ncol = 2, nrow = 1) # 1500 x 500

# psychological distress in adulthood by good vs. poor relationship quality with mom in childhood 

levdf3_mom <- d3[c("Mom")]

levdf3_mom$MomRelQual <- dplyr::recode(levdf3_mom$Mom, "1" = "High", "2" = "High", "3" = "High", "4" = "Low", "5" = "Low")

levdf3_mom$PsyDist <- d3$x

levdf3_mom <- na.omit(levdf3_mom)

histogram_3a <- ggplot(levdf3_mom, aes(x=PsyDist, color=MomRelQual)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count", title = "Psychological Distress in Adulthood by Low vs. High Quality of Relationship with Mom in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank()) +
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf3_mom$PsyDist)

# psychological distress in adulthood by low vs. high relationship quality with dad in childhood 

levdf3_dad <- d3[c("Dad")]

levdf3_dad$DadRelQual <- dplyr::recode(levdf3_dad$Dad, "1" = "High", "2" = "High", "3" = "High", "4" = "Low", "5" = "Low")

levdf3_dad$PsyDist <- d3$x

levdf3_dad <- na.omit(levdf3_dad)

histogram_3b <- ggplot(levdf3_dad, aes(x=PsyDist, color=DadRelQual)) +
  geom_histogram(fill="white", alpha=0.7, position="identity", bins = 24) + 
  labs(x = "Psychological Distress in Adulthood", y = "Count",  title = "Psychological Distress in Adulthood by Low vs. High Quality of Relationship with Dad in Childhood") +
  theme(plot.title = element_text(size=10), legend.title = element_blank())+
  scale_color_manual(values=c("darkgoldenrod2", "aquamarine3"))

describe(levdf3_dad$PsyDist)

# psychological distress in adulthood by low vs. high relationship quality with parent in childhood 
# export dimensions: width 1500 x height 500

ggarrange(histogram_3a, histogram_3b, ncol = 2, nrow = 1)










