setwd("/Users/nevi.r.shah@ey.com/desktop")
library(datasets)
data("PlantGrowth")
head(PlantGrowth)
library(tidyverse) 
library("knitr")
library("rmarkdown")

#create two separate data sets 
t1control <- PlantGrowth[c(1:20),]
t2control <- PlantGrowth[c((1:10),(21:30)),]

#QUESTION  1

#PART 1A
lm1 <- lm(weight~ group, data = t1control)
lm2 <- lm(weight~ group, data = t2control)

#double check
#formula of difference estimator is yBar1 - yBar0. So we will do mean(treatment) - mean(control)

#treatment1
controlAverage <- mean(t1control[c(1:10),1])
treatment1Average <- mean(t1control[c(11:20),1])
diffEst1 <- treatment1Average - controlAverage
diffEst1

#treatment2
controlAverage <- mean(t2control[c(1:10),1])
treatment2Average <- mean(t2control[c(11:20),1])
diffEst2 <- treatment2Average - controlAverage
diffEst2

#PART 1B
controlAverage #5.032
treatment1Average #4.661 
treatment2Average #5.526
#according to the averages, treatment 2 has the highest average weight 

#PART 1C
#i'm from new jersey go new jersey! 
minwage <- read.csv("Min_Wage.csv", header = TRUE)

#separate data into groups 
B <- minwage[which(minwage$State == "New Jersey" & minwage$d == 0),]
D <- minwage[which(minwage$State == "New Jersey" & minwage$d == 1),]
A <- minwage[which(minwage$State == "Philadelphia" & minwage$d == 0),]
C <- minwage[which(minwage$State == "Philadelphia" & minwage$d == 1),]

#PART 1D (i)
avgB <- mean(B$fte)
avgB

avgD <- mean(D$fte)
avgD

avgA <- mean(A$fte)
avgA

avgC <- mean(C$fte)
avgC

#PART 1D (ii)
DID <- (avgD-avgB) - (avgC-avgA)
DID #2.75

#PART 2E
#use regression to estimate for DID
minwage <- minwage %>%
  mutate(Treated = ifelse(minwage$State=="New Jersey",1,0) )

#create interaction 
minwage$did = minwage$d * minwage$Treated
minwage

#create linear model 
linmod = lm(fte ~ Treated + d + did, data = minwage)
linmod
#A = 23.380
#B = 23.380 - 2.949 = 20.431
#C = 23.380 -2.283 = 21.097
#D = 23.380 -2.949 - 2.283 + 2.750 = 20.898
#difference of two differences is 2.750 which is the same DID we got using the means. This means you can use either approach to solve for DID 

#QUESTION 2
berkshire <- read.csv("Berkshire.csv", header = TRUE)
library("PerformanceAnalytics")
library("lubridate")
library("xts")

#PART 2A
sd <- sd(berkshire$BrkRet)
sd #6.75%

#PART 2B
avg <- mean(berkshire$BrkRet)
avg #1.90%

#PART 2C
outperformed = mean(berkshire$BrkRet) - mean(berkshire$MKT)
outperformed #.88%

#part 2D
#create an xts dataset for berk returns
berkshire$Date <- mdy(berkshire$Date)
berkshire2 <- berkshire[order(berkshire$Date),]
xtsBerk <- xts(berkshire2[,-1], order.by=berkshire2[,1],)

#find cumulative return
returnInc <- Return.cumulative(xtsBerk$BrkRet, geometric = TRUE)
return <- returnInc*10000


#PART 2E
#plot cumulative returns 
chart.CumReturns(xtsBerk, main = "Cumulative Returns",wealth.index = FALSE, geometric = TRUE,legend.loc = "left")


#PART 2F
#calculate sharpe ratio 
SharpeRatio(xtsBerk$BrkRet,xtsBerk$RF)
SharpeRatio(xtsBerk$MKT,xtsBerk$RF)
#the sharpe ratio is larger for for the berk return

#PART 2G
#finding beta. Beta is MKR intercept 
mod =lm(BrkRet ~ MKT, data= xtsBerk)
summary(mod)
mod
#Beta is .69852

#PART 2H
#get alpha value 
xtsBerk1 <-transform(xtsBerk, MktExcess=MKT - RF, FundExcess=BrkRet-RF)
Alpha=lm(FundExcess~MktExcess,data=xtsBerk1) 
summary(Alpha)
#1.08%



