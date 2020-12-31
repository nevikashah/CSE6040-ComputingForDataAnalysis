data("longley")
View(head(longley))

lm <- lm(Employed ~ GNP.deflator + Unemployed + GNP + 
           Unemployed + Armed.Forces + Population, data = longley)
summary(lm)

cor(longley)

#1. (B) sig predictors are GNP, Armed Forces
#2. (C) used corr function to look at correlation of predictors 

data("trees")
View(head(trees))
lm1 <- lm(Volume ~ Girth, data = trees)
lm2 <- lm(Volume ~ Height, data = trees)
summary(lm1) 
summary(lm2)
#3. (C) adjusted is .0331 and .3358

linlin <- lm(Volume ~ Girth, data = trees)
loglin <- lm(log(Volume) ~ Girth, data = trees)
linlog <- lm(Volume ~ log(Girth), data = trees)
loglog <- lm(log(Volume) ~ log(Girth), data = trees)
#4. (C) lin log has the worst with .8837

loglog2 <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
summary(loglog2)


a <- (exp(.01*1.982650) - 1)*100
b <- (exp(.01*1.117123) - 1)*100

c<- exp(log(101/100)*(1.982650 + 1.117123)) 
d <- 1.982650*1.117123
exp(0.0040568)-1
1.1^1.982650
1.1^1.117123

exp(log(1.1) * 3.099773)
exp((log((100 + 50)/100)) * -0.18)
(1.01^1.982650 + 1.01^1.117123)
exp(.01*3.099773) - 1
(exp(0.01* -0.40568)-1) *100





#5. A

#6. B -



setwd("/Users/nevi.r.shah@ey.com/desktop")
advertising <- read.csv("advertising.csv", header=TRUE, stringsAsFactors=FALSE)
head(advertising,40)
advertising$User.ID <- as.factor(advertising$User.ID)
advertising$Age <- as.integer(advertising$Age)
advertising$EstimatedSalary <- as.integer(advertising$EstimatedSalary)
advertising$Purchased <- as.factor(advertising$Purchased)

mkt <- glm(Purchased ~ Age + EstimatedSalary, data = advertising, family = "binomial")
summary(mkt)$coef
exp(confint(mkt, level = .95))

#7. A

#8. D? 

mkt <- lm(Purchased ~ Gender + Age + EstimatedSalary, data = advertising)
summary(mkt)

#9. B
exp(confint(mkt))

#10.
data(sleep)
sleep1 <- sleep[c(1:10),]
sleep2 <- sleep[c(11:20),]

#average
average1 <- mean(sleep1[c(1:10),1])
average2 <- mean(sleep2[c(1:10),1])

average2 - average1
lm1 <- lm(extra~ group, data = sleep)

#10. 1.58 (C)

#11. (B) 

#12 (B) 
counties <- read.csv("us-counties.csv", header=TRUE, stringsAsFactors=FALSE)
head(counties)

NJBefore <- counties[which(counties$state == "New Jersey" & counties$date < "2020-04-11"),]
NYBefore <- counties[which(counties$state == "New York" & counties$date < "2020-04-11"),]
ILBefore <- counties[which(counties$state == "Illinois" & counties$date < "2020-04-11"),]
CaliBefore <- counties[which(counties$state == "California" & counties$date < "2020-04-11"),]
B <- rbind(NJBefore, NYBefore, ILBefore, CaliBefore)

NJAfter <- counties[which(counties$state == "New Jersey" & counties$date >= "2020-04-11"),]
NYAfter <- counties[which(counties$state == "New York" & counties$date >= "2020-04-11"),]
ILAfter <- counties[which(counties$state == "Illinois" & counties$date >= "2020-04-11"),]
CaliAfter <- counties[which(counties$state == "California" & counties$date >= "2020-04-11"),]
D <- rbind(NJAfter, NYAfter, ILAfter, CaliAfter )
s <- rbind(treatmenttotalAfter,treatmenttotalBefore)

control <- subset(counties, state != "New Jersey" & state != "New York" & state != "California" & state != "Illinois", select=c(date, county, state, fips, cases, deaths))
A <- control[which(control$date < "2020-04-11"),]
C <- control[which(control$date >= "2020-04-11"),]

aavg <- mean(A$cases)
bavg <- mean(B$cases)
cavg <- mean(C$cases)
davg <- mean(D$cases)
(davg - bavg) - (cavg - aavg)

#13. (C) 
bavg
davg
aavg
cavg

#14. (C)  
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics") 
library(PerformanceAnalytics) 
data(managers)
library("lubridate")
head(managers)
sd(managers$HAM1)
library("stringr")
names(managers)<-str_replace_all(names(managers), c(" " = "."))
#15. (C)
(mean(managers$HAM1)) - (mean(managers$SP500.TR))

managers$ExcessPerformance<- managers$HAM1-managers$SP500.TR
mean(managers$ExcessPerformance)

berk <- read.csv("berkshire.csv", header=TRUE, stringsAsFactors=FALSE)


#16. (A) 
Ham4 <- Return.cumulative(managers$HAM4, geometric = TRUE)[1]
Ham4
head(managers)
managers
summary(managers)
50000*(1+Ham4)

Ham4
(prod(managers$HAM1+1)-1)*50000


#17. (C)
SharpeRatio(managers$HAM3, managers$US.3m.TR)
SharpeRatio(managers$SP500.TR, managers$US.3m.TR)
#index - .1255
#Ham3 - .2525

#18. C
setwd("/Users/nevi.r.shah@ey.com/desktop")
factors <- read.csv("factors.csv", header=TRUE, stringsAsFactors=FALSE)
factors$Date <- mdy(factors$Date)



factoraapl <- lm(AAPL ~ Mkt.RF+SMB+HML, data = factors)
factorba <- lm(BA ~ Mkt.RF+SMB+HML, data = factors)
factorm <- lm(M ~ Mkt.RF+SMB+HML, data = factors)

#19. A
#20. C
