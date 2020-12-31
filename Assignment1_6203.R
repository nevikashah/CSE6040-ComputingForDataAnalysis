setwd("/Users/nevi.r.shah@ey.com/desktop")

#QUESTION 1
dat1 <- read.csv("airbnb_data.csv", header = TRUE)

#part a
linmodel <- lm(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms, data = dat1)
summary(linmodel)

#part b
#The price for a shared room decreases by $76.668 from the base case (entire house/apt) at a certain point x

#part c
newdata <- data.frame(bedrooms=1, accommodates=2, reviews=70, overall_satisfaction=4, room_type="Private room")
predict(linmodel, newdata, interval = "confidence")
#value is $66

#part d
par(mfrow=c(2,2))
plot(linmodel)

#get cook distance values
cooks <- cooks.distance(linmodel)

#loop through cooks list and find which are greater than 1. Row 94 and 95 are greater than 1
for (i in 1:length(cooks)) {if (cooks[i] > 1) {print(cooks[i])}}

#Remove row 94 and 95 
dataa <- data.frame(dat1)
dat2 <- dataa[-c(94,95),]

#rerun linear mod on new data set without those two points
linmodel2 <- lm(price ~ room_type + reviews + overall_satisfaction + accommodates + bedrooms, data = dat2)

#look at residual vs leverage graph to see if there are any other points to remove - there are not
par(mfrow=c(2,2))
plot(linmodel2)

#print summary 
summary(linmodel2)
#R^2 improved and all predictors are significant 


#QUESTION 2
library(tidyverse) 

#read in data
dm <- read.csv("direct_marketing.csv", header = TRUE)

#mutate to create code vals and dummy variables
dm <- dm %>%
  mutate(Low = ifelse(dm$History=="Low",1,0) ) %>%
  mutate(Medium =ifelse(dm$History=="Medium",1,0)) %>%
  mutate(High = ifelse(dm$History=="High",1,0))

#create interaction variables 
dm <- dm %>%
  mutate(LowSalary = (dm$Salary*Low)) %>%
  mutate(MediumSalary = (dm$Salary*Medium)) %>%
  mutate(HighSalary = (dm$Salary*High))

#create linear model with response variable as amount spent
model2 <- lm(AmountSpent ~ Salary+ Low + Medium + High + LowSalary + MediumSalary + HighSalary, data = dm)

#create data frame with given values for low, medium, high, and none
low <- data_frame(Salary=10000, Low = 1, Medium=0, High=0, LowSalary=10000, MediumSalary = 0, HighSalary=0)
medium <-data_frame(Salary=10000, Low = 0, Medium=1, High=0, LowSalary=0, MediumSalary = 10000, HighSalary=0)
high <- data_frame(Salary=10000, Low = 0, Medium=0, High=1, LowSalary=0, MediumSalary = 0, HighSalary=10000)
none <- data_frame(Salary=10000, Low = 0, Medium=0, High=0, LowSalary=0, MediumSalary = 0, HighSalary=0)

#use linear model to predict amount spent given history 
predict(model2, low)
predict(model2,  medium)
predict(model2, high)
predict(model2, none)

#QUESTION 3

#read in data
datbnb <- read.csv("airbnb_data.csv", header = TRUE)

#create the different models use .01 as instructed in the TA session notes 
linlin <- lm(price~ overall_satisfaction,data = datbnb)
loglin <- lm(log(price) ~ overall_satisfaction, data=datbnb)
linlog <- lm(price ~ log(overall_satisfaction+.01), data = datbnb)
loglog <- lm(log(price) ~ log(overall_satisfaction+.01), data = datbnb) 

#Look at summary of models to see R^2 
summary(linlin)
summary(loglin)
summary(linlog)
summary(loglog)

#LinLog has the highest R^2 at .02126
#howeverall R^2 values for each of the four models seem quite low. I blieve this model would work better in conjunction with other predictors instead of by itself 


#QUESTION 3

#read in data
titanic <-read.csv("titanic_data.csv")

#change columns to factors and age to an integer 
titanic$Name <- as.factor(titanic$Name)
titanic$PClass <- as.factor(titanic$PClass)
titanic$Age <- as.integer(titanic$Age)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Survived <- as.factor(titanic$Survived)

#make logistic model
logistic <- glm(Survived ~ Sex, data = titanic, family = "binomial")

#see summary of model
summary(logistic)
#intercept coefficient is the log odds that a female will survive 

#create data frame with given values 
fem = data.frame(Sex = "female", Survived = 1)
male = data.frame(Sex = "male", Survived = 1)



#look at predicted value for male and female 
predict(logistic, fem, type = "response")
#according to the model a probability of 75.34% of women survived on the Titanic

predict(logistic, male, type = "response")
#according to the model, a probability of 20.512% of men survived on the Titanic



