setwd("/Users/nevi.r.shah@ey.com/Desktop")
data = read.csv("KAG__data.csv", header = TRUE)
library("ggplot2")
library("caret")
library("ROCR")
library("dplyr")


#Q1A
head(data)
#get costs per click and add column 
data$cpc = data$Spent/data$Clicks
newdata <- filter(data, cpc == "NaN")
newdata[order(-newdata1$Impression),]
#ad_id = 1121094

#Q1B
1000*(sum(data[which(data$xyz_campaign_id == 936),9])/sum(data[which(data$xyz_campaign_id == 936),7]))
1000*(sum(data[which(data$xyz_campaign_id == 1178),9])/sum(data[which(data$xyz_campaign_id == 1178),7]))
1000*(sum(data[which(data$xyz_campaign_id == 916),9])/sum(data[which(data$xyz_campaign_id == 916),7]))
#campaign_id = 936 spent the least efficiently  

#Q2
#calculate ROAA column
data$roas <- round(((10*data$Total_Conversion + 50*data$Approved_Conversion) / (data$Spent)), 2)
newdata <- filter(data, Spent != 0)
newdata

newdata3 <- filter(newdata, interest == 15 | interest == 21 | interest == 101)
as.data.frame(newdata3)
newdata3$interest <- as.factor(newdata3$interest)
ggplot(newdata3,aes(x=interest, y=roas, fill = gender)) + geom_boxplot() + scale_y_log10()

newdata4 <- filter(newdata, (interest == 15 | interest == 21 | interest == 101) & xyz_campaign_id == 1178 & gender == "M")
newdata5 <- filter(newdata, (interest == 15 | interest == 21 | interest == 101) & xyz_campaign_id == 1178 & gender == "F")
meanM <- mean(newdata4$roas)
meanF <- mean(newdata5$roas)
medianM <- median(newdata4$roas)
medianF <- median(newdata5$roas)
df <- data.frame("Category" = c("meanMale", "meanFemale", "medianMale", "medianFemale"), "Value" = c(meanM, meanF, medianM, medianF))
df


#Q3
data2 = read.csv("advertising1.csv", header = TRUE, stringsAsFactors = FALSE)
data2$Clicked.on.Ad <- as.factor(data2$Clicked.on.Ad)
head(data2)

ggplot(data2, aes(x=Daily.Internet.Usage, y=Age, shape=Clicked.on.Ad)) + geom_point() +
  scale_shape_manual(values=c(0,1)) 
#they would click on the ad 

#Q4
logistic <- glm(Clicked.on.Ad ~ Daily.Time.Spent.on.Site + Age + Area.Income, data = data2, family = "binomial")
data2$probs <- predict(logistic, newdata=data2, type="response")

#create threshold 
data2$pred <- ifelse((data2$probs >= .8), 1, 0)

#change pred response as factor 
data2$pred <- as.factor(data2$pred)

#create confusion matrix
confusionMatrix(data2$pred, data2$Clicked.on.Ad)

#Q5
#false negatives are 87

# head(data2)


rocData <- roc(as.numeric(data2$Clicked.on.Ad),as.numeric(data2$pred))
plot(rocData)

#use Wq to find average time a customer spends in the queue
#Q6a
Wq1 <- (60*(62/(70*(70-62))))
Wq1
#customers will wait for 7 minutes on average in line 

#Q6b
Wq2 <- (60*(62/(84*(84-62))))
Wq2
#customers will now wait for 2 minutes on average in line 


for (i in 7:20) {
  print(60*(82/((14*i)*((14*i)-82))))
}


df <- data.frame("Servers" = c(7,8,9,10,11,12,13,14,15, 16, 17, 18, 19, 20), 
                 "Time" = c(3.137755, 1.464286, 0.8874459, 0.6059113, 
                            0.4437229, 0.3405316, 0.2703297, 0.2201933, 0.1830357, 0.1546781, 
                            0.1325145, 0.1148459, 0.100523, 0.08874459))

#Q7/Q8a
for (i in 7:20) {
  print(60*(82/((14*i)*((14*i)-82))))
}
plot(df$Servers, df$Time)
#at 8 servers it drops below 3 minutes 

#Q8b
#As the number of servers increases the wait time decreases exponentially. After about 14 servers it starts flat lining. if we go on to add more servers to infinity we will get a number that gets closer and closer to 0 but will never reach 0 because you cannot have a 0 in the denominator of the equation. 


