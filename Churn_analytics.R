
#Read the data after cleaning headers
mainData <- read.csv('Dataset.txt')

##Sampling: Stratified sampling
mainData0 <- mainData[mainData$Churn == 0,]
mainData1 <- mainData[mainData$Churn == 1,]
samp0 <- mainData0[sample(nrow(mainData0), 323), ]

sampData <- rbind(samp0,mainData1)

#---clustering
#k means clustering for Age, CHI 0 months, CHI 1 month, Login 1 month, Views and Days since last login
#---

#Age
k.max <- 15
data <- sampData$Cust_Age
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$Cust_Age_Cat <-as.factor( kmeans(data, 4, nstart=50,iter.max = 15 )$cluster)

#CHI 0 months
k.max <- 15
data <- sampData$CHI_0
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$CHI_0_Cat <-as.factor( kmeans(data, 3, nstart=50,iter.max = 15 )$cluster)

#CHI 1 month
k.max <- 15
data <- sampData$CHI_1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$CHI_1_Cat <-as.factor( kmeans(data, 4, nstart=50,iter.max = 15 )$cluster)

#Login 1 month
k.max <- 15
data <- sampData$Login_1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$Login_1_Cat <-as.factor( kmeans(data, 3, nstart=50,iter.max = 15 )$cluster)

#Views 1 month
k.max <- 15
data <- sampData$Views_1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$Views_1_Cat <-as.factor( kmeans(data, 3, nstart=50,iter.max = 15 )$cluster)

#Days since last login 1 month
k.max <- 15
data <- sampData$Days_Since_1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

#determine optimum number of clusters using the elbow curve
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


sampData$Days_Since_1_Cat <-as.factor( kmeans(data, 3, nstart=50,iter.max = 15 )$cluster)

#splitting into test and train datasets
set.seed(88)
#install.packages('caTools')
require(caTools)
split <- sample.split(sampData$Churn, SplitRatio = 0.75)
train <- subset(sampData, split == TRUE)
test <- subset(sampData, split == FALSE)

#Run logistic regression

model <- glm (Churn ~ Cust_Age_Cat + CHI_0_Cat + CHI_1_Cat+ Login_1_Cat +
                Views_1_Cat + Days_Since_1_Cat , data = train, family = binomial)

summary(model)

predict <- predict(model, type = 'response')

#confusion matrix: get accuracy here
table(train$Churn, predict>0.5)

#66% accuracy achieved on our train-test set

#Random Forest: Accuracy is ~66%, which is what we get in Logistic Regression
install.packages('randomForest')
require(randomForest)

train$Churn <- as.factor(train$Churn)

#Get importance of variables along with the model
model_rf  <- randomForest(Churn ~ Cust_Age_Cat + CHI_0_Cat + CHI_1_Cat + Sup_Case_0 + Sup_Case_1+ SP_1+SP_0 + Login_1 + Blog_1 +
                         Views_1 + Days_Since_1, data = train , ntree = 500, importance = TRUE)

predict_rf <- predict(model2, type = 'response')

#Feature importance
model_rf$importance

write.csv(model_rf$importance, file = "FI.csv")

#Final output with probability for each customer id

output <- glm (Churn ~ Cust_Age_Cat + CHI_0_Cat + CHI_1_Cat+ Login_1_Cat +
                Views_1_Cat + Days_Since_1_Cat , data = mainData, family = binomial)

summary(output)

predict <- predict(output, type = 'response')

