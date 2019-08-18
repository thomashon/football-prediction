fus <- read.csv("data/games0108-r.csv", sep = "\t")

pred0108 <- read.csv("data/pred0108-r.csv", sep = "\t")

library(caret)



training <- fus
summary(training)
str(training)
#training$fortlfd <- c(1:506)
warnings()



set.seed(1337)
hit_idx = createDataPartition(training$fortlfd, p = 0.8, list = FALSE)
trn = training[hit_idx,]
tst = training[-hit_idx,]

mod001 <- lm(resultNumeric~trn$hoamAvgPoints, data=trn)
summary(mod001)
plot(mod001)

num<- training[sapply(training, is.numeric)]
pairs(num)
str(num)

grid = expand.grid(mtry = c(1:18))
oob = trainControl(method="cv", number=10)

library(rpart)

mod1 <- train(
  resultNumeric~.
  , method = "rf"
  , data = trn
  , trcontrol = oob
  , tuneGrid = grid
  , importance = TRUE
  #, preProcess = c("center", "scale")
)

mod2 <- train(
  resultNumeric~.
  , method = "svmLinear"
  , data = trn
  #, trcontrol = oob
  #, tuneLength = 1
  #, preProcess = c("center", "scale")
)

summary(trn)


plot(mod2)
plot(mod2$finalModel)

mod1
plot(mod1$finalModel)
plot(mod1)
mod1$finalModel
text(mod1$finalModel)

pred_train <- predict(mod1, trn)
pred <- predict(mod1, tst)
length(pred)

alle <- predict(mod1, training)
cust_rmse <- ((sum((tst$resultNumeric-pred)^2))/length(pred))^0.5
cust_rmse

df <- data.frame(alle, training$resultNumeric)
df

summary(pred)
conf <- caret::confusionMatrix(pred, tst$resultNumeric)
conf
order(pred, decreasing = "FALSE")
pred
vergleich <- c(pred, tst$hoamResultFactor)
vergleich

summary(mod1)
summary(training)

cor(training$resultNumeric, training$hoamAvgPoints)
options(max.print = 1500)

pred2 <- predict(mod1, pred0108)
summary(pred0108)
str(fus)
pred1
pred2

library(mlbench)
importance <- varImp(mod1, scale=FALSE)
importance$importance
plot(importance)
