

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)

training <- segmentationOriginal[ segmentationOriginal$Case == "Train", ]
testing  <- segmentationOriginal[ segmentationOriginal$Case == "Test",  ]

Mod <- train(Class ~ ., method = "rpart", data = training)
print(Mod$finalModel)

plot(Mod$finalModel, uniform = TRUE, main = "Classification Tree")
text(Mod$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)

library(rattle)
library(rpart)
library(rpart.plot)
fancyRpartPlot(Mod$finalModel)

predict(Mod, )


names(segmentationOriginal)



# Question 3

library(tree)
library(pgmm)
data(olive)
olive <- olive[ , -1]
newdata <- as.data.frame(t(colMeans(olive)))

# Converting Area to a factor variable changes the output
# It allows you to use type = "class" to predict an integral Area value (3)
# Leaving Area as a numeric variable results in a prediction of 2.875
olive$Area = factor(olive$Area)

Mod <- tree(Area ~ ., data = olive)
predict(Mod, newdata)

plot(Mod, uniform = TRUE, main = "Classification Tree")
text(Mod, all = TRUE, cex = 0.8)



# Question 4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train <- sample(1:dim(SAheart)[1], size = dim(SAheart)[1]/2, replace = F)
trainSA <- SAheart[train,]
testSA  <- SAheart[-train,]

set.seed(13234)

ModO <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
            data = trainSA, family = "binomial")

missClass <- function(values, prediction) {
                      sum(((prediction > 0.5)*1) != values)/length(values)
}

# missclassification rate for test set
missClass(testSA$chd, predict(ModO, testSA, type = "response"))

# missclassification rate for training set
missClass(trainSA$chd, predict(ModO,trainSA, type = "response"))




# Question 5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)
# ModX <- train(y ~ ., data = vowel.train, method = "rf", prox = TRUE)

ModY <- randomForest(y ~ ., data = vowel.train, mtry = 3,
                     importance = TRUE, na.action = na.omit)

ModY <- randomForest(y ~ ., data = vowel.train)

varImp(ModY, useModel = TRUE)


