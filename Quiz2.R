
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis, predictors)
testIndex <- createDataPartition(diagnosis, p = 0.5, list = FALSE)
training <- adData[-testIndex,]
testing  <- adData[ testIndex,]


# Question 2

rm(list = ls())

library(AppliedPredictiveModeling)
library(caret)
data(concrete)

set.seed(975)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training <- mixtures[ inTrain,]
testing  <- mixtures[-inTrain,]

names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

###################
nam <- names(concrete)[-which(names(concrete)=="CompressiveStrength")]

getPlots <- list()

fctE <- function(pName) {
  vcut <-cut2(training[,pName],g=3)
  p <- qplot(data=training,y=CompressiveStrength,x=c(1:dim(training)[1]), color=vcut)
  return(p)
} 

for (n in nam) {
  p <- fctE(n)
  getPlots <- append(getPlots,list(p))
}
do.call(grid.arrange, getPlots)
#######################

 # Question 3
rm(list = ls())
library(AppliedPredictiveModeling)
library(caret)
data(concrete)

set.seed(975)
inTrain <- createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training <- mixtures[ inTrain,]
testing  <- mixtures[-inTrain,]

hist(training$Superplasticizer)

str(training$Superplasticizer)

plot(cut2(training$Superplasticizer, g = 5))


# Question 4

rm(list = ls())
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
set.seed(3433)
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain, ]
testing  <- adData[-inTrain, ]


ILvars <- subset(training, select = substring(names(training),1,2) == "IL")
ILvars <- data.frame(ILvars,diagnosis[ inTrain ])
colnames(ILvars)[13] <- "diagnosis"
M <- abs(cor(ILvars[ , -13 ]))
diag(M) <- 0

which(M > 0.6, arr.ind = TRUE)


preProc <- preProcess(log10(ILvars[ , -13 ] + 10), method = "pca", thresh = 0.9)
# print preProc shows 9 components are needed

pca <- princomp(ILvars[,-13], scores = TRUE, cor = TRUE)
summary(pca)
# summary shows 9 components will explain the cumulative variance > 0.9

# Question 5

rm(list = ls())

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
set.seed(3433)
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[ inTrain, ]
testing  <- adData[-inTrain, ]

# create a training data set with predictors beginning with "IL" and the diag
ILvars <- subset(training, select = substring(names(training),1,2) == "IL")
ILvars <- data.frame(ILvars,diagnosis[ inTrain ])
colnames(ILvars)[13] <- "diagnosis"

# build the fancy model
preProc9 <- preProcess(ILvars[ , -13 ], method = "pca", thresh = 0.8)
trainPC9 <- predict(preProc9, ILvars[ , -13])
Mod9 <- train(ILvars$diagnosis ~ ., method = "glm", data = trainPC9)

# build the regular model including all 12 IL variables
preProc0 <- preProcess(ILvars[ , -13 ], method = "pca", pcaComp = 12)
trainPC0 <- predict(preProc0, ILvars[ , -13])
Mod0 <- train(ILvars$diagnosis ~ ., method = "glm", data = trainPC0)

# Now subset the Test set
ILvart <- subset(testing, select = substring(names(testing),1,2) == "IL")
ILvart <- data.frame(ILvart,diagnosis[ -inTrain ])
colnames(ILvart)[13] <- "diagnosis"

# confusion matrix for fancy model
testMod9 <- predict(preProc9, ILvart[ , -13])
confusionMatrix(ILvart$diagnosis, predict( Mod9 ,testMod9 ) )

# confusion matrix for vanilla model
testMod0 <- predict(preProc0, ILvart[ , -13])
confusionMatrix(ILvart$diagnosis, predict( Mod0 ,testMod0 ) )

# Now, with absolutely no preprocessing

Mod6 <- glm(diagnosis ~ ., data = ILvars, family = "binomial")

# first get the probabilities
Mod6_prob <- predict(Mod6, newdata = ILvart[ , -13], type = "response")

# then use the probabilities to derive a response value
Mod6_diag <- rep("Control", 82)
Mod6_diag[Mod6_prob < 0.5] <- "Impaired"
Mod6_diag <- as.factor(Mod6_diag)

# revers the order of factors
Mod6_diag <- factor(Mod6_diag, levels = c("Impaired", "Control"))
confusionMatrix(ILvart$diagnosis, Mod6_diag )



table(ILvart$diagnosis, Mod6_diag)




