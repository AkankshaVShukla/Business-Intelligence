
#PART 3
#Iris dataset
K=10
irisfolds <- cvFolds(nrow(iris),K,type="random")
errorIrisDT <- c()
errorIrisSVM <- c()

#Breast Cancer dataset
breastCancer <- read.csv("Wisconsin_Breast_Cancer_data.csv", header = FALSE)[, 2:32]
cancerfolds <- cvFolds(nrow(breastCancer), K, type="random")
errorCancerDT <- c()
errorCancerSVM <- c()

#Ecoli datatset
ecoli = read.csv("Ecoli_data.csv", header = FALSE)
ecolifolds <- cvFolds(nrow(ecoli), K, type="random")
errorEcoliDT <- 0
errorEcoliSVM <- 0

#Glass dataset
glass = read.csv("Glass_data.txt", header = FALSE, colClasses=c(rep('numeric', 10), 'factor'))
glassfolds <- cvFolds(nrow(glass), K, type="random")
errorGlassDT <- c()
errorGlassSVM <- c()

#Yeast dataset
yeast = read.csv("Yeast_data.csv", header = FALSE)
yeastFolds <- cvFolds(nrow(yeast), K, type="random")
errorYeastDT <- c()
errorYeastSVM <- c()

for(i in 1:K){
  # Iris
  index <- which((irisfolds$which == i))
  trainset <- iris[index, ]
  #print(trainset)
  testset <- iris[-index, ]
  
  #Decision Tree Model
  fitIrisDT <- C50::C5.0(trainset[,1:4], trainset$Species)
  predIrisDT <- predict(fitIrisDT, testset[,1:4])
  #errorIrisDT[i] <- sqrt(sum((as.numeric(testset$Species) - as.numeric(predIrisDT))^2)/length(testset))
  error1 <- sum((predIrisDT != testset$Species) / length(predIrisDT))
  errorIrisDT[i] <- error1*100
  #Support Vector Machine Model
  fitIrisSVM <- ksvm(Species~., trainset)
  predIrisSVM <- predict(fitIrisSVM, testset[,1:4])
  #errorIrisSVM[i] <- sqrt(sum((as.numeric(testset$Species) - as.numeric(predIrisSVM))^2)/length(testset))
  error2 <- sum((predIrisSVM != testset$Species) / length(predIrisSVM))
  errorIrisSVM[i] <- error2*100
  
  # Breast Cancer
  index <- which((cancerfolds$which == i))
  trainset <- breastCancer[index, ]
  testset <- breastCancer[-index, ]
  #Decision Tree Model
  fitCancerDT <- C50::C5.0(trainset[, 2:31], trainset$V2, trials = 10)
  predCancerDT <- predict(fitCancerDT, testset[ ,2:31])
  #errorDTreeCancer[i] <- sqrt(sum((as.numeric(testset$V2) - as.numeric(predCancerDT))^2)/length(testset))
  error3 <- sum((predCancerDT != testset$V2) / length(predCancerDT))
  errorCancerDT[i] <- error3*100
  #Support Vector Machine Model
  fitCancerSVM <- ksvm(V2~., trainset)
  predCancerSVM <- predict(fitCancerSVM, testset[ ,2:31])
  #errorCancerSVM[i] <- sqrt(sum((as.numeric(testset$V2) - as.numeric(predCancerSVM))^2)/length(testset))
  error4 <- sum((predCancerSVM != testset$V2) / length(predCancerSVM))
  errorCancerSVM[i] <- error4*100
  
  # Ecoli
  index <- which((ecolifolds$which == i))
  trainset <- ecoli[index, ]
  testset <- ecoli[-index, ]
  #print(trainset)
  #Decision Tree Model
  fitEcoliDT <- C50::C5.0(trainset[, 1:8], trainset$V9, trials = 10)
  predEcoliDT <- predict(fitEcoliDT, testset[ ,1:8])
  #errorEcoliDT <- sqrt(sum((as.numeric(testset$V9) - as.numeric(predEcoliDT))^2)/length(testset))
  error5 <- sum((predEcoliDT != testset$V9) / length(predEcoliDT))
  errorEcoliDT[i] <- error5*100
  #Support Vector Machine Model 
  fitEColiSVM <- ksvm(V9 ~ ., trainset)
  predEcoliSVM <- predict(fitEColiSVM, testset[ ,1:8])
  #errorEcoliSVM <- sqrt(sum((as.numeric(testset$V9) - as.numeric(predEcoliSVM))^2)/length(testset))
  error6 <- sum((predEcoliSVM != testset$V9) / length(predEcoliSVM))
  errorEcoliSVM[i] <- error6*100
  # Glass
  index <- which((glassfolds$which == i))
  trainset <- glass[index, ]
  testset <- glass[-index, ]
  #Decision Tree Model
  fitGlassDT <- C50::C5.0(trainset[, 1:10], trainset$V11, trials = 10)
  predGlassDT <- predict(fitGlassDT, testset[ ,1:10])
  # errorGlassDT <- sqrt(sum((as.numeric(testset$V11) - as.numeric(predEcoliSVM))^2)/length(testset))
  error7 <- sum((predGlassDT != testset$V11) / length(predGlassDT))
  errorGlassDT[i] <- error7*100
  #Support Vector Machine Model
  fitGlassSVM <- ksvm(V11 ~ ., trainset)
  predGlassSVM <- predict(fitGlassSVM, testset[ ,1:10])
  #errorGlassSVM <- sqrt(sum((as.numeric(testset$V11) - as.numeric(predGlassSVM))^2)/length(testset))
  error8 <- sum((predGlassSVM != testset$V11) / length(predGlassSVM))
  errorGlassSVM[i] <- error8*100
  
  # Yeast
  index <- which((yeastFolds$which == i))
  trainset <- yeast[index, ]
  testset <- yeast[-index, ]
  #print(trainset)
  #Decision Tree Model
  fitYeastDT <- C50::C5.0(trainset[, 1:9], trainset$V10, trials = 10)
  predYeastDT <- predict(fitYeastDT, testset[ ,1:9])
  #errorDTreeYeast <- sqrt(sum((as.numeric(testset$V11) - as.numeric(predYeastDT))^2)/length(testset))
  error9 <- sum((predYeastDT != testset$V10) / length(predYeastDT))
  errorYeastDT[i] <- error9*100
  #Support Vector Model
  fitYeastSVM <- ksvm(V10 ~ ., trainset)
  predYeastSVM <- predict(fitYeastSVM, testset[ ,1:9])
  #errorYeastSVM <- sqrt(sum((as.numeric(testset$V10) - as.numeric(predYeastDT))^2)/length(testset))
  error10 <- sum((predYeastSVM != testset$V10) / length(predYeastSVM))
  errorYeastSVM[i] <- error10*100
}
errorIrisDT
errorIrisSVM
errorCancerDT
errorCancerSVM
errorEcoliDT
errorEcoliSVM
errorGlassDT
errorGlassSVM
errorYeastDT
errorYeastSVM
# Wilcoxon Signed Rank test
errorDT <- rbind(errorIrisDT,errorCancerDT,errorEcoliDT, errorGlassDT,errorYeastDT)
errorSVM <- rbind(errorIrisSVM, errorCancerSVM,errorEcoliSVM, errorGlassSVM, errorYeastSVM)

wilcox.test(errorDT,errorSVM, paired = TRUE)