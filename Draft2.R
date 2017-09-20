setwd("C:\\Users\\guo\\Desktop\\pro")
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(caret)
require(corrplot)
require(Hmisc)
require(parallel)
require(doParallel)
require(ggthemes)
require(MASS)
install.packages("caret")
install.packages("corrplot")

# parallel processing set up
n_Cores = detectCores()
cluster_Set = makeCluster(n_Cores)
registerDoParallel(cluster_Set)

# check data 
ship_Original=read.csv("data1.csv",stringsAsFactors=TRUE)
describe(ship_Original)

### impute missing data
original_Impute = preProcess(ship_Original,method="bagImpute")
ship_Original = predict(original_Impute,ship_Original)
describe(ship_Original)


###find correlations between factors
factor_Corr= cor(ship_Original[,-c(1,2,3,5,6,7,8,13,14,22,23)])
corrplot(factor_Corr,method="number")

### separate dataset into training and testing sets
ship_Original=ship_Original[,-c(1,11)]
sample_Index = createDataPartition(ship_Original$price,p=0.99,list=FALSE)
ship_Train = ship_Original[sample_Index,]
ship_Test = ship_Original[-sample_Index,]
attach(ship_Train)

### preprocess factors for further modeling
###pp = preProcess(ship_Train,method=c("scale","center","pca"),outcome=ship_Train$price)
###ship_Train = predict(pp,ship_Train)
###ship_Test = predict(pp,ship_Test)


### define formula

model_Formula = price~vehicles+ vehicleOperable+numVehicles+pickup.city+pickup.state+delivery.city+delivery.state+truckMiles+total_weight+both_states+rv+van+travel.trailer+car+atv+suv+pickup+motorcycle+heavy.equipment+boat


###set cross-validation parameters

modelControl = trainControl(method="repeatedcv",number=5,
                             repeats=5,allowParallel=TRUE)

###model selection stepwise
model=lm(model_Formula)
step=stepAIC(model,direction="both")

### stepwise model
model2=lm(price~vehicles+ vehicleOperable+pickup.city+delivery.city+total_weight+both_states+total_weight)
model1_prediction=predict(model,ship_Test)
model2_prediction=predict(mode2,ship_Test)
##model1 compare tp Test data
plot( ship_Test$price,type="l",col="blue", ylab="Price")
 lines(model1_prediction,col="red")
##Model2 compare to test data
##plot( ship_Test$price,type="l",col="blue", ylab="Price")
 ##lines(model2_prediction,col="red")



### random forrest
##rf_Model <- train(model_Formula,
                  data=ship_Train,
                  method="rf",
                  trControl=modelControl,
                  ntrees=500)








