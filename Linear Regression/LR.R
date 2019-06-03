library(readr)

#Reading of data set
kre.df <- read_csv("Kre.csv")

#Selecting the predictors
sel.var <- c(2:7,9)

#Set seed for reproducing partition
set.seed(1) 
options(scipen=999)

#!!!!!!!!! Question i) !!!!!!!!!!!!!!
#Removing the rows where Purchasing = 0 as we are predicting the amount a PURCHASING customer will spend for the 
#remainder of 2019. 
kre.df <- kre.df[kre.df$PURCHASE != 0, ]

#Splitting the dataset
#train.index <- sample(c(1:2000),1200)
train.index <- sample(c(1:1000),600)
train.mlm <- kre.df[train.index,sel.var]
test.mlm <- kre.df[-train.index,sel.var]

#!!!!!!!!! Question ii) !!!!!!!!!!!!!!
#Linear Regression on the dataset
kre.mlm <- lm(formula = CY_SPENT ~., data=train.mlm)

kre.mlm  # displays output from Simple LinReg Model
summary(kre.mlm)

#!!!!!!!!! Question iv) !!!!!!!!!!!!!!
kre.mlm$coef %*% c(1, 0, 4, 450, 1, 0, 0)


#!!!!!!!!! Question v) !!!!!!!!!!!!!!
# Using the trained model to make prediction on the test set with predict()
library(forecast)

train.pred <- predict(kre.mlm,train.mlm)
test.pred <- predict(kre.mlm,test.mlm)

#!!!!!!!!! Question vi) !!!!!!!!!!!!!!
train.residual.sel <- train.mlm$CY_SPENT - train.pred 
test.residual.sel <- test.mlm$CY_SPENT - test.pred 

train.errors<- data.frame("Predicted"=train.pred,
                              "Actual"=train.mlm$CY_SPENT,
                              "Residual"=train.residual.sel )

test.errors <- data.frame("Predicted"=test.pred ,
                              "Actual"=test.mlm$CY_SPENT,
                              "Residual"=test.residual.sel)

#!!!!!!!!! Question vii) !!!!!!!!!!!!!!
accuracy(test.pred ,test.mlm$CY_SPENT) 

accuracy(train.pred ,train.mlm$CY_SPENT)
