install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("e1071")
install.packages("pROC")
install.packages("C50")
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(pROC)
library(C50)

# a) load genie dataset and replace column headers
geniefd.df <- read.csv("geniefd.csv")
geniefdheaders.df <- read.csv("geniefdcol.csv")
colnames(geniefd.df) <- geniefdheaders.df[,]

# b) 
# i) Partioning Dataset to 60(training) and 40(test)
set.seed(1)
geniefd.df.index <- sample(c(1:nrow(geniefd.df)[1]),nrow(geniefd.df)[1]*.6)
geniefd.df.train <- geniefd.df[geniefd.df.index,]
geniefd.df.test  <- geniefd.df[-geniefd.df.index,]

# ii) In PDF
# iii) 
rpart.tree <- rpart(FlashDeal ~., data = geniefd.df.train,
                    control = rpart.control(maxdepth = 5, minbucket =50), method = "class")

prp(rpart.tree, type = 3, extra = 1, under = T, split.font = 1, varlen = -10)

rpart.tree.pred.train <- predict(rpart.tree,geniefd.df.train,type="class")
confusionMatrix(table(rpart.tree.pred.train, geniefd.df.train$FlashDeal),
                positive = "1")

# predictions on test set
rpart.tree.pred.test <- predict(rpart.tree,geniefd.df.test,type="class")
confusionMatrix(table(rpart.tree.pred.test, geniefd.df.test$FlashDeal),
                positive = "1")
# iv) In PDF

# v) 
#Using C50 Decision Trees
geniefd.df.c5<-C5.0(as.factor(FlashDeal)~., data = geniefd.df.train)

# Rules set
geniefd.df.c5.rules<-C5.0(as.factor(FlashDeal)~., data = geniefd.df.train, rules= TRUE)
summary(geniefd.df.c5.rules)

# on test set
geniefd.df.c5.pred <- predict(geniefd.df.c5,geniefd.df.test,type="class")
confusionMatrix(table(geniefd.df.c5.pred, geniefd.df.test$FlashDeal),
                positive = "1")

# c) 
# i) Evaulating the performance of rpart and c5.0
confusionMatrix(table(rpart.tree.pred.test, geniefd.df.test$FlashDeal),
                positive = "1")

confusionMatrix(table(geniefd.df.c5.pred, geniefd.df.test$FlashDeal),
                positive = "1")

#From the results, c5 is a better model. 

# ii) In PDF

# d) 
# i) Extracting and exporting the rules into a text file. 
write(capture.output(summary(geniefd.df.c5.rules)), "c50model.txt")

#ii) In PDF
#iii) In PDF
