library(data.table)
library(ggplot2)
library(statisticalModeling)
train <- fread("train.csv")
test <- fread("test.csv")

# Mon Dec 26 22:04:54 2016 ------------------------------
# response variable -> SalePrice(in dollars)

# distribution of response variable
ggplot(train, aes(x = SalePrice)) + geom_histogram()

# explore each explanatory(may be) variable

# 1. BedroomAbvGr
ggplot(train, aes(x = BedroomAbvGr)) + geom_histogram()
ggplot(train, aes(y = SalePrice, x = BedroomAbvGr)) + geom_point()

model1 <- lm(SalePrice~BedroomAbvGr, data = train)
fmodel(model1, ~BedroomAbvGr, data = train) + 
  geom_point(aes(y = SalePrice, x = BedroomAbvGr), data = train)

model2 <- lm(SalePrice~BedroomAbvGr + I(BedroomAbvGr^2), data = train)
fmodel(model2, ~BedroomAbvGr, data = train) + 
  geom_point(aes(y = SalePrice, x = BedroomAbvGr), data = train)

cerr <- cv_pred_error(model1, model2)
t.test(mse~model, cerr) # since p-value is high
# we fail to reject the null hypothesis which is 'prediction error is same'

# predict output for training data
output1 <- evaluate_model(model1, data = train)
output2 <- evaluate_model(model2, data = train)

# RMSE
with(output1, sqrt(mean((SalePrice-model_output)^2)))
with(output2, sqrt(mean((SalePrice-model_output)^2)))

# predict output for test data
output <- evaluate_model(model1, data = test)

df <- data.frame(Id = test$Id, SalePrice = output$model_output)
write.csv(x = df, file = "model1.csv", quote = F, row.names = F)