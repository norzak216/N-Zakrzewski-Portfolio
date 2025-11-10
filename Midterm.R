
install.packages("caret", dependencies = TRUE)

library(caret)


evals <- read.csv(
  "/Users/nora/Desktop/DSS R Programming/Datasets/evals.csv",
  header = TRUE
)
evals

# Convert categorical variables to factors
evals$rank <- as.factor(evals$rank)
evals$ethnicity <- as.factor(evals$ethnicity)
evals$gender <- as.factor(evals$gender)
evals$language <- as.factor(evals$language)
evals$cls_level <- as.factor(evals$cls_level)
evals$cls_profs <- as.factor(evals$cls_profs)
evals$pic_outfit <- as.factor(evals$pic_outfit)
evals$pic_color <- as.factor(evals$pic_color)

#split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(evals$score, p = 0.8, list = FALSE)
train <- evals[train_index, ]
test <- evals[-train_index, ]


#training simple linear regression model
model <- train(
  score ~ .,             # predict score using all other variables
  data = train,
  method = "rf",         # random forest
  preProcess = c("center", "scale"),  # standardize numeric variables
  trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
)

#results
print(model)
summary(model$finalModel)

#check variable importance numerically
importance <- varImp(model)

print(importance)

# Plot variable importance
plot(importance, top = 10, main = "Top 10 Most Important Predictors")




