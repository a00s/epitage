sink("/dev/null")
library(caret)
args <- commandArgs(trailingOnly = TRUE)
model_name <- "cubist"
model_info <- getModelInfo(model_name, regex = FALSE)[[model_name]]
model_libraries <- model_info$library
for (lib in model_libraries) {
  library(lib, character.only = TRUE)
}

df_pre <-
  read.table(
    args[1],
    header = TRUE,
    sep = ",",
    row.names = 1,
    dec = ".",
    stringsAsFactors = FALSE,
    strip.white = TRUE
)

df_pre[is.na(df_pre)] <- 0.0
df_pre[df_pre == ""] <- 0.0
df_pre <- df_pre[, apply(df_pre, 2, function(col) !all(col == 0.0))]

target_name <- "age"
X <- df_pre
Y <- df_pre[[target_name]]
print(class(X))
print(head(X))

X[["sex"]] <- as.factor(X[["sex"]])

trainIndex <- createDataPartition(Y,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
set.seed(524)
trainSet <- X[trainIndex,]
testSet  <- X[-trainIndex,]

if (!is.data.frame(trainSet)) {
  trainSet <- as.data.frame(trainSet)
}
if (!is.data.frame(testSet)) {
  testSet <- as.data.frame(testSet)
}

ctrl <- trainControl(method = "cv", number = 5)

# Train the model
formula <- as.formula(paste(target_name, "~ ."))
model <-
  train(
    formula,
    data = trainSet,
    method = model_name,
    trControl = ctrl
  )

# Save the model to a file
saveRDS(model, "epitage_trained_model_cubist.rds")

# Make predictions
pred <- predict(model, newdata = testSet)

# Evaluate the model
result_pred <- postResample(pred, testSet[[target_name]])
rsq <- round(result_pred[["Rsquared"]], digits = 10)
mae <- round(result_pred[["MAE"]], digits = 10)
#result <- paste(result_pred[["Rsquared"]], result_pred[["MAE"]])
result <- paste(rsq, mae)
sink()
cat(result)

