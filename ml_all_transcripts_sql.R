target_name <- "age"
    X <- df
    Y <- df[[target_name]]
    X[["sex"]] <- as.factor(X[["sex"]])
    trainIndex <- createDataPartition(Y,
                                      p = .8,
                                      list = FALSE,
                                      times = 1)
    trainSet <- X[trainIndex,]
    testSet  <- X[-trainIndex,]
    #
    if (!is.data.frame(trainSet)) {
      trainSet <- as.data.frame(trainSet)
    }
    if (!is.data.frame(testSet)) {
      testSet <- as.data.frame(testSet)
    }
    #
    ctrl <- trainControl(method = "cv", number = 10)
    #
    # # Train the model
    formula <- as.formula(paste(target_name, "~ ."))
    model <-
      train(
        formula,
        data = trainSet,
        method = model_name,
        trControl = ctrl,
      )
# Make predictions
    pred <- predict(model, newdata = testSet)

    # Evaluate the model
    result_pred <- postResample(pred, testSet[[target_name]])
    rsq <- round(result_pred[["Rsquared"]], digits = 2)
    mae <- round(result_pred[["MAE"]], digits = 2)
    result <- paste(rsq, mae)
    query <- paste("INSERT INTO tatu_ml (project, illumina_column, name, glmnet, cpgcount) VALUES ('GSE87571', 4, '", resultado[i, "name"], "',",rsq,",",countcg,")")
    dbExecute(con, query)
  } else {
    print("The query returned no results.")
    query <- paste("INSERT INTO tatu_ml (project, illumina_column, name) VALUES ('GSE87571', 4, '", resultado[i, "name"], "')")
    dbExecute(con, query)
  }

