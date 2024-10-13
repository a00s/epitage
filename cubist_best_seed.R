library(caret)
library(doParallel)
current_seed <- 0
# Detectar o número de núcleos disponíveis no sistema
numCores <- detectCores()

# Registrar os núcleos para paralelismo
cl <- makeCluster(numCores)
registerDoParallel(cl)

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
X[["sex"]] <- as.factor(X[["sex"]])

best_rsq <- -Inf  # Inicializando o melhor valor de Rsquared
threshold <- 0.001  # Defina um limite para a melhoria mínima

repeat {
  # Definir uma semente aleatória para cada iteração
  current_seed <- current_seed + 1
  set.seed(current_seed)  # Define a semente
  
  trainIndex <- createDataPartition(Y, p = .8, list = FALSE, times = 1)
  trainSet <- X[trainIndex,]
  testSet  <- X[-trainIndex,]

  if (!is.data.frame(trainSet)) {
    trainSet <- as.data.frame(trainSet)
  }
  if (!is.data.frame(testSet)) {
    testSet <- as.data.frame(testSet)
  }

  # Controle de treinamento, agora utilizando paralelismo
  ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

  # Treina o modelo
  formula <- as.formula(paste(target_name, "~ ."))
  model <- train(formula, data = trainSet, method = model_name, trControl = ctrl)
  
  # Faz previsões
  pred <- predict(model, newdata = testSet)
  
  # Avalia o modelo
  result_pred <- postResample(pred, testSet[[target_name]])
  rsq <- round(result_pred[["Rsquared"]], digits = 10)
  mae <- round(result_pred[["MAE"]], digits = 10)
  
  # Imprime o resultado atual
  result <- paste("Rsquared:", rsq, "MAE:", mae, "Seed:",current_seed)
  cat(result, "\n")
  
  # Verifica se o Rsquared melhorou
  improvement <- rsq - best_rsq
  if (improvement > threshold) {
    best_rsq <- rsq
    best_mae <- mae
  } else {
    # Se quiser continuar rodando mesmo sem melhoria, basta remover o break
    # break  # Se não houver melhoria significativa, sairia do loop
  }
}

# Finaliza o cluster
stopCluster(cl)
registerDoSEQ()  # Volta ao processamento sequencial
