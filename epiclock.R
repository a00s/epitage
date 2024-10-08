# Load necessary libraries
library(caret)

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if the argument was passed
if (length(args) == 0) {
  stop("No input file provided.")
}

# Load the trained model from file
model <- readRDS("epitage_trained_model_cubist.rds")

# Load the new data (the CSV file passed as an argument)
new_data <- read.table(
  args[1],
  header = TRUE,
  sep = ",",
  row.names = 1,
  dec = ".",
  stringsAsFactors = FALSE,
  strip.white = TRUE
)

# Preprocess the new data (if necessary)
new_data[is.na(new_data)] <- 0.0
new_data[new_data == ""] <- 0.0
new_data <- new_data[, apply(new_data, 2, function(col) !all(col == 0.0))]

# Ensure that new data has the same columns as the training data
model_features <- model$finalModel$xNames  # Get the features used in the trained model

# Add any missing columns to the new data, filling them with 0s
for (feature in model_features) {
  if (!(feature %in% colnames(new_data))) {
    new_data[[feature]] <- 0  # Add the missing column with all values set to 0
  }
}

# Ensure categorical variables are set correctly (modify if necessary)
if ("sex" %in% colnames(new_data)) {
  new_data[["sex"]] <- as.factor(new_data[["sex"]])
}

# Extract the sample names (row names), original ages, and remove the 'age' column from features
sample_names <- rownames(new_data)
original_ages <- new_data[["age"]]
new_data <- new_data[, !(colnames(new_data) %in% c("age"))]  # Remove 'age' from predictors

# Make predictions with the loaded model
predictions <- predict(model, newdata = new_data)

# Round predicted ages
predictions_rounded <- round(predictions)
original_ages_rounded <- round(original_ages)

# Calculate the difference between predicted and original ages (rounded)
age_difference <- predictions_rounded - original_ages_rounded

# Prepare the output data frame
output <- data.frame(Sample = sample_names, 
                     Original_Age = original_ages_rounded, 
                     Predicted_Age = predictions_rounded, 
                     Difference = age_difference)

# Print the output as CSV to the console
write.csv(output, row.names = FALSE)