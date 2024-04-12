library(MASS)

# Load the data
data <- read.csv("height_data.csv")

# Define a function to evaluate model with given predictors
evaluate_model <- function(predictors, data) {
  formula <- as.formula(paste("height ~", paste(predictors, collapse = "+")))
  model <- lm(formula, data = data)
  pred <- predict(model, newdata = data)
  rmse <- sqrt(mean((pred - data$height)^2))
  r_squared <- summary(model)$r.squared
  return(list(rmse = rmse, r_squared = r_squared, model = model))
}

# Get list of predictor variables
predictors <- names(data)[which(names(data) != "height")]

# Initialize variables to store best model information
best_model <- NULL
best_rmse <- Inf
best_r_squared <- -Inf
best_predictors <- NULL

# Function to generate combinations of predictors including multiplications
generate_combinations <- function(predictors, degree) {
  interactions <- expand.grid(replicate(degree, predictors, simplify = FALSE))
  apply(interactions, 1, function(x) paste(x, collapse = "*"))
}

# Iterate through all possible combinations of predictors
for (i in 1:length(predictors)) {
  combos <- combn(predictors, i)
  for (j in 1:ncol(combos)) {
    predictors_subset <- combos[, j]

    # Include interactions up to a certain degree
    for (degree in 2:3) { # Consider interactions up to degree 3
      interaction_terms <- generate_combinations(predictors_subset, degree)
      for (interaction_term in interaction_terms) {
        predictors_combined <- c(predictors_subset, interaction_term)
        result <- evaluate_model(predictors_combined, data)
        print(paste("Predictors:", paste(predictors_combined, collapse = ", "), "RMSE:", result$rmse, "R-squared:", result$r_squared))
        if (result$rmse < best_rmse) {
          best_rmse <- result$rmse
          best_r_squared <- result$r_squared
          best_model <- result$model
          best_predictors <- predictors_combined
        }
      }
    }
  }
}

# Print the best model and its evaluation metrics
summary(best_model)
print(best_model)
cat("Best RMSE:", best_rmse, "\n")
cat("Best R-squared:", best_r_squared, "\n")
# print mean median of all columns in the data
cat("Mean:\n")
print(apply(data, 2, mean))

cat("Median:\n")
print(apply(data, 2, median))

cat("Standard Deviation:\n")
print(apply(data, 2, sd))

cat("Variance:\n")
print(apply(data, 2, var))

cat("Maximum:\n")
print(apply(data, 2, max))

cat("Minimum:\n")
print(apply(data, 2, min))

cat("Range:\n")
print(apply(data, 2, range))
cat("Predictor variables in the best model:", paste(best_predictors, collapse = ", "), "\n")

# newData <- data.frame(
#   Age = 7,
#   BloodPressure = 71,
#   BloodSugar = 89,
#   Diabetic = 0
# )

# # Predict the medical bill for new data
# pred <- predict(best_model, newdata = newData)
# print(paste("Predicted Medical Bill:", pred))
