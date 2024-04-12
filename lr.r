# Create a dataframe with the provided data
data <- read.csv("height_data.csv")

# Fit a linear regression model
model <- lm(height ~ mother_height, data = data)

# Plot the data and the regression line
plot(data$mother_height, data$height, main = "Height Data", xlab = "Father Height", ylab = "Height")
abline(model, col = "red")

# also print model equation
cat("Model equation: height = ", round(coef(model)[1], 2), " + ", round(coef(model)[2], 2), " * mother_height\n")

# Print the summary of the model
summary(model)

# Print the R-squared value
summary(model)$r.squared
