library("purrr")

# question 1
# Consider the following data with x as the predictor and y as as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

# Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.
fit <-  lm(y~x)
round(summary(fit)$coefficients[2, 4], digits = 5)

# question 2
# Consider the previous problem, give the estimate of the residual standard deviation.
round(summary(fit)$sigma, 3)

# question 3
# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). 
# Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit <- lm(y~x, data = mtcars)
predict(fit, newdata = data.frame(x = mean(x)), interval = "confidence")

# question 4
# Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?
?mtcars

# question 5
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight 
# (1,000 lbs). A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for 
# its mpg. What is the upper endpoint?
y <- mtcars$mpg
x <- mtcars$wt
fit <- lm(y~x)
predict(fit, newdata = data.frame(x = 3), interval = "prediction")

# question 6

# question 7

# question 8

# question 9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. 
# About what is the ratio of the the sum of the squared errors, ∑i=1n(Yi−Y^i)2 when comparing a 
# model with just an intercept (denominator) to the model with the intercept and 
# slope (numerator)?
y <- mtcars$mpg
x <- mtcars$wt
fit <- lm(y~x, y = TRUE)

with(fit, 1 - c(crossprod(residuals) / crossprod(y - mean(y))))
with(fit, 1 - c(crossprod(residuals) / crossprod(y)))

# question 10
t1s <- lapply(1:1000, function(x) rnorm(100, 0, 1))
t2s <- lapply(1:1000, function(x) rnorm(100, 0, 1))

ms <- vector(mode = "list", length = length(t1s))
for (i in seq_along(t1s)) {
  ms[[i]] <- lm(t2s[[i]] ~ t1s[[i]])
}

hist(map_dbl(ms, ~sum(.x$residuals)))
