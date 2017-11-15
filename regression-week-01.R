library("purrr")

# question 1
# Consider the data set given below
x <- c(0.18, -1.54, 0.42, 0.95)
# And weights given by
w <- c(2, 1, 3, 1)
# Give the value of μ that minimizes the least squares equation
# sum(w*(x-mu)^2)

mus <- seq(mean(x) - 10 * sd(x), mean(x) + 10 * sd(x), length.out = 1000)

outs <- sapply(mus, function(z) sum(w*(x-z)^2))

plot(outs)

mus[which.min(outs)]

# question 2
# Consider the following data set
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
# Fit the regression through the origin and get the slope treating y as the outcome and x as the 
# regressor. (Hint, do not center the data since we want regression through the origin, not through the means of the data.)

plot(x, y)

lm(y~x, offset = 0)$coefficients[2]

# question 3
# Do data(mtcars) from the datasets package and fit the regression model with mpg as the outcome and 
# weight as the predictor. Give the slope coefficient.
data(mtcars)
lm(mpg~wt, data = mtcars)$coefficients[2]

# question 4
# Consider data with an outcome (Y) and a predictor (X). The standard deviation of the predictor 
# is one half that of the outcome. The correlation between the two variables is .5. What value 
# would the slope coefficient for the regression model with Y as the outcome and X as the predictor?

xs <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 0.5))
ys <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 1))

cors <- map2_dbl(xs, ys, ~cor(.x, .y))

plot(sort(cors))

ids <- which(abs(cors - 0.5) < 0.1)

coeffs <- vector(mode = "numeric", length = length(ids))
for (i in seq_along(ids)) {
  coeffs[i] <- lm(ys[[ids[i]]] ~ xs[[ids[i]]])$coefficients[2]
}

mean(coeffs)

# question 5
# Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1. 
# The correlation between the scores on the two tests was 0.4. What would be the expected score on 
# Quiz 2 for a student who had a normalized score of 1.5 on Quiz 1?

t1s <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 1))
t2s <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 1))

cors <- map2_dbl(t1s, t2s, ~cor(.x, .y))

ids <- which(abs(cors - 0.4) < 0.05)

lms <- vector(mode = "list", length = length(ids))
for (i in seq_along(ids)) {
  lms[[i]] <- lm(t2s[[ids[[i]]]] ~ t1s[[ids[[i]]]])
}

slopes <- sapply(lms, function(x) x$coefficients[2])
intercepts <- sapply(lms, function(x) x$coefficients[1])

mean(map2_dbl(slopes, intercepts, ~.x * 1.5 + .y))

# question 6
# Consider the data given by the following
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
# What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?

# question 7
# Consider the following data set (used above as well). What is the intercept for fitting the model 
# with x as the predictor and y as the outcome?

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x)$coefficients[1]

# question 8
# You know that both the predictor and response have mean 0. What can be said about the 
# intercept when you fit a linear regression?
preds <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 1))
resps <- lapply(1:1000, function(x) rnorm(25, mean = 0, sd = 1))

ints <- vector(mode = "numeric", length = length(preds))
for (i in seq_along(ids)) {
  ints[i] <- lm(resps[[i]] ~ preds[[i]])$coefficients[1]
}

plot(sort(ints))

# question 9

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

# What value minimizes the sum of the squared distances between these points and itself?

lm(x~x)

# question 10
# Let the slope having fit Y as the outcome and X as the predictor be denoted as β1. Let the 
# slope from fitting X as the outcome and Y as the predictor be denoted as γ1. Suppose that you 
# divide β1 by γ1; in other words consider β1/γ1. What is this ratio always equal to?

x <- rnorm(25, mean = 0, sd = 1)
y <- rnorm(25, mean = 0, sd = 1)

beta <- lm(y~x)$coefficients[2]
gamma <- lm(x~y)$coefficients[2]

out <- beta/gamma

abs(var(y)/var(x) - out) < 0.01
abs(cor(y, x) - out) < 0.01
abs(1 - out) < 0.01
abs(2*sd(y)/sd(x) - out) < 0.01
