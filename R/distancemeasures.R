# Load necessary libraries
# These libraries are typically pre-installed
library(ggplot2)
library(car)

# Sample data: Using the built-in 'mtcars' dataset
data(mtcars)


# Fit a linear regression model
# Predicting 'mpg' (miles per gallon) based on 'disp' (displacement), 'hp' (horsepower), and 'wt' (weight)
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

### So far the test setup

distances <- function(data, model, measure='cooks'){
  #input validation
  if (!is.data.frame(data)) {
    stop("The input data must be of type data.frame.")
  }

  if (class(model) != "lm") {
    stop("The input model must be an object of class lm")
  }

  ##do more input data validations...
  ## do we need consistency checks between input data and what is in the model?
  ## check limitations of each method... e.g. https://www.statology.org/dffits-in-r/
  #e.g. A size-adjusted cutoff recommended by Belsley, Kuh, and Welsch is 2*sqrt(p/n)
  #not sure why the data also had to be provided to the function.
  #are we not able to get everything we need from the model itself?
  #Hadi measure also has a cut off value@!
}

#based on https://doi.org/10.2307/1268249
#https://doi.org/10.1080/00401706.1977.10489493

#From: https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.influence.R
cooks_distance_lm <- function(model){
          #as per page 15 of Cook 1977 (above equation 1)
          resid = weighted.residuals(model) #to allow for weighted regression
          #residual degrees of freedom (number of observ.
          #minus number of regression coefficients)
          df= df.residual(model)
          sd = sqrt(deviance(model)/df)
          #diagonals of the ‘hat’ matrix.
          hat = lm.influence(model, do.coef=FALSE)$hat

          p <- model$rank
          #equation 7 in Cook 1977
          D <- ((resid/(sd * sqrt((1 - hat)))^2 * hat)/(p*(1 - hat)))
          D[is.infinite(D)] <- NaN
          return(D)
  }


#based on https://avys.omu.edu.tr/storage/app/public/rezzanu/141865/[David_A._Belsley,_Edwin_Kuh,_Roy_E._Welsch]_Regre(BookFi.org).pdf
#based on https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.influence.R
dffits_lm <- function(model){

  hat = lm.influence(model, do.coef=FALSE)$hat
  sigma = lm.influence(model, do.coef=FALSE)$sigma
  res = weighted.residuals(model)
  #based on equation 2.11 on page 15 of Belsley 1980
  #re-written to avoid one sqrt(1-hat)
  dffits <- res * sqrt(hat)/(sigma*(1-hat))
  dffits[is.infinite(dffits)] <- NaN
  return(dffits)
}

hadi_lm <- function(model){

}

hadi_lm <- function(model) {
  X <- model.matrix(model)  # Model matrix (design matrix)
  n <- nrow(X)  # Number of observations
  p <- ncol(X)  # Number of predictors
  H <- hatvalues(model)  # Leverage values
  res <- residuals(model, type = "response")  # Residuals
  sigma2 <- sum(res^2) / (n - p)  # Estimate of variance

  # Calculate Hadi's influence measure
  Hadi <- (H / (1 - H)) * (res^2 / (p * sigma2)) * (1 + H * (p + 1) / (1 - H))

  return(Hadi)
}

#Still need to dynamically estimate p= number of predictor vars (it's not always 1!!)
#summary(model)$df[1] - 1 is number of betas without intercept
#or length(coef(fit))-1

##based on equation 3.7 in Hadi 1992 and 4.9.3 in Regression-Analysis-by-Example
hadi <- function(model) {
  h <- hatvalues(model)
  #based on sentence under equation 3.7
  di <- residuals(model)/sqrt(sum(residuals(model)^2))
  h/(1-h) + 2/(1-h) * di^2/(1-di^2)
}



# Plotting Cook's Distance
# Plot using base R
plot(cooks_d,
     type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Number",
     col = "blue")

# Add a horizontal line at 4/(n-k-1) threshold (common rule of thumb)
abline(h = 4/(nrow(mtcars) - length(coef(model)) - 1), col = "red", lty = 2)

# Highlight observations with high Cook's Distance
text(x = 1:length(cooks_d), y = cooks_d, labels = ifelse(cooks_d > 4/(nrow(mtcars) - length(coef(model)) - 1), names(cooks_d), ""), pos = 4, cex = 0.7, col = "red")

# Alternative: Plot using ggplot2 for a more polished look
ggplot(data = data.frame(Observation = 1:length(cooks_d), CooksDistance = cooks_d), aes(x = Observation, y = CooksDistance)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(yintercept = 4/(nrow(mtcars) - length(coef(model)) - 1), color = "red", linetype = "dashed") +
  geom_text(aes(label = ifelse(cooks_d > 4/(nrow(mtcars) - length(coef(model)) - 1), Observation, "")),
            hjust = 1.5, color = "red", size = 3.5) +
  labs(title = "Cook's Distance", x = "Observation Number", y = "Cook's Distance") +
  theme_minimal()

