#' distances
#'
#' @param data A Dataframe
#' @param model A linear model of class lm
#' @param plots A boolean parameter to supress plots
#'
#' @return a list with names entries (cooks,dffits,hadi)
#' @export
#'
#' @examples{ x1 <- rnorm(100, 50, 9);
#' y1 <-5*x1+1;
#' randomdata <- data.frame(y1, x1);
#' model <- lm(y1 ~ x1, data = randomdata);
#' distances(randomdata, model, plots = TRUE)
#' }

distances <- function(data, model, plots=TRUE){
# input validation
  if (!is.data.frame(data)) {
    stop("The input data must be of type data.frame.")
  }

  if (!inherits(model,'lm')) {
    stop("The input model must be an object of class lm")
  }

  if (!inherits(plots,'logical')) {
    stop("The input parameter plots must be a boolean")
  }

  cooks <- cooks_distance_lm(model)
  dffits <- dffits_lm(model)
  hadi <- hadi_lm(model)

  if (plots) {
    # divide frame in 2X2 grid
    graphics::par(mfrow = c(2, 2))
    # draw 4 plots
    plot(cooks, layout = NULL)
    plot(dffits, layout = NULL)
    plot(hadi, layout = NULL)
  }
  return(list(cooks = cooks, dffits = dffits, hadi = hadi))

}

# based on https://doi.org/10.1080/00401706.1977.10489493
# and https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.influence.R
cooks_distance_lm <- function(model) {
  # as per page 15 of Cook 1977 (above equation 1)
  resid <- stats::weighted.residuals(model) # to allow for weighted regression
  # residual degrees of freedom (number of observ.
  # minus number of regression coefficients)
  df <- stats::df.residual(model)
  sd <- sqrt(stats::deviance(model) / df)
  # diagonals of the ‘hat’ matrix.
  hat <- stats::lm.influence(model, do.coef = FALSE)$hat

  p <- model$rank
  # equation 7 in Cook 1977
  D <- ((resid / (sd * sqrt((1 - hat))))^2 * hat / (p * (1 - hat)))
  D[is.infinite(D)] <- NaN
  return(D)


}

# based on https://avys.omu.edu.tr/storage/app/public/rezzanu/141865/[David_A._Belsley,_Edwin_Kuh,_Roy_E._Welsch]_Regre(BookFi.org).pdf
# and https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.influence.R
dffits_lm <- function(model) {
  hat <- stats::lm.influence(model, do.coef = FALSE)$hat
  sigma <- stats::lm.influence(model, do.coef = FALSE)$sigma
  res <- stats::weighted.residuals(model)
  # based on equation 2.11 on page 15 of Belsley 1980
  dffits <-  sqrt(hat/(1-hat))* (res/ (sigma * sqrt(1 - hat)))
  dffits[is.infinite(dffits)] <- NaN
  return(dffits)
}

## based on equation 3.7 in Hadi 1992 and 4.9.3 in Regression-Analysis-by-Example
hadi_lm <- function(model) {
  h <- stats::hatvalues(model)
  # based on sentence under equation 3.7
  di <- stats::residuals(model) / sqrt(sum(stats::residuals(model)^2))
  p <- length(stats::coef(model)) - 1
  result <-(h / (1 - h) + (p + 1) / (1 - h) * di^2 / (1 - di^2))
  result[is.infinite(result)] <- NaN
  return(result)
}
