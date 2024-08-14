
distances <- function(data, model, plots=TRUE){

  #input validation
  if (!is.data.frame(data)) {
    stop("The input data must be of type data.frame.")
  }

  if (class(model) != "lm") {
    stop("The input model must be an object of class lm")
  }

  if (class(plots) != "logical") {
    stop("The input parameter plots must be a boolean (object of class logical) so FALSE or TRUE")
  }

  cooks<-cooks_distance_lm(model)
  dffits<-dffits_lm(model)
  hadi<-hadi_lm(model)

  if(plots){

  # divide frame in 2X2 grid
  par( mfrow= c(2,2) )
  # draw 4 plots
  plot(cooks,layout = NULL)
  plot(dffits,layout = NULL)
  plot(hadi,layout = NULL)
  }
  return(list(cooks=cooks,dffits=dffits,hadi=hadi))

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
#and https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/lm.influence.R
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

##based on equation 3.7 in Hadi 1992 and 4.9.3 in Regression-Analysis-by-Example
hadi_lm <- function(model) {
  h <- hatvalues(model)
  #based on sentence under equation 3.7
  di <- residuals(model)/sqrt(sum(residuals(model)^2))
  p<-length(coef(model))-1
  return(h/(1-h) + (p+1)/(1-h) * di^2/(1-di^2))

}

