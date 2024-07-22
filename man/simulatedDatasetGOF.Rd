\name{simulatedDatasetGOF}
\alias{simulatedDatasetGOF}
\docType{data}
\title{
  Simulated Wolf Behaviour Dataset from the Final Model in Simon et
  al. (in prep.) to Assess its Goodness of Fit
}
\description{
  This dataset was obtained by using the function
  \code{simulateWolfDataGof}. It contains the different response
  variables of our model (shotwolf, approach, shotappr, attack, etc.)
  simulated using the MCMC samples of our final model Fit, to assess its
  goodness of fit. 
}
\usage{data("simulatedDatasetGOF")}
\format{
  A data.frame with 5 response variables and 3000 rows corresponding to
  the 3000 MCMC samples in the dataset \code{mcmcFinal}
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2023) Wolves on
  the hunt in sheep depredation hotspots in France. In prep.
}
\examples{
## This dataset was obtained with the following code.

## Load the dataset used to fit the model
data(hotspotwolf)

## Load the data from the final model
data(mcmcFinal)

## The constants used in the model fit
constWf <-  list(n=nrow(hotspotwolf),
                 year=hotspotwolf$year-2016,
                 flock.id=as.numeric(factor(hotspotwolf$flock.id)),
                 nyear=max(hotspotwolf$year-2016))
constWf$obs.id <- hotspotwolf$obs.id
constWf$nobs <- max(hotspotwolf$obs.id)
constWf$nflock <- max(constWf$flock.id)


\dontrun{

set.seed(777)
simulatedDatasetGOF <- t(sapply(1:3000,  function(x) {
     cat(x,"\r")
        colSums(simulateWolfDataGOF(x, mcmcFinal, constWf))
}))

## The results are in the dataset simulatedWolfDatasetGOF
}

}
\keyword{datasets}
