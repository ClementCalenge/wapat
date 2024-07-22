\name{simulateWolfDataGOF}
\alias{simulateWolfDataGOF}
\title{
  Simulate a Wolf Behaviour Dataset from a Set of MCMC Samples
}
\description{
  This function can be used to simulate fake datasets from a set of MCMC
  samples obtained by fitting a model to the data on wolf behaviour.  The
  model simulated by the function is the final model (without any
  explanatory variable, coded in \code{finalModel}). The resulting
  data.frame contains the different response variables of our model
  (shotwolf, approach, shotappr, attack, etc.) simulated using the MCMC
  samples, to assess the goodness of fit.
}
\usage{
simulateWolfDataGOF(i, mcmcFinal, constWf)
}
\arguments{
  \item{i}{
    The row number used to select the set of parameters in the object \code{mcmcFinal}
  }
  \item{mcmcFinal}{
    The MCMC samples returned by nimble
  }
  \item{constWf}{
    The list of constants used to fit the model.
  }
}
\details{
  This function simulates a fake dataset, using the vector of parameters
  corresponding to the i-th row of the matrix obtained by
  \code{do.call(rbind, mcmcFinal)}.
}
\value{
  A data.frame with the same format as the one stored in the dataset
  \code{hotspotwolf}. 
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2023) Wolves on
  the hunt in sheep depredation hotspots in France. In prep.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr} 
}
\seealso{
  \code{\link{hotspotwolf}}
}
\examples{

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
\keyword{model}
