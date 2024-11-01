\name{simulateDataPow}
\alias{simulateDataPow}
\title{
  Simulate the Effect of Predictive Variable on a Response Variable in
  the Wolf Behaviour Model
}
\description{
  This function simulates a dataset using the MCMC samples of the final
  model of wolf behaviour in depredation hotspot, by setting
  a given effect of a given predictive variable (either dog presence or
  fence presence) on a given response variable (either approach or
  attack. 
}
\usage{
simulateDataPow(effect,
                variable = c("dog.presence", "fence.presence"),
                proba = c("approach", "attack"), hotspotwolf, mcmcFinal,
                constWf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{effect}{
    A numeric value indicating the logarithm of inverse odds-ratio of
    the effect to be simulated (e.g. a value of 3 means that the odds of
    the response variable is divided by 3 when the preventive measure is
    present)
}
  \item{variable}{
    The predictive variable for which the effect is set.
  }
  \item{proba}{
    The response variable
}
  \item{hotspotwolf}{
    The original dataset used to fit the model (i.e., available in
    \code{data(hotspotwolf)}) 
}
\item{mcmcFinal}{
  The MCMC samples of the fit of the final model.
}
  \item{constWf}{
    The list containing the constants used to fit the final model for
    the wolf behaviour.
}
}
\details{
  This function simulates the final model of the wolf behaviour in
  depredation hotspots, i.e. no effect of any predictive variable on any
  response variable (probability of being shot, probability of
  approach, etc.), except for one response variable (either approach or
  attack) which is supposed to be affected by one predictive variable
  (either dog presence or fence presence) with a certain strength
  (controlled by \code{effect}). The model also includes random effects
  for the observer and the year (simulated using the mean standard
  deviation calculated from \code{mcmcFinal}).
}
\value{
  A data.frame with 338 rows (the 338 interventions where the wolf was
  observed) and 5 variables (the 5 response variables).
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2024)
  Modeling the predation sequence of wolves in sheep depredation
  hotspots. In prep. 
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{hotspotwolf}} for the original dataset,
  \code{link{mcmcFinal}} for the MCMC samples drawn during the fit of
  the final model.
}
\examples{

## Original dataset + MCMC fit
data(hotspotwolf)
data(mcmcFinal)

## The constants used to fit the model
constWf <-  list(n=nrow(hotspotwolf),
                 year=hotspotwolf$year-2016,
                 flock.id=as.numeric(factor(hotspotwolf$flock.id)),
                 nyear=max(hotspotwolf$year-2016))
constWf$obs.id <- hotspotwolf$obs.id
constWf$nobs <- max(hotspotwolf$obs.id)
constWf$nflock <- max(constWf$flock.id)

## Simulate an inverse odds-ratio of 3 for the effect of the dog
## presence on the probability of approach
sid <- simulateDataPow(log(3), "dog.presence", "approach", hotspotwolf,
                       mcmcFinal, constWf)

head(sid)


}
\keyword{model}
