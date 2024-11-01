\name{fitModelPow}
\alias{fitModelPow}
\title{
  Fit a Model of the Probability of a Wolf Behaviour Event as a Function of
  a Predictive Variable.
}
\description{
  This function fits the model of the probability of a wolf behaviour
  event (either approach or attack) as a function of a predictive
  variable (either dog.presence or fence.presence).  
}
\usage{
fitModelPow(response, expVar, sid, hotspotwolf, constWf)
}
\arguments{
  \item{response}{
    a character string.  Either \code{"approach"} or \code{"attack"}.
  }
  \item{expVar}{
    a character string.  Either \code{"dog.presence"} or \code{"fence.presence"}
}
  \item{sid}{
    The simulated dataset returned by the function
    \code{simulateDataPow} (see Details), used to fit the model
  }
  \item{hotspotwolf}{
    The original dataset, containing the explanatory variables
    (available in \code{data(hotspotwolf)}).
}
  \item{constWf}{
    The list containing the constants used to fit the final model for
    the wolf behaviour.
}
}
\details{
  The model also estimates the random effects for observers and
  year. This model is fit using the function \code{stan_glmer} from the
  package \code{rstanarm}.  Note that since this function is used in a
  loop that simulates fake datasets with the function
  \code{simulateDataPow}, a problematic situation may occur where the
  response variable takes only 0's or 1's.  In such cases, the function
  \code{stan_glmer} return an error. For such cases, the function fits
  the model with the function \code{jags.model} from the package
  \code{rjags}.
}
\value{
  A vector containing the MCMC samples for the coefficient of the
  predictive variable of interest.
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
  \code{\link{simulateDataPow}} to simulate \code{sid},
  \code{\link{hotspotwolf}} for the original dataset.
}
\examples{

## original dataset
data(hotspotwolf)

## The MCMC fit of the final model
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

\dontrun{

## fit the model and get the MCMC samples for the coefficient
## of dog.presence in the model of the probability of approach
fm <- fitModelPow("approach", "dog.presence", sid,
                  hotspotwolf, constWf)

}

}
\keyword{model}
