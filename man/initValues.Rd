\name{initValues}
\alias{initValues}
\docType{data}
\title{
  Starting Values for the Complete Model of the Wolf Behaviour
}
\description{
  This list contains a set of starting values for the MCMC fit of the
  complete model of wolf behaviour.
}
\usage{data("initValues")}
\format{
  A list.
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2024)
  Modeling the predation sequence of wolves in sheep depredation
  hotspots. In prep. 
}
\examples{

## This is how it is used in the paper

## load the model code
data(ModelWithVariables)

## load the dataset
data(hotspotwolf)

## load the starting values for the model
data(initValues)

\dontrun{

    ## prepare the dataset for nimble fit
    dataWf <- list(shotwolf=hotspotwolf$shotwolf,
                   approach=hotspotwolf$approach,
                   attack=hotspotwolf$attack,
                   shotappr=hotspotwolf$shotappr,
                   shotatta=hotspotwolf$shotatta,
                   X =cbind(1, hotspotwolf$weather, hotspotwolf$habitat,
                            hotspotwolf$depredation.pressure,
                            hotspotwolf$dog.presence, hotspotwolf$fence.presence,
                            hotspotwolf$dog.presence*hotspotwolf$fence.presence))

    ## Constants in the model
    constWf <-  list(n=nrow(hotspotwolf),
                     year=hotspotwolf$year-2016,
                     flock.id=as.numeric(factor(hotspotwolf$flock.id)),
                     nyear=max(hotspotwolf$year-2016))
    constWf$obs.id <- hotspotwolf$obs.id
    constWf$nobs <- max(hotspotwolf$obs.id)
    constWf$nflock <- max(constWf$flock.id)
    constWf$nvar <- ncol(dataWf$X)


    ## Starts the fit
    set.seed(77)
    initValuesComplete <- initValues

    mcmcComplete <- nimbleMCMC(code = ModelWithVariables, constants = constWf,
                               data = dataWf, thin=400, inits=initValues,
                               nchains = 3, niter = 440000, nburnin=40000,
                               monitors=c("a_at","a_ap","a_sa","a_st","a_sw",
                                          "sigmayear_ap",
                                          "sigmayear_at",
                                          "sigmayear_sw",
                                          "sigmayear_sa",
                                          "sigmayear_st",
                                          "sigmaobs_at",
                                          "sigmaobs_ap",
                                          "sigmaobs_sw",
                                          "sigmaobs_sa",
                                          "sigmaobs_st"),
                               samplesAsCodaMCMC=TRUE,
                               setSeed=77)

    ## The results is in the dataset mcmcComplete
    ## for further details, see the vignette    
}

}
\keyword{datasets}
