\name{resultsPA}
\alias{resultsPA}
\docType{data}
\title{
  Results of the Bayesian Power Analysis in the Wolf Behaviour Study
}
\description{
  This dataset contains the results of the power analysis carried out by
  Simon et al. (in prep., see details).
}
\usage{data("resultsPA")}
\format{
  This dataset is a list with one element per tested effect: the first
  element contains the result of the power analysis for the effect of
  dog presence, whereas the second element contains the result of the
  power analysis for the effect of the fence presence.\cr

  Each element of the list is a matrix with two columns: the first
  column corresponds to the result of the power analysis for the
  approach behaviour whereas the second elements contains the result of
  the analysis for the attack behaviour.\cr

  Each row of the matrix correspond to a value of inverse odds-ratio
  measuring the strength of the effect on the target behaviour (the
  tested odds ratios are 1, 2, 3, 5, 10, 30, 50, 100).\cr
  
  For a given target behaviour, effect, and strength, the value
  corresponds to the proportion of 100 simulated datasets for which the
  MCMC fit leads to a 90 percent credible interval different from 0.  
  
}
\details{
  This study was carried out to identify the effect strength that would
  have been necessary to detect an effect of a given preventive measure
  (either dog presence or fence presence) on a given behaviour of the
  wolf (either approach or attack).  The example section illustrates how
  this dataset was obtained.  
}
\references{
  Simon R., Calenge C., Quenette P.Y., Jean N., Drouet-Hoguet N. (2023) Wolves on
  the hunt in sheep depredation hotspots in France. In prep.
}
\examples{

effect <- log(c(1,2,3,5,10,30,50,100))
qprop <- c("approach","attack")
qvarp <- c("dog.presence","fence.presence")

\dontrun{

## Power Analysis
powerAnalysis <- list()
for (iv in 1:length(qvarp)) {
    liqp <- list()
    for (iq in 1:length(qprop)) {
        lief <- list()
        for (ie in 1:length(effect)) {

            varl <- list()
            for (r in 1:100) {

                ## Simulation and fit:
                sid <- simulateDataPow(effect[ie], qvarp[iv],
                                       qprop[iq], hotspotwolf, mcmcFinal)
                re <- fitModelPow(qprop[iq], qvarp[iv], sid, hotspotwolf)
                varl[[r]] <- re
            }
            lief[[ie]] <- do.call(cbind,varl)
        }
        liqp[[iq]] <- lief
        powerAnalysis[[iv]] <- liqp
    }
}

## Calculation of the proportion of credible intervals different from 0
## for each response variable and each predictive variable
resultsPA <- lapply(1:length(powerAnalysis), function(iv) {
    do.call(cbind,lapply(1:length(powerAnalysis[[iv]]),
                         function(iq) {
                      sapply(1:length(powerAnalysis[[iv]][[iq]]),
                             function(ie) {
                          vl <- powerAnalysis[[iv]][[iq]][[ie]]
                          interv <- apply(vl,2,quantile,c(0.05,0.95))
                          pr <- mean(interv[1,]<=0&0<=interv[2,])
                          return(pr)
                      })
                  }))
})

}

data(resultsPA)
resultsPA

## Power for the effect of dog.presence on approach
plot(c(1,2,3,5,10,30,50,100), 1-resultsPA[[1]][,1], type="b",
     xlab="1/odds", ylab="Proportion of 90\% CI not including 0",
     log="x")


}
\keyword{datasets}
