simulateDataPow <-
function(effect,
         variable=c("dog.presence","fence.presence"),
         proba=c("approach","attack"), hotspotwolf, mcmcFinal,
         constWf)
{
    mo <- do.call(rbind, mcmcFinal)
    mo <- mo[,-c(grep("yeareff",colnames(mo)),grep("obseff",colnames(mo)))]
    param <- colMeans(mo)
    proba <- match.arg(proba)
    variable <- match.arg(variable)

    qpr <- as.numeric(proba=="attack")

    ## Assign parameters
    sigmaobs_at <- param["sigmaobs_at"]
    sigmaobs_ap <- param["sigmaobs_ap"]
    sigmaobs_sw <- param["sigmaobs_sw"]
    sigmaobs_sa <- param["sigmaobs_sa"]
    sigmaobs_st <- param["sigmaobs_st"]
    sigmayear_at <- param["sigmayear_at"]
    sigmayear_ap <- param["sigmayear_ap"]
    sigmayear_sw <- param["sigmayear_sw"]
    sigmayear_sa <- param["sigmayear_sa"]
    sigmayear_st <- param["sigmayear_st"]
    a_ap <- param["a_ap"]
    a_at <- param["a_at"]
    a_sw <- param["a_sw"]
    a_sa <- param["a_sa"]
    a_st <- param["a_st"]

    ## Observer Random effects
    eaot <- rnorm(constWf$nobs, 0, sigmaobs_at)
    eaop <- rnorm(constWf$nobs, 0, sigmaobs_ap)
    eaosw <- rnorm(constWf$nobs, 0, sigmaobs_sw)
    eaosa <- rnorm(constWf$nobs, 0, sigmaobs_sa)
    eaost <- rnorm(constWf$nobs, 0, sigmaobs_st)

    ## Year random effects
    eayt <- rnorm(constWf$nyear, 0, sigmayear_at)
    eayp <- rnorm(constWf$nyear, 0, sigmayear_ap)
    eaysw <- rnorm(constWf$nyear, 0, sigmayear_sw)
    eaysa <- rnorm(constWf$nyear, 0, sigmayear_sa)
    eayst <- rnorm(constWf$nyear, 0, sigmayear_st)

    ## Calculation probas: Warning: the simulated effect is negative
    ## (inverse odds-ratio => log is negative, this is why we used
    ## -effect and not +effect)
    lpapproach <- a_ap+eayp[constWf$year]+eaop[constWf$obs.id]-
        effect*(1-qpr)*hotspotwolf[,variable]
    papproach <- 1/(1+exp(-lpapproach))

    lpattack <- a_at+eayt[constWf$year]+
        eaot[constWf$obs.id]-effect*(qpr)*hotspotwolf[,variable]
    pattack <- 1/(1+exp(-lpattack))

    lpshotwolf <- a_sw+eaysw[constWf$year]+eaosw[constWf$obs.id]
    pshotwolf <- 1/(1+exp(-lpshotwolf))

    lpshotappr <- a_sa+eaysa[constWf$year]+eaosa[constWf$obs.id]
    pshotappr <- 1/(1+exp(-lpshotappr))

    lpshotatt <- a_st+eayst[constWf$year]+eaost[constWf$obs.id]
    pshotatt <- 1/(1+exp(-lpshotatt))

    shotwolf <- rbinom(length(papproach), 1, pshotwolf)
    approach <- rbinom(length(papproach), 1, (1-shotwolf)*papproach)
    shotappr <- rbinom(length(papproach), 1, (1-shotwolf)*approach*pshotappr)
    attack <- rbinom(length(papproach), 1, (1-shotwolf)*approach*(1-shotappr)*pattack)
    shotatta <- rbinom(length(papproach), 1, (1-shotwolf)*approach*
                                             (1-shotappr)*attack*pshotatt)

    return(data.frame(shotwolf, approach, shotappr, attack, shotatta))
}
