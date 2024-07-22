simulateWolfDataGOF <-
function(i, mcmcFinal, constWf)
{
    mo <- do.call(rbind, mcmcFinal)
    mo <- mo[,-c(grep("yeareff",colnames(mo)),grep("obseff",colnames(mo)))]

    ## Assign parameters
    sigmaobs_at <- mo[i,"sigmaobs_at"]
    sigmaobs_ap <- mo[i,"sigmaobs_ap"]
    sigmaobs_sw <- mo[i,"sigmaobs_sw"]
    sigmaobs_sa <- mo[i,"sigmaobs_sa"]
    sigmaobs_st <- mo[i,"sigmaobs_st"]
    sigmayear_at <- mo[i,"sigmayear_at"]
    sigmayear_ap <- mo[i,"sigmayear_ap"]
    sigmayear_sw <- mo[i,"sigmayear_sw"]
    sigmayear_sa <- mo[i,"sigmayear_sa"]
    sigmayear_st <- mo[i,"sigmayear_st"]
    a_ap <- mo[i,"a_ap"]
    a_at <- mo[i,"a_at"]
    a_sw <- mo[i,"a_sw"]
    a_sa <- mo[i,"a_sa"]
    a_st <- mo[i,"a_st"]


    ## Observer random effects
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

    ## Proba approach/attack
    lpapproach <- a_ap+eayp[constWf$year]+eaop[constWf$obs.id]
    papproach <- 1/(1+exp(-lpapproach))

    lpattack <- a_at+eayt[constWf$year]+eaot[constWf$obs.id]
    pattack <- 1/(1+exp(-lpattack))

    lpshotwolf <- a_sw+eaysw[constWf$year]+eaosw[constWf$obs.id]
    pshotwolf <- 1/(1+exp(-lpshotwolf))

    lpshotappr <- a_sa+eaysa[constWf$year]+eaosa[constWf$obs.id]
    pshotappr <- 1/(1+exp(-lpshotappr))

    lpshotatt <- a_st+eayst[constWf$year]+eaost[constWf$obs.id]
    pshotatt <- 1/(1+exp(-lpshotatt))

    ## Simulate Data
    shotwolf <- rbinom(length(papproach), 1, pshotwolf)
    approach <- rbinom(length(papproach), 1, (1-shotwolf)*papproach)
    shotappr <- rbinom(length(papproach), 1, (1-shotwolf)*approach*pshotappr)
    attack <- rbinom(length(papproach), 1, (1-shotwolf)*approach*(1-shotappr)*pattack)
    shotatta <- rbinom(length(papproach), 1, (1-shotwolf)*approach*
                                             (1-shotappr)*attack*pshotatt)

    return(data.frame(shotwolf, approach, shotappr, attack, shotatta))
}
