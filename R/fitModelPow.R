fitModelPow <-
function(response, expVar, sid, hotspotwolf, constWf)
{
    qpro <- response
    sid$variable <- hotspotwolf[,expVar]
    sid$yearf <- factor(constWf$year)
    sid$obsf <- factor(constWf$obs.id)


    ## If approach, focus only when wolf has not been shot before
    if (qpro=="approach")
        sidi <- sid[sid$shotwolf==0,]
    ## Safe for attack
    if (qpro=="attack")
        sidi <- sid[sid$shotwolf==0&sid$approach==1&sid$shotappr==0,]
    sidi$reponse <- sidi[,qpro]

    ## If only 1 or 0 in the response, switch on JAGS (rstanarm does
    ## not work in such cases):
    if (all(sidi$reponse==0)|all(sidi$reponse==1)) {
        modelSimPowJags <- "model {

  a~dnorm(0,0.16)
  b~dnorm(0,0.16)
  siga~dgamma(0.01,0.01)
  sigo~dgamma(0.01,0.01)

  for (j in 1:Na) {
     ea[j]~dnorm(0,siga)
  }
  for (j in 1:No) {
     eo[j]~dnorm(0,sigo)
  }
  for (i in 1:N) {
    lpro[i] <- a+b*variable[i]+ea[yearf[i]]+eo[obsf[i]]
    pro[i] <- 1/(1+exp(-lpro[i]))
    reponse[i]~dbern(pro[i])
  }

}"
        dasi <- list(reponse=sidi$reponse,
                     variable=sidi$variable-mean(sidi$variable),
                     yearf=as.numeric(sidi$yearf),
                     obsf=as.numeric(sidi$obsf),
                     N=nrow(sidi), No=max(as.numeric(sidi$obsf)),
                     Na=max(as.numeric(sidi$yearf)))
        jm <- rjags::jags.model(textConnection(modelSimPowJags),
                                data=dasi)
        co <- rjags::coda.samples(jm,variable.names="b",n.iter=40000, thin=10)
        resu <- unlist(unclass(co))
    } else {
        ## Sinon, rstanarm c'est plus simple et plus rapide.
        mc <- rstanarm::stan_glmer(reponse~1+variable+(1|yearf)+(1|obsf),
                                   family="binomial", data=sidi)
        ma <- as.matrix(mc)
        resu <- ma[,"variable"]
    }
    return(resu)
}
