## ----setup, include=FALSE, cache=FALSE--------------------
# set global chunk options
library('knitr')
opts_chunk$set(fig.path="wapat-",
               fig.align="center",
               fig.show="hold",
               echo=TRUE,
               results="markup",
               fig.width=10,
               fig.height=10, out.width='\\linewidth',
               out.height='\\linewidth',
               cache=FALSE,
               dev='png',
               concordance=TRUE,
               error=FALSE)
opts_knit$set(aliases = c(h = 'fig.height',
              w = 'fig.width',
              wo='out.width',
              ho='out.height'))
options(replace.assign=TRUE,width=60)
set.seed(9567)


## ----eval=FALSE-------------------------------------------
## ## If devtools is not yet installed, type
## install.packages("devtools")
## 
## ## Install the package badgertub
## devtools::install_github("ClementCalenge/wapat", ref="main")


## ----load-wapat-------------------------------------------
library(wapat)


## ----load-dataset-----------------------------------------
data(hotspotwolf)
str(hotspotwolf)


## ----load-nimble------------------------------------------
library(nimble)


## ----code-nimble-infection-model--------------------------
data(ModelWithVariables)
ModelWithVariables


## ----prepare-dataset--------------------------------------
## List with data
dataWf <- list(shotwolf=hotspotwolf$shotwolf,
               approach=hotspotwolf$approach,
               attack=hotspotwolf$attack,
               shotappr=hotspotwolf$shotappr,
               shotatta=hotspotwolf$shotatta,
               X =cbind(1, hotspotwolf$weather, hotspotwolf$habitat,
                        hotspotwolf$depredation.pressure,
                        hotspotwolf$dog.presence, hotspotwolf$fence.presence,
                        hotspotwolf$dog.presence*hotspotwolf$fence.presence))

## List with constant values
constWf <-  list(n=nrow(hotspotwolf),
                 year=hotspotwolf$year-2016,
                 flock.id=as.numeric(factor(hotspotwolf$flock.id)),
                 nyear=max(hotspotwolf$year-2016))
constWf$obs.id <- hotspotwolf$obs.id
constWf$nobs <- max(hotspotwolf$obs.id)
constWf$nflock <- max(constWf$flock.id)
constWf$nvar <- ncol(dataWf$X)


## ----starting-values-model--------------------------------
data(initValues)
str(initValues)


## ----model-fit, eval=FALSE--------------------------------
## ## For reproducibility
## set.seed(77)
## 
## mcmcComplete <- nimbleMCMC(code = ModelWithVariables, constants = constWf,
##                            data = dataWf, thin=400, inits=initValues,
##                            nchains = 3, niter = 440000, nburnin=40000,
##                            monitors=c("a_at","a_ap","a_sa","a_st","a_sw",
##                                       "sigmayear_ap",
##                                       "sigmayear_at",
##                                       "sigmayear_sw",
##                                       "sigmayear_sa",
##                                       "sigmayear_st",
##                                       "sigmaobs_at",
##                                       "sigmaobs_ap",
##                                       "sigmaobs_sw",
##                                       "sigmaobs_sa",
##                                       "sigmaobs_st"),
##                            samplesAsCodaMCMC=TRUE,
##                            setSeed=77)


## ----load-results-model-fit-------------------------------
data(mcmcComplete)
str(mcmcComplete)


## ----load-coda-silent, echo=FALSE-------------------------
library(coda)


## ----plot-chains, eval=FALSE------------------------------
## library(coda)
## par(ask=TRUE) ## press enter for next set of graphs
## plot(mcmcComplete)


## ----gelman-diag-complete---------------------------------
max(gelman.diag(mcmcComplete, multivariate=FALSE)$psrf[,"Upper C.I."])


## ----complete-model-parameter-estimates-------------------
mo <- do.call(rbind, mcmcComplete)
estim <- apply(mo,2, \(x)
paste0(round(mean(x),2), " (",
       round(quantile(x,0.05),2), ", ",
       round(quantile(x,0.95),2),")"))

estim <- c(estim[1:7], "",estim[8:14])
nom <- c("Intercept", "weather", "habitat", "depredation.pressure",
         "dog.presence", "fence.presence", "interaction dog.fence")
dfo <- data.frame(Name=c(paste0(nom,"_approach"), "",
                        paste0(nom,"_attack")),
                  Estimation=estim)
row.names(dfo) <- 1:nrow(dfo)
print(dfo)


## ----preparation-data-for-model-2-------------------------
## We change the matrix of predictive variable (we remove the
## interactions)
dataWf2 <- dataWf
dataWf2$X <- cbind(1, hotspotwolf$weather, hotspotwolf$habitat,
                   hotspotwolf$depredation.pressure,
                   hotspotwolf$dog.presence,
                   hotspotwolf$fence.presence)

## We change the starting values, removing the starting value for the
## interactions
initsf2 <- initValues
initsf2$a_ap <- initValues$a_ap[-length(initValues$a_ap)]
initsf2$a_at <- initValues$a_at[-length(initValues$a_at)]

## Change the number of variables
constWf$nvar <- ncol(dataWf2$X)


## ----fit-model-2, eval=FALSE------------------------------
## ## For reproducibility
## set.seed(77)
## 
## mcmcAdditive <- nimbleMCMC(code = ModelWithVariables, constants = constWf,
##                            data = dataWf2, thin=400, inits=initsf2,
##                            nchains = 3, niter = 440000, nburnin=40000,
##                            monitors=c("a_at","a_ap","a_sa","a_st","a_sw",
##                                       "sigmayear_ap",
##                                       "sigmayear_at",
##                                       "sigmayear_sw",
##                                       "sigmayear_sa",
##                                       "sigmayear_st",
##                                       "sigmaobs_at",
##                                       "sigmaobs_ap",
##                                       "sigmaobs_sw",
##                                       "sigmaobs_sa",
##                                       "sigmaobs_st"),
##                            samplesAsCodaMCMC=TRUE, setSeed=77)


## ----load-results-additive-model-fit----------------------
data(mcmcAdditive)


## ----plot-chains-2, eval=FALSE----------------------------
## par(ask=TRUE) ## press enter for next set of graphs
## plot(mcmcAdditive)


## ----gelman-diag-additive---------------------------------
max(gelman.diag(mcmcAdditive, multivariate=FALSE)$psrf[,"Upper C.I."])


## ----additive-model-parameter-estimates-------------------
mo <- do.call(rbind, mcmcAdditive)
estim <- apply(mo,2, \(x)
paste0(round(mean(x),2), " (",
       round(quantile(x,0.05),2), ", ",
       round(quantile(x,0.95),2),")"))
estim <- c(estim[1:6], "",estim[7:12])
nom <- c("Intercept", "weather", "habitat",
         "depredation.pressure",
         "dog.presence", "fence.presence")
dfo <- data.frame(Name=c(paste0(nom,"_approach"), "",
                         paste0(nom,"_attack")),
                  Estimation=estim)
row.names(dfo) <- 1:nrow(dfo)
print(dfo)


## ----code-for-final-model---------------------------------
data(finalModel)
finalModel


## ----data-preparation-final-model-------------------------
initsLast <- initValues
initsLast$a_ap <- initsLast$a_ap[1]
initsLast$a_at <- initsLast$a_at[1]

constWf$nvar <- NULL

dataWf$X <- NULL


## ----fit-model-final, eval=FALSE--------------------------
## ## For reproducibility
## set.seed(77)
## mcmcFinal <- nimbleMCMC(code = finalModel, constants = constWf,
##                         data = dataWf, thin=400, inits=initsLast,
##                         nchains = 3, niter = 440000, nburnin=40000,
##                         monitors=c("a_at","a_ap","a_sa","a_st","a_sw",
##                                    "sigmayear_ap",
##                                    "sigmayear_at",
##                                    "sigmayear_sw",
##                                    "sigmayear_sa",
##                                    "sigmayear_st",
##                                    "sigmaobs_at",
##                                    "sigmaobs_ap",
##                                    "sigmaobs_sw",
##                                    "sigmaobs_sa",
##                                    "sigmaobs_st",
##                                    "obseff_at","obseff_ap","obseff_sw",
##                                    "obseff_sa","obseff_st",
##                                    "yeareff_at","yeareff_ap","yeareff_sw",
##                                    "yeareff_sa","yeareff_st"),
##                         samplesAsCodaMCMC=TRUE, setSeed=77)


## ----load-results-final-model-fit-------------------------
data(mcmcFinal)


## ----plot-chains-final, eval=FALSE------------------------
## par(ask=TRUE) ## press enter for next set of graphs
## plot(mcmcFinal)


## ----gelman-diag-final------------------------------------
max(gelman.diag(mcmcFinal, multivariate=FALSE)$psrf)


## ----simulate-goodness-of-fit, eval=FALSE-----------------
## ## For reproducibility
## set.seed(777)
## simulatedDatasetGOF <- t(sapply(1:3000,  function(x) {
##     cat(x,"\r")
##     colSums(simulateWolfDataGOF(x, mcmcFinal, constWf))
## }))


## ----load-simus-gof---------------------------------------
data(simulatedDatasetGOF)


## ---------------------------------------------------------
ap <- paste0("[",apply(round(apply(simulatedDatasetGOF,2, quantile,c(0.05,0.95))),2,
                       paste0,collapse=", "),"]")
ob <- sapply(colnames(simulatedDatasetGOF), function(x) sum(dataWf[[x]]))
data.frame(observed=ob, Expected=ap)


## ----estimated-parameters-of-the-model--------------------
mo <- do.call(rbind, mcmcFinal)
mob <- mo[,-c(grep("yeareff",colnames(mo)),grep("obseff",colnames(mo)))]

## Warning: the model that we fit estimates the probability to have a
## shot after observation, approach and attack.  But we are interested
## in the probability of NO shot. The sign of the intercept must
## therefore be changed:
mob[,c(3:5)] <- -mob[,c(3:5)]

pres <- function(x) {
    paste0(round(mean(x),2), " (",
                  round(quantile(x,0.05),2), ", ",
           round(quantile(x,0.95),2),")")
}
u <- data.frame(Parametre=c("Intercept approach","Intercept attack",
                       "Intercept no shot | approach",
                       "Intercept no shot | attack",
                       "Intercept no shot | observation",
                       "SD obs approach",
                       "SD obs attack",
                       "SD obs no shot | approach",
                       "SD obs no shot | attack",
                       "SD obs no shot | observation",
                       "SD year approach",
                       "SD year attack",
                       "SD year no shot | approach",
                       "SD year no shot | attack",
                       "SD year no shot | observation"),
                Estimation=apply(mob,2, pres))
reord <- c(5,1,3,2,4)
u <- u[c(reord, reord+5, reord+10),]
row.names(u) <- 1:nrow(u)
u


## ----estimated-mean-probability---------------------------
## Estimate mean proba
probability_approach <-  pres(1/(1+exp(-mo[,"a_ap"])))
probability_attack <-  pres(1/(1+exp(-mo[,"a_at"])))
probability_noshot_before_approach <-  pres(1-1/(1+exp(-mo[,"a_sw"])))
probability_noshot_after_approach <-  pres(1-1/(1+exp(-mo[,"a_sa"])))
probability_noshot_after_attack <-  pres(1-1/(1+exp(-mo[,"a_st"])))

data.frame(Probability=c("Not shot before approach","Approach",
                         "Not shot after approach","Attack","Not shot after attack"),
           Estimation =c(probability_noshot_before_approach,
                         probability_approach,
                         probability_noshot_after_approach,
                         probability_attack,
                         probability_noshot_after_attack))


## ----estimated-probablity-with-random-effects-------------
probability_approach2 <-  pres(1/(1+exp(-mo[,"a_ap"]+
                                        rnorm(nrow(mo), 0, mo[,"sigmayear_ap"])+
                                        rnorm(nrow(mo), 0, mo[,"sigmaobs_ap"]))))
probability_attack2 <-  pres(1/(1+exp(-mo[,"a_at"]+
                                     rnorm(nrow(mo), 0, mo[,"sigmayear_at"])+
                                     rnorm(nrow(mo), 0, mo[,"sigmaobs_at"]))))
probability_noshot_before_approach2 <-
    pres(1-1/(1+exp(-mo[,"a_sw"]+
                    rnorm(nrow(mo), 0, mo[,"sigmayear_sw"])+
                    rnorm(nrow(mo), 0, mo[,"sigmaobs_sw"]))))
probability_noshot_after_approach2 <-
    pres(1-1/(1+exp(-mo[,"a_sa"]+
                    rnorm(nrow(mo), 0, mo[,"sigmayear_sa"])+
                    rnorm(nrow(mo), 0, mo[,"sigmaobs_sa"]))))
probability_noshot_after_attack2 <-
    pres(1-1/(1+exp(-mo[,"a_st"]+
                    rnorm(nrow(mo), 0, mo[,"sigmayear_st"])+
                    rnorm(nrow(mo), 0, mo[,"sigmaobs_st"]))))

data.frame(Probability=c("No shot before approach","Approach","No shot after approach",
                         "Attack","No shot after attack"),
           Estimation =c(probability_noshot_before_approach2, probability_approach2,
                         probability_noshot_after_approach2,
                         probability_attack2,probability_noshot_after_attack2))


## ----simulate-power, eval=FALSE---------------------------
## ## The tested effects:
## effect <- log(c(1,2,3,5,10,30,50,100))
## qprop <- c("approach","attack")
## qvarp <- c("dog.presence","fence.presence")
## 
## 
## powerAnalysis <- list()
## 
## ## For each preventive measure
## for (iv in 1:length(qvarp)) {
##     liqp <- list()
## 
##     ## For each response variable (behavior)
##     for (iq in 1:length(qprop)) {
##         lief <- list()
## 
##         ## And for each effect strength:
##         for (ie in 1:length(effect)) {
## 
##             varl <- list()
## 
##             ## We simulate 100 datasets and fit 100 models
##             for (r in 1:100) {
##                 cat("############################################################\n")
##                 cat("############################################################\n")
##                 cat("############################################################\n")
##                 cat("############################################################\n")
##                 cat("############################################################\n")
##                 cat("Rep.",r,",eff=",ie,",qp=",iq,",vp=",iv,"\n")
## 
##                 ## Simulation data:
##                 sid <- simulateDataPow(effect[ie], qvarp[iv], qprop[iq],
##                                        hotspotwolf, mcmcFinal)
##                 re <- fitModelPow(qprop[iq], qvarp[iv], sid, hotspotwolf)
##                 varl[[r]] <- re
##             }
##             ## Stores results
##             lief[[ie]] <- do.call(cbind,varl)
##         }
##         liqp[[iq]] <- lief
##         powerAnalysis[[iv]] <- liqp
##     }
## }
## 
## 
## resultsPA <- lapply(1:length(powerAnalysis), function(iv) {
##     do.call(cbind,lapply(1:length(powerAnalysis[[iv]]),
##                          function(iq) {
##                       sapply(1:length(powerAnalysis[[iv]][[iq]]),
##                              function(ie) {
##                           vl <- powerAnalysis[[iv]][[iq]][[ie]]
##                           interv <- apply(vl,2,quantile,c(0.05,0.95))
##                           pr <- mean(interv[1,]<=0&0<=interv[2,])
##                           return(pr)
##                       })
##                   }))
## })
## 


## ----set-values-of-power-analysis, echo=FALSE-------------
## The tested effects:
effect <- log(c(1,2,3,5,10,30,50,100))
qprop <- c("approach","attack")
qvarp <- c("dog.presence","fence.presence")


## ----load-resultsPA---------------------------------------
data(resultsPA)
str(resultsPA)


## ----plot-power-------------------------------------------
library(ggplot2)

## Prepare the data.frame for graphical display with ggplot2:
effet <- c(1,2,3,5,10,30,50,100)
do <- data.frame(var=rep("dog.presence", 2*length(effet)),
                 effet=c(effet,effet),
                 type=c(rep("approach",length(effet)),
                        rep("attack",length(effet))),
                 pr=c(resultsPA[[1]][,1],resultsPA[[1]][,2]))
do2 <- data.frame(var=rep("fence.presence", 2*length(effet)),
                  effet=c(effet,effet),
                  type=c(rep("approach",length(effet)),
                         rep("attack",length(effet))),
                  pr=c(resultsPA[[2]][,1],resultsPA[[2]][,2]))
do <- rbind(do,do2)
do$var <- c(rep("LGD presence",16), rep("Fence presence", 16))
do$Probability <- do$type

## Show the graph
ggplot(do, aes(x=effet,y=1-pr, col=Probability))+geom_point()+
    geom_line()+facet_wrap(~var)+
    geom_hline(yintercept=0.8)+xlab("1/Odds-ratio")+
    ylab("Probability to identify an effect")+
    scale_x_log10(breaks=c(1,2,3,5,10,30,50, 100))


## ---------------------------------------------------------
re <- lapply(c("LGD presence","Fence presence"), function(var) {
    lap <- lapply(c("approach","attack"), function(type) {
        dt <- do[do$var==var&do$type==type,c("effet","pr")]
        k <- 0
        for (i in 1:(nrow(dt)-1)) {
            if (dt$pr[i]>0.2&dt$pr[i+1]<0.2)
                k <- i
        }
        du <- dt[k:(k+1),]
        return(round(approx(du$pr, du$effet, xout=0.2)$y,1))
    })
    names(lap) <- c("approach","attack")
    return(lap)
})
names(re) <- c("LGD presence","Fence presence")
re

