# ------------------------------------------------------------------------------------------------------------#
#
# ------- CLEANING MEMORY AND LOADING FUNCTIONS -------
#
# ------------------------------------------------------------------------------------------------------------#
#' @export
detachAllData <- function () {

  pos.to.detach <- (1:length(search()))[substring(search(), first = 1, last = 8) != "package:" &
                                          search() != ".GlobalEnv" & search() != "Autoloads" & search() !=
                                          "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
for (i in 1:length(pos.to.detach)) {

  if (length(pos.to.detach) > 0) {
  detach(pos = pos.to.detach[1])
  pos.to.detach <- (1:length(search()))[substring(search(),
  first = 1, last = 8) != "package:" & search() !=
  ".GlobalEnv" & search() != "Autoloads" & search() !=
  "CheckExEnv" & search() != "tools:rstudio" &
  search() != "TempEnv"]

    }
  }
}

#detachAllData()



# ------------------------------------------------------------------------------------------------------------#
#
# ------- MODEL ESTIMATION -------
#
# ------------------------------------------------------------------------------------------------------------#

#return values per individual
#' @export
loglike=function(beta) {
  return (foreach::foreach(i = 1:env$kernel, .combine='c' ) %dopar% {
    individualLoglikelihood(env$data, env$availabilties, env$Nindv,
                            beta,
                            env$draws, env$Ndraws, env$p)
  })
}

#return only final LL value
#' @export
loglike_sum=function(beta) {
  return (foreach::foreach(i = 1:env$kernel,  .combine='+' ) %dopar% {
    loglikelihood(env$data, env$availabilities, env$Nindv,
                  beta,
                  env$draws, env$Ndraws, env$p)
  })
}

#' @export
runmodel=function(beta, fixedparams) {

  model<-maxLik::maxLik(loglike,start=beta,fixed=fixedparams,method="BFGS",print.level=3,iterlim=10000)

  message("hesse_start ",  Sys.time())

  model$hessian <- numDeriv::hessian(func = loglike_sum, x = model$estimate)

  message("hesse_end ",  Sys.time())

  return(model)

}

#execudes the model. Assumes that the data and draws are already prepared.
# the data should be global as a dataframe, with individual id's starting at 1 to n
# the draws must be a matrix of Nindividuals*Ndraws x Ndimensions.

#' @export
execute=function(config, startvalues, fixedparams, data, N, draws, Ndraws) {
  set.seed(config$seed)
  env <<- new.env()


  start <- Sys.time() ########## decide when to start
  start
  env$kernel <- config$number.of.cores
  setUpCluster(env$kernel, data, N, draws, Ndraws)

  env$model = runmodel(beta = startvalues, fixedparams = fixedparams)

  env$runtime <- Sys.time() - start

  runlabel <- paste0(modelname,"_", format(Sys.time(), "%Y%m%d_%H%M%S_"))

  # formatted output
  modeloutput(startvalues, env$model )

  out <- capture.output(env$runtime, modeloutput(startvalues, env$model))
  cat(out,file=paste0(runlabel, "model.txt"),sep="\n",append=TRUE)


  # create latex-output (see also texout source code)

  texout(startvalues, env$model)
  save(est, file = paste0(modelname, "_est.Rdata"))
  save(robvarcov, file = paste0(modelname, "_robvarcov.Rdata"))

  #make sure we stop the cluster at the end
  parallel::stopCluster(env$cl)

  return (env$model)

}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE OUTSHEET FUNCTION -------
#
# ------------------------------------------------------------------------------------------------------------#

modeloutput=function(beta, model) {

  if (functionality!=1) {
    print("There is nothing to estimate!")
  }

  else {
   est<<-model$estimate
   varcov=vcov(model)
   meat1=sandwich::meat(model)
   bread1=sandwich::bread(model)
   meat1[is.na(meat1)]=0
   bread1[is.na(bread1)]=0
   robvarcov<-sandwich::sandwich(model,bread1,meat1)
   se=sqrt(diag(varcov))
   robse=sqrt(diag(robvarcov))
   trat_0=est/se
   robtrat_0=est/robse
   trat_1=(est-1)/se
   robtrat_1=(est-1)/robse
   se[model$fixed]=NA
   robse[model$fixed]=NA
   trat_0[model$fixed]=NA
   robtrat_0[model$fixed]=NA
   trat_1[model$fixed]=NA
   robtrat_1[model$fixed]=NA
   varcov[model$fixed,]=NA
   varcov[,model$fixed]=NA
   robvarcov[model$fixed,]<-NA
   robvarcov[,model$fixed]<-NA
   corrmat=varcov/(se%*%t(se))
   robcorrmat=robvarcov/(robse%*%t(robse))
   iterations=model$iterations
   zeroLL=sum(loglike(0*beta))
   initLL=sum(loglike(beta))
   finalLL=sum(loglike(est))
   params=length(beta)-sum(model$fixed)
   rho2zero=1-finalLL/zeroLL
   adjrho2zero=1-(finalLL-params)/zeroLL
   est=round(est,4)
   se=round(se,4)
   trat_0=round(trat_0,2)
   trat_1=round(trat_1,2)
   robse=round(robse,4)
   robtrat_0=round(robtrat_0,2)
   robtrat_1=round(robtrat_1,2)
   output=t(rbind(est,se,trat_0,trat_1,robse,robtrat_0,robtrat_1))

   cat("Runtime:", env$runtime,"\n\n")
   cat("Model diagnosis:",model$message,"\n\n")
   cat("Number of decision makers:",N,"\n")
   cat("Number of observations:",choicetasks,"\n\n")
   cat("Number of draws for random component:",Ndraws,"\n\n")

   cat("LL(null): ",zeroLL,"\n")
   cat("LL(final): ",finalLL,"\n")
   cat("Rho2: ",rho2zero,"\n")

   cat("Estimated parameters: ",length(beta)-length(fixedparams),"\n\n")

   cat("AIC: ",round(-2*finalLL+2*(length(beta)-length(fixedparams)),2),"\n")
   cat("AICc: ",round(-2*finalLL+2*(length(beta)-length(fixedparams))*N/(N-(length(beta)-length(fixedparams))-1),2),"\n")
   cat("BIC: ",round(-2*finalLL+(length(beta)-length(fixedparams))*log(choicetasks),2),"\n\n")

   cat("Estimates:\n")
   print(output)

   varcov=signif(varcov,4)
   robvarcov<<-signif(robvarcov,4)

  # cat("\n\nCovariance matrix:\n")
  # print(varcov)

  # cat("\n\nCorrelation matrix:\n")
  # print(corrmat)

  cat("\n\nRobust covariance matrix:\n")
  print(robvarcov)

  # cat("\n\nRobust Correlation matrix:\n")
  # print(robcorrmat)

  }
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- DRAW LIBRARY -------
#
# ------------------------------------------------------------------------------------------------------------#

shuffle=function(inv){
  out=inv[rank(runif(length(inv)))];
  out}

# MLHS

mlhs=function(N,d,i){
  temp=seq(0,N-1)/N;
  out=matrix(0,N*i,d);
  j=1;
  k=1;
  while(j<i+1){
    k=1;
    while(k<d+1){
      out[(1+N*(j-1)):(N*j),k]=shuffle(temp+runif(1)/N);
      k=k+1}
    j=j+1}
  out}

# HALTON

halton=function(n,d){
  prime=2;
  out=(haltonsequence(prime,n));
  i=2;
  while(i<d+1){
    k=0
    while(k<1){
      prime=prime+1;
      if(sum(prime/1:prime==prime%/%1:prime)==2) k=1;
    }

    out=cbind(out,haltonsequence(prime,n));
    i=i+1}
  out}

haltonelement=function(prime,element){
  H=0;
  power=(1/prime);
  while(element>0){
    digit=(element%%prime);
    H=H+digit*power;
    element=element%/%prime;
    power=power/prime}
  H}

haltonsequence=function(prime,lengthvec){
  i=1;
  out=0;
  while(i<lengthvec+1){
    out[i]=haltonelement(prime,i);
    i=i+1}
  out}



