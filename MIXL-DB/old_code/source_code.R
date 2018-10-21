
# ------------------------------------------------------------------------------------------------------------#
#
# ------- PACKAGES -------
#
# ------------------------------------------------------------------------------------------------------------#

# change local library if needed

localLibrary <- NULL

require(miscTools, lib.loc = localLibrary)
require(zoo, lib.loc = localLibrary)
require(maxLik, lib.loc = localLibrary)
require(data.table, lib.loc = localLibrary)
require(plyr, lib.loc = localLibrary)
require(numDeriv, lib.loc = localLibrary)
require(foreach, lib.loc = localLibrary)
require(doParallel, lib.loc = localLibrary)
require(sandwich, lib.loc = localLibrary)
require(randtoolbox, lib.loc = localLibrary)
require(dplyr, lib.loc = localLibrary)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- CLEANING MEMORY AND LOADING FUNCTIONS -------
#
# ------------------------------------------------------------------------------------------------------------#

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

runmodel=function() {

  model<<-maxLik(loglike,start=beta,fixed=fixedparams,method="BFGS",print.level=4,iterlim=10000)
  loglikelihood_sum <- function(beta_est) sum(loglike(beta_est))

  message("hesse_start ",  Sys.time())

  model$hessian<<-numDeriv::hessian(func = loglikelihood_sum, x = model$estimate)

  message("hesse_end ",  Sys.time())

}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE OUTSHEET FUNCTION -------
#
# ------------------------------------------------------------------------------------------------------------#

modeloutput=function(model) {

  if (functionality!=1) {
    print("There is nothing to estimate!")
  }

  else {
   est<<-model$estimate
   varcov=vcov(model)
   meat1=meat(model)
   bread1=bread(model)
   meat1[is.na(meat1)]=0
   bread1[is.na(bread1)]=0
   robvarcov<<-sandwich(model,bread1,meat1)
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
   robvarcov[model$fixed,]<<-NA
   robvarcov[,model$fixed]<<-NA
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

   cat("Runtime:", runtime,"\n\n")
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


# ------------------------------------------------------------------------------------------------------------#
#
# ------- ORDERED LOGIT -------
#
# ------------------------------------------------------------------------------------------------------------#

# 4 items

ordered_logit_4=function(indic,lv,zeta,t1,t2,t3) {
  prob=(    (exp(t1-zeta*lv)/(1 + exp(t1 - zeta*lv)))  * (indic == 1 )
            + (exp(t2 - zeta*lv)/(1 + exp(t2 - zeta*lv)) - (exp(t1 - zeta*lv) / (1 + exp(t1 - zeta*lv)))) * (indic == 2 )
            + (exp(t3 - zeta*lv)/(1 + exp(t3 - zeta*lv)) - (exp(t2 - zeta*lv) / (1 + exp(t2 - zeta*lv)))) * (indic == 3 )
            + (1-exp(t3 - zeta*lv)/(1 + exp(t3 - zeta*lv))) * (indic == 4 ))
  return(prob)
}

# 5 items

ordered_logit_5=function(indic,lv,zeta,t1,t2,t3,t4) {
  prob=(    (exp(t1-zeta*lv)/(1 + exp(t1 - zeta*lv)))  * (indic == 1 )
            + (exp(t2 - zeta*lv)/(1 + exp(t2 - zeta*lv)) - (exp(t1 - zeta*lv) / (1 + exp(t1 - zeta*lv)))) * (indic == 2 )
            + (exp(t3 - zeta*lv)/(1 + exp(t3 - zeta*lv)) - (exp(t2 - zeta*lv) / (1 + exp(t2 - zeta*lv)))) * (indic == 3 )
            + (exp(t4 - zeta*lv)/(1 + exp(t4 - zeta*lv)) - (exp(t3 - zeta*lv) / (1 + exp(t3 - zeta*lv)))) * (indic == 4 )
            + (1-exp(t4 - zeta*lv)/(1 + exp(t4 - zeta*lv))) * (indic == 5 ))
  return(prob)
}

# 7 items

ordered_logit_7=function(indic,lv,zeta,t1,t2,t3,t4,t5,t6) {
prob=(    (exp(t1-zeta*lv)/(1 + exp(t1 - zeta*lv)))  * (indic == 1 )
        + (exp(t2 - zeta*lv)/(1 + exp(t2 - zeta*lv)) - (exp(t1 - zeta*lv) / (1 + exp(t1 - zeta*lv)))) * (indic == 2 )
        + (exp(t3 - zeta*lv)/(1 + exp(t3 - zeta*lv)) - (exp(t2 - zeta*lv) / (1 + exp(t2 - zeta*lv)))) * (indic == 3 )
        + (exp(t4 - zeta*lv)/(1 + exp(t4 - zeta*lv)) - (exp(t3 - zeta*lv) / (1 + exp(t3 - zeta*lv)))) * (indic == 4 )
        + (exp(t5 - zeta*lv)/(1 + exp(t5 - zeta*lv)) - (exp(t4 - zeta*lv) / (1 + exp(t4 - zeta*lv)))) * (indic == 5 )
        + (exp(t6 - zeta*lv)/(1 + exp(t6 - zeta*lv)) - (exp(t5 - zeta*lv) / (1 + exp(t5 - zeta*lv)))) * (indic == 6 )
        + (1-exp(t6 - zeta*lv)/(1 + exp(t6 - zeta*lv))) * (indic == 7 ))
return(prob)
}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- DELTA METHOD -------
#
# ------------------------------------------------------------------------------------------------------------#

deltamethod=function(par1,par2)
{
  #
  v1=est[par1]+est[par2]
  se1=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]+2*robvarcov[par1,par2])
  t1=round(v1/se1,2)
  #
  v2=est[par1]-est[par2]
  se2=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]-2*robvarcov[par1,par2])
  t2=round(v2/se2,2)
  #
  # v3=est[par2]-est[par1]
  # se3=se2
  # t3=-t2
  #
  v4=est[par1]/est[par2]*60
  se4=sqrt(v4^2*(robvarcov[par1,par1]/(est[par1]^2)+robvarcov[par2,par2]/(est[par2]^2)-2*robvarcov[par1,par2]/(est[par1]*est[par2])))
  t4=round(v4/se4,2)
  #
  # v5=est[par2]/est[par1]
  # se5=sqrt(v5^2*(robvarcov[par1,par1]/(est[par1]^2)+robvarcov[par2,par2]/(est[par2]^2)-2*robvarcov[par1,par2]/(est[par1]*est[par2])))
  # t5=round(v5/se5,2)
  #
  function_value=round(c(v1,v2,v4),4)
  function_se=round(c(se1,se2,se4),4)
  function_t=c(t1,t2,t4)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1,"+",par2),collapse=""),paste(cbind(par1,"-",par2),collapse=""),paste(cbind(par1,"/",par2),collapse=""))
  #
  return(delta_output)
}

zweierdelta=function(par1,par2,par3)
{
  #
  v1=est[par1]+est[par2]
  var1=robvarcov[par1,par1] + robvarcov[par2,par2] +
    2*robvarcov[par1,par2]
  cov1 = robvarcov[par1,par3] + robvarcov[par2,par3]

  vtts = v1/est[par3]*60
  se_vtts = sqrt(vtts^2*(var1/(v1^2)+robvarcov[par3,par3]/(est[par3]^2)-2*cov1/(v1*est[par3])))
  t_vtts = round(vtts/se_vtts,2)

  #
  function_value=round(c(vtts),4)
  function_se=round(c(se_vtts),4)
  function_t=c(t_vtts)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1," plus urban or purpose TT / ",par3),collapse=""))
  #
  return(delta_output)
}


superdelta=function(par1,par2,par3,par4)
{
  #
  v1=est[par1]+est[par2]+est[par3]
  var1=robvarcov[par1,par1] + robvarcov[par2,par2] + robvarcov[par3,par3] +
       2*robvarcov[par1,par2] + 2*robvarcov[par1,par3] + 2*robvarcov[par2,par3]
  cov1 = robvarcov[par1,par4] + robvarcov[par2,par4] + robvarcov[par3,par4]

  vtts = v1/est[par4]*60
  se_vtts = sqrt(vtts^2*(var1/(v1^2)+robvarcov[par4,par4]/(est[par4]^2)-2*cov1/(v1*est[par4])))
  t_vtts = round(vtts/se_vtts,2)

  #
  function_value=round(c(vtts),4)
  function_se=round(c(se_vtts),4)
  function_t=c(t_vtts)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1," plus urban TT plus purpose TT / ",par4),collapse=""))
  #
  return(delta_output)
}

mixldelta=function(par1,par2,par3)
{
  #
  v1=est[par1]+est[par2]+est[par3]
  var1=robvarcov[par1,par1] + robvarcov[par2,par2] + robvarcov[par3,par3] +
    2*robvarcov[par1,par2] + 2*robvarcov[par1,par3] + 2*robvarcov[par2,par3]
  cov1 = robvarcov[par1,par4] + robvarcov[par2,par4] + robvarcov[par3,par4]

  vtts = v1/est[par4]*60
  se_vtts = sqrt(vtts^2*(var1/(v1^2)+robvarcov[par4,par4]/(est[par4]^2)-2*cov1/(v1*est[par4])))
  t_vtts = round(vtts/se_vtts,2)

  #
  function_value=round(c(vtts),4)
  function_se=round(c(se_vtts),4)
  function_t=c(t_vtts)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1," plus urban TT plus purpose TT / ",par4),collapse=""))
  #
  return(delta_output)
}

