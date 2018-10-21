
# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE TEXOUT FUNCTION -------
#
# ------------------------------------------------------------------------------------------------------------#

texout=function(beta, model) {

  #calculations

  est<<-model$estimate
  varcov=vcov(model)
  meat1=meat(model)
  bread1=bread(model)
  meat1[is.na(meat1)]=0
  bread1[is.na(bread1)]=0
  robvarcov=sandwich(model,bread1,meat1)
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
  robvarcov[model$fixed,]=NA
  robvarcov[,model$fixed]=NA
  finalLL=model$maxim
  iterations=model$iterations
  zeroLL=sum(loglike(0*beta))
  initLL=sum(loglike(beta))
  #choiceLL=sum(LLchoice)
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
  rob_pval0 = 2*pnorm(-abs(robtrat_0))
  rob_pval1 = 2*pnorm(-abs(robtrat_1))

  output=t(rbind(est,se,trat_0,trat_1,robse,robtrat_0,rob_pval0,robtrat_1,rob_pval1))

  # customize coefficient estimates output

  take_robpval1 <- grepl(pattern = "SCALE", row.names(output), fixed=T)

  robpval_print <- ifelse(take_robpval1, rob_pval1, rob_pval0 )

  output=t(rbind(est,robse,robpval_print))

  # print and save output tables

  cat("Model diagnosis:",model$message,"\n\n")

  cat("LL: ",finalLL,"\n\n")

  cat("Estimates:\n")
  print(output)

  xtable::xtable(output)

  # make custom GOFs

  gofs=c("# estimated parameters"=length(beta)-length(fixedparams),

         "Number of respondents"= N,
         "Number of choice observations"= choicetasks,

         "Number of draws"= Ndraws,
         "LL(null)"= zeroLL,
         "LL(final)"= finalLL,
         #"LL(choicemodel)"= choiceLL,

         "McFadden R2" = (1-finalLL/zeroLL),

         "AIC"= round(-2*finalLL+2*(length(beta)-length(fixedparams)),2),
         "AICc"= round(-2*finalLL+2*(length(beta)-length(fixedparams))*N/(N-(length(beta)-length(fixedparams))-1),2),
         "BIC"= round(-2*finalLL+(length(beta)-length(fixedparams))*log(choicetasks),2)

  )

  # #Make coefficient ratio table
  #
  # ratios=c('VTTS [CHF/h] CAR' = unname(60 * est['B_TIME_C'] / est['B_COST']) ,
  #          'VTTS [CHF/h] PT' = unname(60 * est['B_TIME_PT'] / est['B_COST'])
  # )

  #make texreg table

  texmod <- texreg::createTexreg(coef.names = names(est),
                         coef = unname(est),
                         se = unname(robse),

                         pvalues = robpval_print,

                         gof.names = c(names(gofs)),
                         gof = c(unname(gofs))

  )


  runlabel <- paste0(modelname,"_", format(Sys.time(), "%Y%m%d_%H%M%S_"))

  save(texmod, file=paste0(runlabel, "texmod.RData"))


  tab1 <- texreg::texreg(texmod, stars = c(0.01,0.05,0.1),
                 caption = modelname, fontsize = "footnotesize",
                 booktabs = T)
  print(tab1)

}

