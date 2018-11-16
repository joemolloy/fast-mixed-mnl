
# 
# result <- structure(
#   list(
#     coefficients  = x$coefficients,
#     logLik        = logLik,
#     gradient      = gradient,
#     hessian       = hessian,
#     est.stat      = x$est.stat,
#     fitted.values = fitted,
#     probabilities = probabilities,
#     linpred       = linpred,
#     indpar        = indpar,
#     residuals     = resid,
#     omega         = Omega,
#     rpar          = rpar,
#     nests         = nests,
#     model         = mf,
#     freq          = freq,
#     formula       = formula,
#     call          = callT),
#   class = 'mlogit'
# ) 
# #result$Mi <- Mi
# #result$alt.lev <- alt.lev
# result


#mixl class:
#  model, data, Nindividuals, Ndraws

#' @export
summary.mixl <- function (object,...){
    SIG_FIGS4 <- 4
    SIG_FIGS2 <- 2
  
    model <- object
    
    finalLL <- model$finalLL
    zeroLL  <- model$zeroLL
    est <- model$estimate
    
    varcov <- vcov(model)
    se=sqrt(diag(varcov))
    se[model$fixed] <- NA
    
    varcov[,model$fixed] <- NA
    varcov[model$fixed,] <- NA
    corrmat <- varcov/(se%*%t(se))
    
    trat_0 <- est/se
    trat_1 <- (est-1)/se
    
    robvarcov <- sandwich::sandwich(model)
    robvarcov[model$fixed,] <- NA
    robvarcov[,model$fixed] <- NA
    
    robse <- sqrt(diag(robvarcov))
    robtrat_0 <- est/robse
    robtrat_1 <- (est-1)/robse
    robcorrmat <- robvarcov/(robse%*%t(robse))
    
    num_params <- length(est)-sum(model$fixed)
    rho2zero <- 1-finalLL/zeroLL
    adjrho2zero <- 1-(finalLL-num_params)/zeroLL
    
    est <- round(est, SIG_FIGS4)
    se <- round(se, SIG_FIGS4)
    trat_0 <- round(trat_0, SIG_FIGS2)
    trat_1 <- round(trat_1, SIG_FIGS2)
    robse <- round(robse, SIG_FIGS4)
    robtrat_0 <- round(robtrat_0, SIG_FIGS2)
    robtrat_1 <- round(robtrat_1, SIG_FIGS2)
    
    coefTable <- t(rbind(est,se,trat_0,trat_1,robse,robtrat_0,robtrat_1))
    
    model$coefTable <- coefTable
    model$robvarcov <- robvarcov
    model$num_params <- num_params

    
    model$metrics <- list(
      rho2zero    = 1-finalLL/zeroLL,
      adjrho2zero = 1-(finalLL-num_params)/zeroLL,
      
      AIC  = round(-2*finalLL+2*(num_params),SIG_FIGS2),
      AICc = round(-2*finalLL+2*(num_params)*Nindividuals/(Nindividuals-(num_params)-1),SIG_FIGS2)
##    ,BIC  = round(-2*finalLL+(num_params)*log(choicetasks),SIG_FIGS2)    ##TODO: what if choice task numbers vary over participants
    )
    
    class(model) <- c("summary.mixl", class(model))
    
    model
}

print.summary.mixl <- function (model_output) {
    with(model_output, {
      cat("Runtime:", "????? ","\n\n")
      cat("Model diagnosis:", message,"\n\n")
      cat("Number of decision makers:", Nindividuals,"\n")
 #     cat("Number of observations:",run_summary$choicetasks,"\n\n")
      cat("Number of draws for random component:", Ndraws,"\n\n")
      
      cat("LL(null): ", zeroLL,"\n")
      cat("LL(init): ", initLL,"\n")
      cat("LL(final): ", finalLL,"\n")
      cat("Rho2: ", metrics$rho2zero,"\n")
      
      cat("Estimated parameters: ",num_params,"\n\n")
      
      cat("Estimates:\n")
      print(coefTable)
      
   #   cat("\n\nRobust covariance matrix:\n")
   #   print(robvarcov)
    
    })
}


