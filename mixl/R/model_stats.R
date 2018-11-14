
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

process_maxlik_output <- function(maxLik.model, data, Nindividuals, Ndraws) {
  class(maxLik.model) <- append(class(maxLik.model), "mixl") #TODO: oder preprend?
  
#  runtime     = "????"
#  choicetasks = choicetasks,
#  Nindividuals= Nindividuals,
#  Ndraws      = Ndraws,
  
  
  return (maxLik.model)
}

#mixl class:
#  model, data, Nindividuals, Ndraws

summary.mixl <- function (object,...){
  SIG_FIGS4 <- 4
  SIG_FIGS2 <- 4
  
    model <- bb
    output <- list()
  
    est <- model$estimate
    varcov <- vcov(model)
    se=sqrt(diag(varcov))
    se[model$fixed] <- NA
    
    varcov[,model$fixed] <- NA
    varcov[model$fixed,] <- NA
    corrmat <- varcov/(se%*%t(se))
    
    trat_0 <- est/se
    trat_1 <- (est-1)/se
    
    robvarcov <- sandwich::sandwich(bb)
    robvarcov[model$fixed,] <- NA
    robvarcov[,model$fixed] <- NA
    
    robse <- sqrt(diag(robvarcov))
    robtrat_0 <- est/robse
    robtrat_1 <- (est-1)/robse
    robcorrmat <- robvarcov/(robse%*%t(robse))
    
    iterations <- model$iterations
    zeroLL <- sum(ll2(0*beta))
    initLL <- sum(ll2(beta))
    finalLL <- sum(ll2(est))
    
    num_params <- length(beta)-sum(model$fixed)
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
    
    object$coefTable <- coefTable
    object$robvarcov <- robvarcov
    
      run_summary = list(
        runtime     = "????",
        choicetasks = choicetasks,
        Nindividuals= Nindividuals,
        Ndraws      = Ndraws,
        iterations  = model$iterations,
        zeroLL      = zeroLL,
        initLL      = initLL,
        finalLL     = finalLL,
        num_params  = num_params,
        rho2zero    = 1-finalLL/zeroLL,
        adjrho2zero = 1-(finalLL-num_params)/zeroLL,
        
        AIC  = round(-2*finalLL+2*(num_params),SIG_FIGS2),
        AICc = round(-2*finalLL+2*(num_params)*Nindividuals/(Nindividuals-(num_params)-1),SIG_FIGS2),
        BIC  = round(-2*finalLL+(num_params)*log(choicetasks),SIG_FIGS2)
      )
    
    
    model_stats
    class(object) <- c("summary.mlogit", "mlogit")
}

print_output <- function (model_output) {
    with(model_output, {
      cat("Runtime:", run_summary$runtime,"\n\n")
      cat("Model diagnosis:",model$message,"\n\n")
      cat("Number of decision makers:",run_summary$Nindividuals,"\n")
      cat("Number of observations:",run_summary$choicetasks,"\n\n")
      cat("Number of draws for random component:",run_summary$Ndraws,"\n\n")
      
      cat("LL(null): ",run_summary$zeroLL,"\n")
      cat("LL(final): ",run_summary$finalLL,"\n")
      cat("Rho2: ",run_summary$rho2zero,"\n")
      
      cat("Estimated parameters: ",run_summary$num_params,"\n\n")
      
      cat("Estimates:\n")
      print(output)
      
      cat("\n\nRobust covariance matrix:\n")
      print(robvarcov)
    
    })
}


