
#' @export
summary.mixl <- function (object,...){
    SIG_FIGS4 <- 4
    SIG_FIGS2 <- 2
    model <- object
  
    finalLL <- sum(model$finalLL)
    zeroLL  <- sum(model$zeroLL)
    initLL  <- sum(model$initLL)
    
    est <- model$estimate
    Nindividuals <- model$Nindividuals
    choicetasks <- model$choicetasks
    
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

    rob_pval0 <- 2*pnorm(-abs(robtrat_0))
    rob_pval1 <- 2*pnorm(-abs(robtrat_1))
    
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
    rob_pval0 <- round(rob_pval0, SIG_FIGS2)
    rob_pval1 <- round(rob_pval1, SIG_FIGS2)
    
    
    coefTable <- as.data.frame(t(rbind(est,se,trat_0,trat_1,robse, robtrat_0,robtrat_1, rob_pval0, rob_pval1)))
    
    ms = list()
    
    ms$message <- model$message
    ms$coefTable <- coefTable
    ms$robvarcov <- robvarcov
    ms$num_params <- num_params
    ms$est <- model$estimate
    ms$Nindividuals <- model$Nindividuals
    ms$choicetasks <- model$choicetasks

    ms$metrics <- list(
      finalLL = finalLL,
      zeroLL  = zeroLL,
      initLL  = initLL,
      
      rho2zero    = 1-finalLL/zeroLL,
      adjrho2zero = 1-(finalLL-num_params)/zeroLL,
      
      AIC  = round(-2*finalLL+2*(num_params),SIG_FIGS2),
      AICc = round(-2*finalLL+2*(num_params)*Nindividuals/(Nindividuals-(num_params)-1),SIG_FIGS2)
     ,BIC  = round(-2*finalLL+(num_params)*log(choicetasks),SIG_FIGS2)    ##TODO: what if choice task numbers vary over participants
    )
    
    class(ms) <- c("summary.mixl")
    
    ms
}

#' @export
print.summary.mixl <- function (model_output) {
    with(model_output, {
      cat("Runtime:", "????? ","\n\n")
      cat("Model diagnosis:", message,"\n\n")
      cat("Number of decision makers:", Nindividuals,"\n")
      cat("Number of observations:", choicetasks,"\n\n")
      cat("Number of draws for random component:", nDraws,"\n\n")
      
      cat("LL(null): ", metrics$zeroLL,"\n")
      cat("LL(init): ", metrics$initLL,"\n")
      cat("LL(final): ", metrics$finalLL,"\n")
      cat("Rho2: ", metrics$rho2zero,"\n")
      
      cat("Estimated parameters: ",num_params,"\n\n")
      
      cat("Estimates:\n")
      print(coefTable)
      
   #   cat("\n\nRobust covariance matrix:\n")
   #   print(robvarcov)
    
    })
}


