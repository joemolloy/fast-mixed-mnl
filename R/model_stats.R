
#' Create a model summary
#' 
#' [summary()] is an S3 method for the class mixl, which adds metrics of goodness of fit
#' 
#' @param object The mixl output to summarize.
#' @param ... Options to pass to summarize (currently).
#' 
#' @return A summary object for a mixl model
#' 
#' @example R/examples/model_stats.R
#' 
#' @export
summary.mixl <- function (object, ...){
    model <- object
  
    choiceLL <- sum(model$choiceLL)
    finalLL <- sum(model$finalLL)
    zeroLL  <- sum(model$zeroLL)
    initLL  <- sum(model$initLL)
    
    est <- model$estimate
    Nindividuals <- model$Nindividuals
    choicetasks <- model$choicetasks
    
    varcov <- vcov.mixl(model)
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

    rob_pval0 <- 2*stats::pnorm(-abs(robtrat_0))
    rob_pval1 <- 2*stats::pnorm(-abs(robtrat_1))
    
    num_params <- length(est)-sum(model$fixed)
    rho2zero <- 1-finalLL/zeroLL
    adjrho2zero <- 1-(finalLL-num_params)/zeroLL
  
    coefTable <- as.data.frame(t(rbind(est,se,trat_0,trat_1,robse, robtrat_0,robtrat_1, rob_pval0, rob_pval1)))
    
    ms = list()
    ms$runtime <- model$runtime
    ms$message <- model$message
    ms$coefTable <- coefTable
    ms$robvarcov <- robvarcov
    ms$num_params <- num_params
    ms$est <- model$estimate
    ms$Nindividuals <- model$Nindividuals
    ms$choicetasks <- model$choicetasks
    ms$is_mixed <- model$is_mixed
    
    if (model$is_mixed) {
      ms$nDraws <- model$nDraws
    }
    
    ms$metrics <- list(
      choiceLL = choiceLL,
      finalLL = finalLL,
      zeroLL  = zeroLL,
      initLL  = initLL,
      
      rho2zero    = 1-finalLL/zeroLL,
      adjrho2zero = 1-(finalLL-num_params)/zeroLL,
      
      AIC  = -2*finalLL+2*(num_params),
      AICc = -2*finalLL+2*(num_params)*Nindividuals/(Nindividuals-(num_params)-1),
      BIC  = -2*finalLL+(num_params)*log(choicetasks)    ##TODO: what if choice task numbers vary over participants
    )
    
    class(ms) <- c("summary.mixl", class(ms))
    
    ms
}

#' Print a model summary
#' 
#' [print()] is an S3 method for the summary.mixl class, the output of a model plus goodness of fit metrics
#' 
#' @param x The summary to print.
#' @param digits minimum number of significant digits to be used for most numbers.
#' @param ... Options to pass to print.
#' 
#' @example R/examples/model_stats.R
#' 
#' @export
print.summary.mixl <- function (x, digits = max(3L, getOption("digits") - 3L), ...) {
  model_output <- x
  model_output$metrics <- lapply(model_output$metrics, function(x) round(x, digits = digits))
  
  with(model_output, {
    cat("Model diagnosis:", message,"\n")
    cat("Runtime: ", format(unclass(runtime), digits = digits), " ", attr(runtime, "units"), "\n\n", sep = "")
    cat("Number of decision makers:", Nindividuals,"\n")
    cat("Number of observations:", choicetasks,"\n\n")
    if (is_mixed) {
      cat("Number of draws for random component:", nDraws,"\n\n")
    }
    cat("LL(null): ", metrics$zeroLL,"\n")
    cat("LL(init): ", metrics$initLL,"\n")
    cat("LL(final): ", metrics$finalLL,"\n")
    cat("LL(choice): ", metrics$choiceLL,"\n")
    cat("Rho2: ", metrics$rho2zero,"\n")
    cat('\n')
    cat("AIC: ", metrics$AIC,"\n")
    cat("AICc: ", metrics$AICc,"\n")
    cat("BIC: ", metrics$BIC,"\n")
    
    cat("Estimated parameters: ",num_params,"\n\n")
    
    cat("Estimates:\n")
    print(round(coefTable, digits = digits), ...)
    
    #   cat("\n\nRobust covariance matrix:\n")
    #   print(robvarcov)
    
  })
  invisible(x)
}


#' Prints the output of a model
#' 
#' [print()] is an S3 method for the mixl class.
#' It creates a model summary and then prints the result
#' 
#' @param x The model to print
#' @param ... Options to pass to print
#' 
#' @example R/examples/model_stats.R
#' 
#' @export
#' @export
print.mixl <- function (x, ...) {
    model_output <- summary(x)
    print(model_output, ...)
}

#' Calculates the Variance-Covariance Matrix of the mixl summary 
#' 
#' [vcov()] is an S3 method for the summary.mixl class, giving the Variance-Covariance Matrix 
#' 
#' @param object The summary to print.
#' @param eigentol The tolerance value.
#' @param ... Options to pass to print.
#' 
#' @example R/examples/model_stats.R
#' 
#' @export
vcov.mixl <- function (object, eigentol = 1e-12, ...) 
{
  if (!is.null(object$varcovar)) 
    return(object$varcovar)
  activePar <- maxLik::activePar(object)
  numParams <- length(object$estimate)
  
  if (!is.null(hess <- maxLik::hessian(object))) {
    hess <- maxLik::hessian(object)[activePar, activePar, drop = FALSE]
    hessev <- abs(eigen(hess, symmetric = TRUE, only.values = TRUE)$values)
    varcovar <- matrix(0, numParams, numParams)
    rownames(varcovar) <- colnames(varcovar) <- names(object$estimate)
    if (min(hessev) > (eigentol * max(hessev))) {
      varcovar[activePar, activePar] <- solve(-maxLik::hessian(object)[activePar, 
                                                               activePar])
      varcovar <- (varcovar + t(varcovar))/2
    }
    else {
      varcovar[activePar, activePar] <- Inf
    }
    return(varcovar)
  }
  else return(NULL)
}