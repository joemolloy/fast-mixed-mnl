


# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE TEXOUT FUNCTION -------
#
# ------------------------------------------------------------------------------------------------------------#

#' Return tex formatted output of a model summary
#' 
#' @param model_summary A summary of an estimated Model
#' 
#' @return Formatted text output suitable for a research paper.
#' 
#' @export 
summary_tex=function(model_summary) {
  m <- model_summary

  ct <- m$coefTable
  
  # customize coefficient estimates output
  
  take_robpval1 <- grepl(pattern = "SCALE", row.names(ct), fixed=T)
  
  robpval_print <- ifelse(take_robpval1, ct$rob_pval1, ct$rob_pval0 )
  
  output <- cbind(ct[, c("est", "robse")], robpval_print)
  
  # print and save output tables
  
  cat("Model diagnosis:",m$message,"\n\n")
  
  cat("LL: ",m$metrics$finalLL,"\n\n")
  
  cat("Estimates:\n")
  print(output)
  
  xtable::xtable(output)
  
  # make custom GOFs
  
  N <- m$Nindividuals
  
  gofs=c("# estimated parameters"= m$num_params,
         
         "Number of respondents"= N,
         "Number of choice observations"= m$choicetasks,
         
         "Number of draws"= m$nDraws,
         "LL(null)"= sum(m$metrics$zeroLL),
         "LL(final)"= sum(m$metrics$finalLL), ###TODO: note that this is the choice LL. Maybe this needs to be changed
         "LL(choicemodel)"= sum(m$metrics$choiceLL),
         
         "McFadden R2" = m$metrics$rho2zero,
         
         "AIC"= m$metrics$AIC,
         "AICc"= m$metrics$AICc,
         "BIC"= m$metrics$BIC
         
  )
  
  # #Make coefficient ratio table
  # 
  # ratios=c('VTTS [CHF/h] CAR' = unname(60 * est['B_TIME_C'] / est['B_COST']) ,
  #          'VTTS [CHF/h] PT' = unname(60 * est['B_TIME_PT'] / est['B_COST'])
  # )
  
  #make texreg table
  
  texmod <- texreg::createTexreg(coef.names = names(m$est),
                         coef = unname(ct$est),
                         se = unname(ct$robse),
                         
                         pvalues = robpval_print,
                         
                         gof.names = c(names(gofs)),
                         gof = c(unname(gofs))
                         
  )
  
  
  runlabel <- paste0(m$model_name,"_", format(Sys.time(), "%Y%m%d_%H%M%S_"))
  
  save(texmod, file=paste0(runlabel, "texmod.RData"))
  
  
  tab1 <- texreg::texreg(texmod, stars = c(0.01,0.05,0.1),
                 caption = m$model_name, fontsize = "footnotesize",
                 booktabs = T)
  print(tab1)
  
}

