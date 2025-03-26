


# ------------------------------------------------------------------------------------------------------------#
#
# ------- THE TEXOUT FUNCTION -------
#
# ------------------------------------------------------------------------------------------------------------#

#' Return tex formatted output of a model summary. If an output_file parameter is provided, save the object to that location
#' 
#' The returned object can be passed to `texreg`'s formatting functions (see 'Examples').
#' 
#' @param model_summary A summary of an estimated Model
#' @param output_file Where to save the tex representation
#' 
#' @return Formatted texreg object containing the latex table suitable for a research paper. See \link[texreg]{createTexreg}
#' 
#' @example R/examples/tex.R
#' 
#' @export 
summary_tex=function(model_summary, output_file) {
  if ('mixl' %in% class(model_summary)) {
    m <- summary(model_summary)
  } else if (!"summary.mixl" %in% (model_summary)) {
    stop('Please provide model_summary as either a mixl model output or mixl model sumamry')
  } else {
    m <- model_summary
  }
  
  ct <- m$coefTable
  
  # customize coefficient estimates output
  
  take_robpval1 <- grepl(pattern = "^S_", row.names(ct), fixed=T)

  robpval_print <- ifelse(take_robpval1, ct$rob_pval1, ct$rob_pval0 )
  
  output <- cbind(ct[, c("est", "robse")], robpval_print)
  
  # make custom GOFs
  
  N <- m$Nindividuals
  
  gofs = c("Number of parameters" = m$num_params, 
           "Number of respondents" = N,
         "Number of choice observations"= m$choicetasks,
         
         "Number of draws"= m$nDraws,
         "LL(null)"= sum(m$metrics$zeroLL),
         "LL(init)"= sum(m$metrics$initLL),
         "LL(final)"= sum(m$metrics$finalLL), 
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
  
  if (!missing(output_file)) {
    save(texmod, file=output_file)
  }
  
  return(texmod)
  
  
  

  
}

