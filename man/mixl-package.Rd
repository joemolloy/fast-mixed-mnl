\name{mixl-package}
\alias{mixl-package}
\alias{mixl}
\docType{package}
\title{
  Estimate mixed multinomial logit models
}
\description{
  Estimate mixed multinomial logit models using (simulated) maximum likelihood estimation. 
  The package supports standard mnl, mixed-logit and hybrid choice. Using compilation to C++,
  model estimation is significantly faster than in native R code.
}
\details{
  This section should provide a more detailed overview of how to use the
  package, including the most important functions.
}
\author{
Joe Molloy <joseph.molloy@ivt.baug.ethz.ch>.
}
\references{
  Molloy, J., Becker, F., Schmid, B., & Axhausen, K. W. (2021). mixl: An open-source R package for estimating complex choice models on large datasets. Journal of choice modelling, 39, 100284.
}
\keyword{ Multinomial logit MNL GPU }
\examples{
  \donttest{
  
  data("Train", package="mlogit")
  head(Train, 3)
  Train$ID <- Train$id
  Train$CHOICE <- as.numeric(Train$choice)

  mnl_test <- "
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;

    U_A =             @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
    U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  model_spec <- mixl::specify_model(mnl_test, Train)
  
  #only take starting values that are needed
  est <- stats::setNames(c(0,0,0,0,0,0), c("B_price", "B_time", "B_timeB", 
  "B_change", "ASC_B", "SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, model_spec$num_utility_functions)
  
  model <- mixl::estimate(model_spec, est, Train, availabilities = availabilities, nDraws = 20)  
  
  summary(model)
  }
}
