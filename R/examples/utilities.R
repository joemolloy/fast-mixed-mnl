\donttest{
  data("Train", package="mlogit")
  Train$ID <- Train$id
  Train$CHOICE <- as.numeric(Train$choice)
  
  est <- stats::setNames(c(1,1,1,1), c("B_price", "B_time", "B_timeB", "B_change"))
  
  availabilities <- mixl::generate_default_availabilities(Train, 2)
  
  Nindividuals <- length(unique(Train$ID))
  
  utility_script <- "
      U_A = @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A;
      U_B = @B_price * $price_B / 1000 + @B_timeB * $time_B / 60 ;
      "
  
  model_spec <- mixl::specify_model(utility_script, Train)
  
  utilities_matrix = mixl::utilities(model_spec, est, Train, availabilities, NULL)
  
  utilities_matrix
}