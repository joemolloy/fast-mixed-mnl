\donttest{
	data("Train", package="mlogit")
	Train$ID <- Train$id
	Train$CHOICE <- as.numeric(Train$choice)
	    
	mnl_test <- "
	U_A = @B_price * $price_A / 1000 + @B_time * $time_A / 60;
	U_B = @asc + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
	"

	model_spec <- mixl::specify_model(mnl_test, Train, disable_multicore=T)

	#only take starting values that are needed
	est <- stats::setNames(c(1, 1,1,1), c("asc", "B_price", "B_time", "B_timeB"))
	availabilities <- mixl::generate_default_availabilities(
		Train, model_spec$num_utility_functions
	)

	model <- mixl::estimate(model_spec, est, Train, availabilities = availabilities)
	probabilities(model)

	#hypothetical scenario where the travel time of option A doubles
	Train$time_A = Train$time_A * 2
	probabilities(model, Train)
}
