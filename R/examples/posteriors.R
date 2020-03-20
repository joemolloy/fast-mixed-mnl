\donttest{
	data("Train", package="mlogit")
	Train$ID <- Train$id
	Train$CHOICE <- as.numeric(Train$choice)
	mnl_test <- "
	      ASC_A_RND = @ASC_A + draw_1 * @SIGMA_A1 + draw_7 * @SIGMA_A2;
	      ASC_B_RND = @ASC_B + draw_2 * @SIGMA_B;

	      U_A = ASC_A_RND + @B_price * $price_A / 1000 
	      		+ @B_time * $time_A / 60 + @B_change * $change_A; 
	      U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
	    "

	#only take starting values that are needed
	est <- stats::setNames(c(-1059.69729,  -181.27796,  -251.78909,  
							 -241.18878,   -86.77386, -173.09451,   
							  291.02618,   142.71793,   332.60909)
	    , c("B_price", "B_time", "B_timeB", "B_change", 
	    	"ASC_A", "ASC_B", "SIGMA_A1", "SIGMA_A2", "SIGMA_B"))

	availabilities <- generate_default_availabilities(Train, 2)

	model_specification <- specify_model(mnl_test, Train, disable_multicore=T)
	model <- estimate(model_specification, est, Train,
			availabilities = availabilities, nDraws = 1)

	posteriors(model)
}