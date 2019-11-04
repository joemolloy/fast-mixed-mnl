data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.numeric(Train$choice)
#in this case not actually individual data columns 
#an ID column is required here
extract_indiv_data(Train, c('comfort_A', 'comfort_B'))