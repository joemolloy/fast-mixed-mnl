data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.numeric(Train$choice)
generate_default_availabilities(Train, 5)