data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.numeric(Train$choice)
Train$avail_A <- sample(2, replace=TRUE, size=nrow(Train))-1
Train$avail_B <- sample(2, replace=TRUE, size=nrow(Train))-1
extract_av_cols(Train, 'avail_')