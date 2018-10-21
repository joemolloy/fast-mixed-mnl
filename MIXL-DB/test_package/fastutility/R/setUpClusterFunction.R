
# ------------------------------------------------------------------------------------------------------------#
#
# ------- OTHER SPECIFICATIONS AND DATA SPLIT FOR PARALLEL COMPUTING -------
#
# ------------------------------------------------------------------------------------------------------------#

#env <<- new.env()

#' @export
allocatePartition <- function(partition) {
  env <<- new.env(parent=.GlobalEnv)
  env$data <-  partition$data # make sure these are really saved
  env$draws <- partition$draws
  env$Nindv  <- max(env$data$row_id)+1 #tell each cluster how big p should be
  env$Ndraws <- Ndraws
  env$p  <- matrix(0, nrow=env$Nindv, ncol=env$Ndraws) #pre allocate temp matrix for log likelihood calculations
}

#' @export
setUpCluster <- function(kernel, data, availabilities, Nindv, draws, Ndraws) {
  #clusterEnv <- createClusterEnvironment()
  ####split up data for cores
  rows.per.cluster <- plyr::round_any(Nindv, kernel, f=ceiling) #max number of rows per cluster
  partitions <- unlist(lapply(1:kernel, function(i) {rep(i, rows.per.cluster/kernel)})) #assign Id's to cores
  data[, parallel_id := partitions[data$ID]]
  data[, row_id := ID - min(ID), by = parallel_id] #calculate row_id on each split dataset

  df.split <- split(data, data$parallel_id)

  if (kernel > 1) { #if parallel
    #create cluster
    env$cl <- parallel::makeCluster(kernel)
    doParallel::registerDoParallel(env$cl)
    parallel::clusterEvalQ(env$cl, library(fastutility))

    #split the draws between cores
    num_records.list <- lapply(1:kernel, function(k) { max(df.split[[k]]$row_id)+1} ) #get number of ID's allocated to each core
    draws.cumsum.list <- c(0, cumsum(num_records.list))*Ndraws #calculate where to split the draws matrix
    draws.split <- lapply(1:(Ndraws-1), function(i) {draws[(draws.cumsum.list[i]+1):draws.cumsum.list[i+1],]})

    #group data and draws by core number
    par_partitions = purrr::transpose(list(data=df.split, draws = draws.split))

    #parallel::clusterExport(env$cl, "Ndraws", envir = )

    #allocate partitions to each core
    parallel::clusterApply (env$cl, par_partitions, allocatePartition)


  } else { #for single core processing

    env$data.cluster <- data
    env$availabilities <- availabilities
    env$draws.cluster <- draws
    env$Nindv  <- Nindv
    env$Ndraws <- Ndraws
    env$p  <- matrix(0, nrow=env$Nindv, ncol=env$Ndraws)

  }
}

