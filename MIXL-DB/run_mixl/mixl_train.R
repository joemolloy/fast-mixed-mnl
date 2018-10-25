
# clean environment

rm(list = ls())


# ------------------------------------------------------------------------------------------------------------#
#
# ------- GLOBAL SETTINGS -------
#
# ------------------------------------------------------------------------------------------------------------#

# source code

setwd("C:/Projects/MSL/MIXL-DB")
load("C:/Projects/MSL/MIXL-DB/checkpoint.RData")

#source("source_code.R") # estimation/postestimation
#source("texout.R")      # creates formatted latex output
# Note: Use "multitable.R" to combine different model outputs

# overall settings
library(data.table)

startingkit = 0         # set to 1 if using betas from a previous run (if available)
paneldata = 1           # set to 0 for cross-sectional estimation
mixing = 1              # set to 1 for models that include random parameters
# define number of draws:

if (mixing==1) {
  Ndraws <- 5
} else Ndraws <- NA

print (paste0("print number draws...............", Ndraws))
functionality = 1       # set to 1 for estimation, 2 for prediction and 3 for uncond./cond. draws
parallelcomputing = 0   # set to 1 to use multiple cores for estimation
# set number of cores for parallel computing:

if (parallelcomputing==1) {
  kernel <- 4
  registerDoParallel(cores=kernel)
} else kernel <- 1

# set modelname

modelname=paste0("mixl")

# what time is it?

start <- Sys.time()
start

# set seed: PI and it's first decimals, always a good choice ;)

set.seed(31415926)

# ------------------------------------------------------------------------------------------------------------#
#
# ------- LOAD DISCRETE TIME USE DATA AND GET STARTED -------
#
# ------------------------------------------------------------------------------------------------------------#

datafilename="data_pcw_joint.csv"
data <- read.csv(datafilename, stringsAsFactors=F, quote = "\\\"", sep=";", row.names=NULL)
data=data.table(data)

# exclude something?

data <- data[!data$problematic_case == 1,]
#data <- data[!data$person_id == "061P",]

# rename person ID and choice

data=plyr::rename(data, c('num_id' = 'ID'))
data=plyr::rename(data, c('choice' = 'CHOICE'))

# number of individuals and choice sets in the data

N=length(unique(data$ID))
choicetasks=nrow(data)


# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE MODELPARAMETERS -------
#
# ------------------------------------------------------------------------------------------------------------#

head(data)

parnames =c('ASC_W',    # PT = reference
            'ASC_B',
            'ASC_C',
            'ASC_CP',
            'ASC_CS',

            'B_COST',      # costs

            'B_TT_W',   # time
            'B_TT_B',
            'B_TT_C',
            'B_TT_PT',
            'B_TT_CS',
            'B_TT_CP',

            'S_MC',   # Scale parameters
            'S_RCC',
            'S_RCPT',

            'SIGMA_W', # Random parameters
            'SIGMA_B',
            'SIGMA_C',
            'SIGMA_CP',
            'SIGMA_CS',

            'SIGMA_SCALE',

            'SIGMA_TT_W',
            'SIGMA_TT_B',
            'SIGMA_TT_C',
            'SIGMA_TT_PT',
            'SIGMA_TT_CP',
            'SIGMA_TT_CS'

)

# set starting values

startvalues=c(
  0.0507, # ASC
  -1.851,
  -1.5937,
  -1.0789,
  -1.5861,

  -1.2045, # COST

  16.92, # TT
  17.61,
  25.47,
  14.99,
  21.57,
  24.32,

  0.5834, # SCALE MC
  1.7893, # SCALRE RCC
  1.8162,  # SCALRE RCPT

  0.5,
  0.5,
  0.5,
  0.5,
  0.5,

  0.5,
  0.5,
  0.5,
  0.5,
  0.5,
  0.5,

  0.5
)

# consistency check (should be zero): nil should be zero

nil <- length(parnames)-length(startvalues)
nil

length(parnames)

if (nil != 0) {
  print("You have a serious problem!")
  stop()
}

# convert the above into a beta vector with names

beta=startvalues
names(beta)=parnames

# set fixed parameters

fixedparams=c()

# use new starting values (if available)

if (startingkit==1) {

  # load beta vector

  load(paste0(modelname,"_est.Rdata"))

  beta <- est

}


# ------------------------------------------------------------------------------------------------------------#
#
# ------- RANDOM DRAWS -------
#
# ------------------------------------------------------------------------------------------------------------#

if (mixing==1) {

# define dimensions of integral

dimensions=12

# use MLHS draws (proposed by Hess et al., 2006)
Ndraws = 100
N = 10000
#draws=as.matrix(qnorm(mlhs(Ndraws,dimensions,N)))
draws=as.matrix(qnorm(halton(Ndraws*N,dimensions)))
#draws=matrix(runif(N*Ndraws*dimensions),nrow=N*Ndraws,byrow=T)

randtoolbox::halton(10000*100, dim=12)

} # %mixing%

# ------------------------------------------------------------------------------------------------------------#
#
# ------- OTHER SPECIFICATIONS AND DATA SPLIT FOR PARALLEL COMPUTING -------
#
# ------------------------------------------------------------------------------------------------------------#
#
# # parallelization: create a data list, containing the data frames by parallel-id's as entries
# # split data into number of cores, by ID
#
  library(plyr)

   rows.per.cluster <- round_any(N, kernel, f=ceiling)
   partitions <- unlist(lapply(1:kernel, function(i) {rep(i, rows.per.cluster/kernel)}))
   data$parallel_id <- partitions[data$ID]
   data[, p_row_id := ID - min(ID), by = parallel_id]
#
#
#   df.split <- split(data, data$parallel_id)
#
#
# if (kernel > 1) {
#
#   clusterExport(cl, "Ndraws")
#   clusterExport(cl, "draws")
#
#   clusterApply (cl, df.split, function(x) { data1<<- x })
#
#   clusterApply (cl, 1:kernel, function(x) {
#     k <<- x;
#     N <<- max(data1$p_row_id)+1;  #tell each cluster how big p should be
#     p <<- matrix(0, nrow=N, ncol=Ndraws);
#   })
#
# } else {
#   data1 <- data
#   availabilities = as.matrix(data1[,(4+1):(4+15)])
#
# }


# ------------------------------------------------------------------------------------------------------------#
#
# ------- MODEL SPECIFICATION -------
#
# ------- Now moved to c++ file. please edit the utility function there
#
# ------------------------------------------------------------------------------------------------------------#



  #preload data and draws onto each core in the cluster
  #NB: the memory allocated for each core can be reduced by removing unused columns from the data


#  clusterExport(cl, "df.split")
#  clusterExport(cl, "draws.split")



  #backups incase we run on the local session
  p = matrix(0, nrow=N, ncol=Ndraws)
  availabilities = as.matrix(data[,(4+1):(4+15)])

  n <- as.integer(N)
  Ndraws <- as.integer(Ndraws)

loglike=function(beta) {

  LL <-  foreach(i = 1:kernel, .packages = c('data.table', 'fastutility'),  .combine='c' ) %dopar% {

          individualLoglikelihood(data, availabilities, N, beta, draws, Ndraws, p)
    }
    return(LL)
}

#seq_r <- loglike(beta)

#  microbenchmark(loglike(beta))

library(fastutility)

# ------------------------------------------------------------------------------------------------------------#
#
# ------- MODEL ESTIMATION AND OUTPUT -------
#
# ------------------------------------------------------------------------------------------------------------#

functionality=1

# model estimation

runmodel()

#rerun with final beta, but with individual likelihoods


# calculate runtime

runtime <- Sys.time() - start

#make sure we stop the cluster at the end
#stopCluster(cl)

# formatted output

modeloutput(model)

runlabel <- paste0(modelname,"_", format(Sys.time(), "%Y%m%d_%H%M%S_"))

# create txt-output

out <- capture.output(runtime, modeloutput(model))
cat(out,file=paste0(runlabel, "model.txt"),sep="\n",append=TRUE)

# create latex-output (see also texout source code)

texout(model)
save(est, file = paste0(modelname, "_est.Rdata"))
save(robvarcov, file = paste0(modelname, "_robvarcov.Rdata"))

