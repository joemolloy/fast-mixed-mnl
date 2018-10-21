library(fastutility)

config <- list(
  startingkit = 0,
  rnd.dimensions = 2,
  Ndraws = 5,
  number.of.cores = 4, # if cores == 1, then it will be run on a single machine
  modelname=paste0("mixl_train"),
  seed=31415926
)

# ------------------------------------------------------------------------------------------------------------#
#
# ------- DEFINE MODELPARAMETERS -------
#
# ------------------------------------------------------------------------------------------------------------#

parnames =c('ASC_A',
            'ASC_U',

            'B_NAH_VERK_A',
            'B_NAH_VERK_U',

            'B_FERN_VERK_A',
            'B_FERN_VERK_U',

            'B_BAU_VERK_A',
            'B_BAU_VERK_U',

            'S11',
            'S22'

)
startvalues=c(rep(0,length(parnames)))

names(startvalues)<-parnames

# set fixed parameters
fixedparams=c()


# consistency check
if (length(names(startvalues))-length(startvalues) != 0) {
  print("Error: Number of start values must match number of parameters")
  stop()
}

fastutility::execute(config, startvalues, c(), data, N, draws, Ndraws)

