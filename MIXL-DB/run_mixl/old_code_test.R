
beta1=as.list(beta)
attach(beta1)
# create draws for coefficients

if (mixing==1) {

  data[ , EC_A :=  S11 * draws_sigma_a ]
  data[ , EC_U :=  S22 * draws_sigma_u  ]

} # %mixing==1%

# define utility functions

# # # # # # # # # # #
# # # # # # # # # # #
# # # # # # # # # #


data[ , U1 := ASC_A + B_NAH_VERK_A*train_n + B_FERN_VERK_A*train_f + B_BAU_VERK_A*train_x + EC_A ]

data[ , U2 :=  U2 ] ### = 1

data[ , U3 := ASC_U + B_NAH_VERK_U*train_n + B_FERN_VERK_U*train_f + B_BAU_VERK_U*train_x + EC_U ]

# create a term we subtract from all utilities for numerical reasons

data[U1>700,  U1:=700]
data[U1< -300,U1:=-700]
data[U2>700,  U2:=700]
data[U2< -300,U2:=-700]
data[U3>700,  U3:=700]
data[U3< -700,U3:=-700]

# exponentiate utilities

data[,U1:=exp(U1)]
data[,U3:=exp(U3)]

# calculate probability for chosen alternative for each row in the data table

data[, NUMER:= ( (CHOICE==1)*U1 + (CHOICE==2)*U2+ (CHOICE==3)*U3 ) ]

data[, DENOM:= (  U1 + U2 + U3 ) ]

# calculate probability for chosen alternative for each observation.
# don't forget availability conditions (if available) in the denominator!

data[ , P_choice := NUMER / DENOM ]

data[ , L := exp( sum( log(P_choice) ) ), by = "ID,draws_index" ]
data[ , Lchoice := L ]

detach(beta1)
