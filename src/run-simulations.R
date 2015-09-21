# Run simulations of experiments and analysis and save results to output/simulations.RData
# 
# Author: Matthew
###############################################################################

source("src/functions/simulations.R")

n_participants = 20
simulations = run_simulations(100, n_participants = n_participants)

save("simulations", file=paste0("output/n", n_participants, "/simulations.RData"))
