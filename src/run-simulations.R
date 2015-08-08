# Run simulations of experiments and analysis and save results to output/simulations.RData
# 
# Author: Matthew
###############################################################################

source("src/functions/simulations.R")

simulations = run_simulations(100)

save("simulations", file="output/simulations.RData")
