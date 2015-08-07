# Run simulations of experiments and analysis and save results to output/simulations.RData
# 
# Author: Matthew
###############################################################################

source("src/functions/simulations.R")

ss = run_simulations(100)

save("ss", file="output/simulations.RData")
