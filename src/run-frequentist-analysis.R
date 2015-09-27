# Run the frequentist analysis on the saved simulations data (from run-simulations.R) 
# 
# Author: mjskay
###############################################################################

source("src/functions/frequentist-analysis.R")

memory.limit(8000)

n_participants = 20

load(paste0("output/n", n_participants, "/simulations.RData"))

freq_effects = frequentist_analysis(simulations)

save("freq_effects", file=paste0("output/n", n_participants, "/freq_effects.RData"))
