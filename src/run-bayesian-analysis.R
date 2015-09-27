# Run the bayesian analysis on the saved simulations data (from run-simulations.R) 
# 
# Author: mjskay
###############################################################################

source("src/functions/bayesian-analysis.R")

memory.limit(8000)

n_participants = 20

load(paste0("output/n", n_participants, "/simulations.RData"))

bayes_effects = bayesian_analysis(simulations)

save("bayes_effects", file=paste0("output/n", n_participants, "/bayes_effects.RData"))
