# Run the bayesian analysis on the saved simulations data (from run-simulations.R) 
# 
# Author: mjskay
###############################################################################

source("src/functions/bayesian-analysis.R")

memory.limit(8000)

load(file="output/simulations.RData")

bayes_effects = bayesian_analysis(ss)

save("bayes_effects", file="output/bayes_effects.RData")
