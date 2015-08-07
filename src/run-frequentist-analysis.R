# Run the frequentist analysis on the saved simulations data (from run-simulations.R) 
# 
# Author: mjskay
###############################################################################

source("src/functions/frequentist-analysis.R")

memory.limit(8000)

load(file="output/simulations.RData")

freq_effects = frequentist_analysis(ss)

save("freq_effects", file="output/freq_effects.RData")
