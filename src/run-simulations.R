# Run simulations of experiments and analysis
# 
# Author: Matthew
###############################################################################

source("src/generate-data.R")
source("src/frequentist-analysis.R")

memory.limit(8000)

ss = run_simulations(2) %>%
    frequentist_analysis() %>%
    bayesian_analysis()
