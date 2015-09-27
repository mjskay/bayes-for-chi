# Functions for running simulations to generate hypothetical experiments
# 
# Author: mjskay
###############################################################################

library(MASS, pos=which(search() == "package:stats"))   #load MASS high up on search path to prevent MASS::select() from having priority
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)       #logit, inv.logit


#run experiment i with n subjects
run_experiment = function(settings, i, n, 
        #log odds ratios between treatments and control for p(completed)
        completed_lor = list(treatment1=settings$treatment1_completed_lor)
    ) {
    odds_ratio_partcipant_intercept = rnorm(n, 0, settings$odds_ratio_between_subject_sd)
    
    df = data.frame(
            interface="control", 
            participant=paste0("p", 1:n), 
            completed=as.logical(rbinom(n, 1, .5))
        ) %>% rbind(ldply(seq_along(completed_lor), function(i) {
            interface = names(completed_lor)[[i]]
            data.frame(
                interface=interface,
                participant=paste0("p", 1:n + n*i), 
                completed=as.logical(rbinom(n, 1, inv.logit(logit(.5) + completed_lor[[interface]])))
            )
        }))
    
    df$experiment = factor(paste0("e", i))
    df
}

#run a simulation: multiple experiments with the given settings
run_simulation = function(...) {
    settings = list(
            n_participants = 100,
            
            treatment1_completed_lor = 0.45,
            treatment2_completed_lor = -0.45,
            
            odds_ratio_between_subject_sd = 1
        ) %>%
        modifyList(list(...))
    
    #all observations from all experiments
    simulation = rbind(
            run_experiment(settings, 1, settings$n_participants),
            run_experiment(settings, 2, settings$n_participants),
            run_experiment(settings, 3, settings$n_participants),
            run_experiment(settings, 4, settings$n_participants, 
                completed_lor = list(
                    treatment1 = settings$treatment1_completed_lor, 
                    treatment2 = settings$treatment2_completed_lor
                )
            )
        )
    attr(simulation, "settings") = settings
    
    simulation
}

run_simulations = function(n_simulations = 2, ...) {
    #list of simulations
    experiments = llply(1:n_simulations, function(s) run_simulation(...))
    
    #collapse data frames from experiments and save settings
    simulations = ldply(1:n_simulations, function(s_i) data.frame(
            simulation = paste0("s", s_i),
            experiments[[s_i]]
        ))
    attr(simulations, "settings") = attr(experiments[[1]], "settings")

    simulations
}
