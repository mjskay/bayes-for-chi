# Functions for running simulations to generate hypothetical experiments
# 
# Author: mjskay
###############################################################################

library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(boot)       #logit, inv.logit


#run experiment i with n subjects
run_experiment = function(settings, i, n, 
        ratings = list(treatment1=settings$treatment1_rating),
        response_rates = list(treatment1=settings$treatment1_rate),
        log_odds_ratios = list(treatment1=settings$treatment1_log_odds_ratio)
    ) {
    rating_participant_intercept = rnorm(n, 0, settings$rating_between_subject_sd)
    
    response_rate_participant_intercept = rnorm(n, 0, settings$response_rate_between_subject_sd)
    
    odds_ratio_partcipant_intercept = rnorm(n, 0, settings$odds_ratio_between_subject_sd)
    
    df = data.frame(
            interface="control", 
            participant=paste0("p", 1:n), 
            rating=rnorm(n, rating_participant_intercept, settings$rating_within_subject_sd),
            response_rate=rpois(n, exp(log(10) + response_rate_participant_intercept)),
            completed=as.logical(rbinom(n, 1, inv.logit(logit(.5) + response_rate_participant_intercept)))
        ) %>% rbind(ldply(names(ratings), function(interface) {
            data.frame(
                interface=interface, 
                participant=paste0("p", 1:n), 
                rating=rnorm(n, ratings[[interface]] + rating_participant_intercept, settings$rating_within_subject_sd),
                response_rate=rpois(n, exp(log(response_rates[[interface]]) + response_rate_participant_intercept)),
                completed=as.logical(rbinom(n, 1, inv.logit(logit(.5) + log_odds_ratios[[interface]] + odds_ratio_partcipant_intercept)))
            )
        }))
    
    df$experiment = factor(paste0("e", i))
    df
}

#run a simulation: multiple experiments with the given settings
run_simulation = function(...) {
    settings = list(
            n_participants = 100,
            
            rating_within_subject_sd = 1,
            rating_between_subject_sd = 1,
            
            response_rate_between_subject_sd = 1,
            
            treatment1_rating = 0.3,
            treatment2_rating = 0.6,
            
            treatment1_rate = 15,
            treatment2_rate = 20,
            
            treatment1_log_odds_ratio = 0.3,
            treatment2_log_odds_ratio = -0.3,
            
            odds_ratio_between_subject_sd = 1
        ) %>%
        modifyList(list(...))
    
    #all observations from all experiments
    simulation = rbind(
            run_experiment(settings, 1, settings$n_participants),
            run_experiment(settings, 2, settings$n_participants),
            run_experiment(settings, 3, settings$n_participants),
            run_experiment(settings, 4, settings$n_participants, 
                ratings = list(
                    treatment1 = settings$treatment1_rating, 
                    treatment2 = settings$treatment2_rating
                ),
                response_rates = list(
                    treatment1 = settings$treatment1_rate,
                    treatment2 = settings$treatment2_rate
                ),
                log_odds_ratios = list(
                    treatment1 = settings$treatment1_log_odds_ratio, 
                    treatment2 = settings$treatment2_log_odds_ratio
                )
            )
        )
    attr(simulation, "settings") = settings
    
    simulation
}

run_simulations = function(n_simulations = 2, ...) {
    #list of simulations
    experiments = llply(1:n_simulations, function(s) run_simulation())
    
    #collapse data frames from experiments and save settings
    simulations = ldply(1:n_simulations, function(s_i) data.frame(
            simulation = paste0("s", s_i),
            experiments[[s_i]]
        ))
    attr(simulations, "settings") = attr(experiments[[1]], "settings")

    simulations
}
