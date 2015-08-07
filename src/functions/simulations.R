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
run_experiment = function(s, i, n, 
        ratings = list(treatment1=s$treatment1_rating),
        response_rates = list(treatment1=s$treatment1_rate),
        log_odds_ratios = list(treatment1=s$treatment1_log_odds_ratio)
    ) {
    rating_participant_intercept = rnorm(n, 0, s$rating_between_subject_sd)
    
    response_rate_participant_intercept = rnorm(n, 0, s$response_rate_between_subject_sd)
    
    odds_ratio_partcipant_intercept = rnorm(n, 0, s$odds_ratio_between_subject_sd)
    
    df = data.frame(
            interface="control", 
            participant=paste0("p", 1:n), 
            rating=rnorm(n, rating_participant_intercept, s$rating_within_subject_sd),
            response_rate=rpois(n, exp(log(10) + response_rate_participant_intercept)),
            completed=as.logical(rbinom(n, 1, inv.logit(logit(.5) + response_rate_participant_intercept)))
        ) %>% rbind(ldply(names(ratings), function(interface) {
            data.frame(
                interface=interface, 
                participant=paste0("p", 1:n), 
                rating=rnorm(n, ratings[[interface]] + rating_participant_intercept, s$rating_within_subject_sd),
                response_rate=rpois(n, exp(log(response_rates[[interface]]) + response_rate_participant_intercept)),
                completed=as.logical(rbinom(n, 1, inv.logit(logit(.5) + log_odds_ratios[[interface]] + odds_ratio_partcipant_intercept)))
            )
        }))
    
    df$experiment = factor(paste0("e", i))
    df
}

#run a simulation: multiple experiments with the given settings
run_simulation = function(
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
) {
    s = list(
        rating_within_subject_sd = rating_within_subject_sd,
        rating_between_subject_sd = rating_between_subject_sd,
        
        response_rate_between_subject_sd = response_rate_between_subject_sd,
        
        treatment1_rating = treatment1_rating,
        treatment2_rating = treatment2_rating,
        
        treatment1_rate = treatment1_rate,
        treatment2_rate = treatment2_rate,
        
        treatment1_log_odds_ratio = treatment1_log_odds_ratio,
        treatment2_log_odds_ratio = treatment2_log_odds_ratio,
        
        odds_ratio_between_subject_sd = odds_ratio_between_subject_sd 
    )
    
    #all observations from all experiments
    s$data = rbind(
        run_experiment(s, 1, 40),
        run_experiment(s, 2, 40),
        run_experiment(s, 3, 40),
        run_experiment(s, 4, 40, 
            ratings = list(
                treatment1 = s$treatment1_rating, 
                treatment2 = s$treatment2_rating
            ),
            response_rates = list(
                treatment1 = s$treatment1_rate,
                treatment2 = s$treatment2_rate
            ),
            log_odds_ratios = list(
                treatment1 = s$treatment1_log_odds_ratio, 
                treatment2 = s$treatment2_log_odds_ratio
            )
        )
    )
    
    s
}

run_simulations = function(simulations = 2, ...) {
    #list of simulations
    experiments = llply(1:simulations, function(s) run_simulation())
    
    #collapse data frames from experiments and save settings
    ss = experiments[[1]]
    ss$data = ldply(1:simulations, function(i) data.frame(
        simulation = paste0("s", i),
        experiments[[i]]$data
    ))

    class(ss) = "simulations"
    ss
}
