# TODO: Add comment
# 
# Author: Matthew
###############################################################################

library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)

rating_within_subject_sd = 1
rating_between_subject_sd = 1

response_rate_between_subject_sd = 1

#run experiment i with n subjects
run_experiment = function(i, n) {
    rating_participant_intercept = rnorm(n, 0, rating_between_subject_sd)
    
    response_rate_participant_intercept = rnorm(n, 0, response_rate_between_subject_sd)
    
    df = data.frame(
            interface="control", 
            participant=paste0("p", 1:n), 
            rating=rnorm(n, rating_participant_intercept, rating_within_subject_sd),
            response_rate=rpois(n, exp(log(10) + response_rate_participant_intercept))
        ) %>%
        rbind(data.frame(
            interface="treatment", 
            participant=paste0("p", 1:n), 
            rating=rnorm(n, 0.5 + rating_participant_intercept, rating_within_subject_sd),
            response_rate=rpois(n, exp(log(15) + response_rate_participant_intercept))
        ))
    
    df$experiment = factor(paste0("e", i))
    df
}

#all observations from all experiments
df = rbind(
    run_experiment(1, 20),
    run_experiment(2, 20),
    run_experiment(3, 20)
#    run_experiment(4, 20),
#    run_experiment(5, 20),
#    run_experiment(6, 20),
#    run_experiment(7, 20),
#    run_experiment(8, 20),
#    run_experiment(9, 20),
#    run_experiment(10, 20),
#    run_experiment(11, 20)
)

#participant-level effects (individual differences in ratings)
participant_effects = ddply(df, ~ experiment + participant, function(df) 
    data.frame(
        rating_diff = df[2,"rating"] - df[1,"rating"],
        response_rate_log_diff = log(df[2,"response_rate"] / df[1,"response_rate"])
    ))
