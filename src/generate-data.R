# TODO: Add comment
# 
# Author: Matthew
###############################################################################

library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lme4)

rating_within_subject_sd = 1
rating_between_subject_sd = 1

response_rate_between_subject_sd = 1

#run experiment i with n subjects
run_experiment = function(i, n, 
        ratings = list(treatment1=0.5),
        response_rates = list(treatment1=15)
    ) {
    rating_participant_intercept = rnorm(n, 0, rating_between_subject_sd)
    
    response_rate_participant_intercept = rnorm(n, 0, response_rate_between_subject_sd)
    
    df = data.frame(
            interface="control", 
            participant=paste0("p", 1:n), 
            rating=rnorm(n, rating_participant_intercept, rating_within_subject_sd),
            response_rate=rpois(n, exp(log(10) + response_rate_participant_intercept))
        ) %>% rbind(ldply(names(ratings), function(interface) {
            data.frame(
                interface=interface, 
                participant=paste0("p", 1:n), 
                rating=rnorm(n, ratings[[interface]] + rating_participant_intercept, rating_within_subject_sd),
                response_rate=rpois(n, exp(log(response_rates[[interface]]) + response_rate_participant_intercept))
            )
        }))
    
    df$experiment = factor(paste0("e", i))
    df
}

#all observations from all experiments
df = rbind(
    run_experiment(1, 20),
    run_experiment(2, 20),
    run_experiment(3, 20),
    run_experiment(4, 20, 
        ratings=list(treatment1=0.5, treatment2=1),
        response_rates=list(treatment1=15, treatment2=20)
    )
)

#participant-level effects (individual differences in treatment effects versus control)
participant_effects = ldply(levels(df$interface)[-1], function(treatment)
    ddply(filter(df, interface %in% c("control", treatment)), ~ experiment + participant, function(df)
        data.frame(
            interface = treatment, 
            rating_diff = df[2,"rating"] - df[1,"rating"],
            response_rate_log_diff = log(df[2,"response_rate"] / df[1,"response_rate"])
        )
    )) %>%
    na.omit()
