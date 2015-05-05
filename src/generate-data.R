# TODO: Add comment
# 
# Author: Matthew
###############################################################################

library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)

within_subject_sd = 1
between_subject_sd = 1

#run experiment i with n subjects
run_experiment = function(i, n) within(list(), {
    participant_intercept = rnorm(n, 0, between_subject_sd)
            
    df = data.frame(interface="control", participant=paste0("p", 1:n), rating=rnorm(n, participant_intercept, within_subject_sd)) %>%
            rbind(data.frame(interface="treatment", participant=paste0("p", 1:n), rating=rnorm(n, 0.5 + participant_intercept, within_subject_sd)))
    
    df$experiment = factor(paste0("e", i))
        
    m = lmer(rating ~ interface + (1|participant), data=df)
    
    confint = confint(m, method="Wald")["interfacetreatment",]
    diff_intervals = data.frame(experiment=paste0("e", i),
        diff=fixef(m)["interfacetreatment"], 
        diff_min=confint[1], 
        diff_max=confint[2],
        se=summary(experiments[[1]]$m)$coef["interfacetreatment","Std. Error"]
        )
})

experiments = list(
    run_experiment(1, 20),
    run_experiment(2, 20),
    run_experiment(3, 20)
)

df = ldply(experiments, function(e) e$df)
diff_intervals = ldply(experiments, function(e) e$diff_intervals)
diffs = ddply(df, ~ experiment + participant, function(df) data.frame(diff=df[2,"rating"] - df[1,"rating"]))
