# Functions for running the frequentist anlaysis 
# 
# Author: mjskay
###############################################################################

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lme4)
library(metafor)
library(lsmeans)
library(boot)       #logit, inv.logit



theme_set(theme_bw())

#perform traditional analysis on given data frame representing a simulation 
#(i.e., a set of experiments from the same simulation)
frequentist_analysis_for_simulation = function(df) {
    #calculate study-level effect sizes (mean diff), intervals, and standard error
    effects = ddply(df, ~ experiment, function (df) {
            #fit models for each outcome variable
            m.rating = lmer(rating ~ interface + (1|participant), data=df)
            m.response_rate = glmer(response_rate ~ interface + (1|participant), data=df, family=poisson)
            m.completed = glmer(completed ~ interface + (1|participant), data=df, family=binomial)
            
            #For each treatment (non-control) condition, get an 
            #estimate of the mean difference against the control.
            #This assumes that the control condition is the first level.
            effects = ldply(levels(factor(df$interface))[-1], function(interface) {
                    coef_name = paste0("interface", interface)
                    ci.rating = confint(m.rating, method="Wald")[coef_name,]
                    ci.response_rate = confint(m.response_rate, method="Wald")[coef_name,]
                    ci.completed = confint(m.completed, method="Wald")[coef_name,]
                    
                    data.frame(
                        interface = interface,
                        rating_diff = fixef(m.rating)[coef_name],
                        rating_diff_min = ci.rating[1],
                        rating_diff_max = ci.rating[2],
                        rating_se = summary(m.rating)$coef[coef_name,"Std. Error"],
                        response_rate_log_diff = fixef(m.response_rate)[coef_name], 
                        response_rate_log_diff_min = ci.response_rate[1],
                        response_rate_log_diff_max = ci.response_rate[2],
                        response_rate_se = summary(m.response_rate)$coef[coef_name,"Std. Error"],
                        completed_lor_diff = fixef(m.completed)[coef_name], 
                        completed_lor_diff_min = ci.completed[1],
                        completed_lor_diff_max = ci.completed[2],
                        completed_se = summary(m.completed)$coef[coef_name,"Std. Error"]
                    )
                })
            
            #if there are 3 conditions, include contrasts of the two treatments
            if (length(levels(factor(df$interface))) == 3) {
                #3rd row is contrast between treatments 
                contr.rating = confint(lsmeans(m.rating, pairwise ~ interface)$contrasts)[3,]
                contr.response_rate = confint(lsmeans(m.response_rate, pairwise ~ interface)$contrasts)[3,]
                contr.completed = confint(lsmeans(m.completed, pairwise ~ interface)$contrasts)[3,]
                
                #effects are negated because lsmeans compares the levels in the opposite order to what we want
                rbind(effects, data.frame(
                    interface = "treatment2 - treatment1",
                    rating_diff = -contr.rating$estimate,
                    rating_diff_min = -contr.rating$upper.CL,
                    rating_diff_max = -contr.rating$lower.CL,
                    rating_se = contr.rating$SE,
                    response_rate_log_diff = -contr.response_rate$estimate, 
                    response_rate_log_diff_min = -contr.response_rate$asymp.UCL,
                    response_rate_log_diff_max = -contr.response_rate$asymp.LCL,
                    response_rate_se = contr.response_rate$SE,
                    completed_lor_diff = -contr.completed$estimate, 
                    completed_lor_diff_min = -contr.completed$asymp.UCL,
                    completed_lor_diff_max = -contr.completed$asymp.LCL,
                    completed_se = contr.completed$SE
                ))
            }
            else effects
        })
    
    #meta analysis
    mm.rating = rma(yi=rating_diff, sei=rating_se, data=filter(effects, interface=="treatment1"))
    smm.rating = summary(mm.rating)
    
    mm.response_rate = rma(yi=response_rate_log_diff, sei=response_rate_se, data=filter(effects, interface=="treatment1"))
    smm.response_rate = summary(mm.response_rate)
    
    mm.completed = rma(yi=completed_lor_diff, sei=completed_se, data=filter(effects, interface=="treatment1"))
    smm.completed = summary(mm.completed)
    
    
    effects %<>% rbind(data.frame(
            experiment="meta",
            interface="treatment1", 
            rating_diff = smm.rating$b, 
            rating_diff_min = smm.rating$ci.lb, 
            rating_diff_max = smm.rating$ci.ub, 
            rating_se = smm.rating$se,
            response_rate_log_diff = smm.response_rate$b, 
            response_rate_log_diff_min = smm.response_rate$ci.lb, 
            response_rate_log_diff_max = smm.response_rate$ci.ub, 
            response_rate_se = smm.response_rate$se,
            completed_lor_diff = smm.completed$b, 
            completed_lor_diff_min = smm.completed$ci.lb, 
            completed_lor_diff_max = smm.completed$ci.ub, 
            completed_se = smm.completed$se
        ))
    
    effects
}

#perform traditional analysis on given set of simulations and return
#a data frame of estimated study effects
frequentist_analysis = function(simulations) {
    ddply(simulations, ~ simulation, frequentist_analysis_for_simulation, 
        .progress=progress_win(title="Running frequentist analysis..."))
}
