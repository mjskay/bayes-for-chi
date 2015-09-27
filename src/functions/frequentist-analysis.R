# Functions for running the frequentist analysis 
# 
# Author: mjskay
###############################################################################

library(MASS, pos=which(search() == "package:stats"))   #load MASS high up on search path to prevent MASS::select() from having priority
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
            m.completed = glm(completed ~ interface, data=df, family=binomial)
            
            #For each treatment (non-control) condition, get an 
            #estimate of the mean difference against the control.
            #This assumes that the control condition is the first level.
            effects = ldply(levels(factor(df$interface))[-1], function(interface) {
                    coef_name = paste0("interface", interface)
                    ci.completed = confint(m.completed, method="Wald")[coef_name,]
                    
                    data.frame(
                        interface = interface,
                        completed_lor = coef(m.completed)[coef_name], 
                        completed_lor_min = ci.completed[1],
                        completed_lor_max = ci.completed[2],
                        completed_se = summary(m.completed)$coef[coef_name,"Std. Error"]
                    )
                })
            
            #if there are 3 conditions, include contrasts of the two treatments
            if (length(levels(factor(df$interface))) == 3) {
                #3rd row is contrast between treatments 
                contr.completed = confint(lsmeans(m.completed, pairwise ~ interface)$contrasts)[3,]
                
                #effects are negated because lsmeans compares the levels in the opposite order to what we want
                rbind(effects, data.frame(
                    interface = "treatment2 - treatment1",
                    completed_lor = -contr.completed$estimate, 
                    completed_lor_min = -contr.completed$asymp.UCL,
                    completed_lor_max = -contr.completed$asymp.LCL,
                    completed_se = contr.completed$SE
                ))
            }
            else effects
        })
    
    #meta analysis
    mm.completed = rma(yi=completed_lor, sei=completed_se, data=filter(effects, interface=="treatment1"))
    smm.completed = summary(mm.completed)
    
    effects %<>% rbind(data.frame(
            experiment="meta",
            interface="treatment1", 
            completed_lor = smm.completed$b, 
            completed_lor_min = smm.completed$ci.lb, 
            completed_lor_max = smm.completed$ci.ub, 
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
