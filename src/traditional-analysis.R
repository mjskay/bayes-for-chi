library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(metafor)

theme_set(theme_bw())

#calculate study-level effect sizes (mean diff), intervals, and standard error
study_effects = ddply(df, ~ experiment, function (df) {
        #fit models for each outcome variable
        m.rating = lmer(rating ~ interface + (1|participant), data=df)
        m.response_rate = glmer(response_rate ~ interface + (1|participant), data=df, family=poisson)
        
        #For each treatment (non-control) condition, get an 
        #estimate of the mean difference against the control.
        #This assumes that the control condition is the first level.
        ldply(levels(factor(df$interface))[-1], function(interface) {
                coef_name = paste0("interface", interface)
                ci.rating = confint(m.rating, method="Wald")[coef_name,]
                ci.response_rate = confint(m.response_rate, method="Wald")[coef_name,]
                
                data.frame(
                    interface = interface,
                    rating_diff = fixef(m.rating)[coef_name],
                    rating_diff_min = ci.rating[1],
                    rating_diff_max = ci.rating[2],
                    rating_se = summary(m.rating)$coef[coef_name,"Std. Error"],
                    response_rate_log_diff = fixef(m.response_rate)[coef_name], 
                    response_rate_log_diff_min = ci.response_rate[1],
                    response_rate_log_diff_max = ci.response_rate[2],
                    response_rate_se = summary(m.response_rate)$coef[coef_name,"Std. Error"]
                )
            })
    })

#meta analysis
mm.rating = rma(yi=rating_diff, sei=rating_se, data=filter(study_effects, interface=="treatment1"))
smm.rating = summary(mm.rating)

mm.response_rate = rma(yi=response_rate_log_diff, sei=response_rate_se, data=study_effects)
smm.response_rate = summary(mm.response_rate)

study_effects %<>% rbind(data.frame(
        experiment="meta",
        interface="treatment1", 
        rating_diff = smm.rating$b, 
        rating_diff_min = smm.rating$ci.lb, 
        rating_diff_max = smm.rating$ci.ub, 
        rating_se = smm.rating$se,
        response_rate_log_diff = smm.response_rate$b, 
        response_rate_log_diff_min = smm.response_rate$ci.lb, 
        response_rate_log_diff_max = smm.response_rate$ci.ub, 
        response_rate_se = smm.response_rate$se
    ))

#plot of difference
ggplot(participant_effects, aes(x=experiment, y=rating_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    geom_hline(yintercept=1, linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3, color="#999999") +
    geom_pointrange(data=study_effects, mapping=aes(ymin=rating_diff_min, ymax=rating_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$experiment))) +    #reverse experiment display order
    facet_wrap(~interface) +
    coord_flip()

ggplot(participant_effects, aes(x=experiment, y=response_rate_log_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=log(1.5), linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3) +
    geom_pointrange(data=study_effects, mapping=aes(ymin=response_rate_log_diff_min, ymax=response_rate_log_diff_max), color="red", size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$experiment))) +    #reverse experiment display order
    coord_flip()

#save.image("output/e123-1.RData")
#load("output/e3-1.RData")
