library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(metafor)

theme_set(theme_bw())

#calculate study-level effect sizes (mean diff), intervals, and standard error
study_effects = ddply(df, ~ experiment, function (df) {
        m.rating = lmer(rating ~ interface + (1|participant), data=df)
        ci.rating = confint(m.rating, method="Wald")["interfacetreatment",]
        
        m.response_rate = glmer(response_rate ~ interface + (1|participant), data=df, family=poisson)
        ci.response_rate = confint(m.response_rate, method="Wald")["interfacetreatment",]
        
        data.frame(
            rating_diff = fixef(m.rating)["interfacetreatment"], 
            rating_diff_min = ci.rating[1],
            rating_diff_max = ci.rating[2],
            rating_se = summary(m.rating)$coef["interfacetreatment","Std. Error"],
            response_rate_log_diff = fixef(m.response_rate)["interfacetreatment"], 
            response_rate_log_diff_min = ci.response_rate[1],
            response_rate_log_diff_max = ci.response_rate[2],
            response_rate_se = summary(m.response_rate)$coef["interfacetreatment","Std. Error"]
        )
    })

#meta analysis
mm.rating = rma(yi=rating_diff, sei=rating_se, data=study_effects)
smm.rating = summary(mm.rating)

mm.response_rate = rma(yi=response_rate_log_diff, sei=response_rate_se, data=study_effects)
smm.response_rate = summary(mm.response_rate)

study_effects %<>% rbind(data.frame(
        experiment="meta", 
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
    geom_hline(yintercept=0.5, linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3) +
    geom_pointrange(data=study_effects, mapping=aes(ymin=rating_diff_min, ymax=rating_diff_max), color="red", size=0.75) +
    coord_flip() +
    ylim(-4,4)

ggplot(participant_effects, aes(x=experiment, y=response_rate_log_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=log(1.5), linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3) +
    geom_pointrange(data=study_effects, mapping=aes(ymin=response_rate_log_diff_min, ymax=response_rate_log_diff_max), color="red", size=0.75) +
    coord_flip()

#save.image("output/e3-1.RData")
#load("output/e3-1.RData")
