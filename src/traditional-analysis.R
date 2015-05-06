library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(metafor)

theme_set(theme_bw())

#calculate study-level effect sizes (mean diff), intervals, and standard error
study_effects = ddply(df, ~ experiment, function (df) {
        m = lmer(rating ~ interface + (1|participant), data=df)
        ci = confint(m, method="Wald")["interfacetreatment",]
        data.frame(
            diff = fixef(m)["interfacetreatment"], 
            diff_min = ci[1],
            diff_max = ci[2],
            se = summary(m)$coef["interfacetreatment","Std. Error"]
        )
    })

#meta analysis
mm = rma(yi=diff, sei=se, data=study_effects)
smm = summary(mm)
study_effects %<>% rbind(data.frame(
        experiment="meta", diff = smm$b, diff_min = smm$ci.lb, diff_max = smm$ci.ub, se = smm$se))

#plot of difference
ggplot(participant_effects, aes(x=experiment, y=diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3) +
    geom_pointrange(data=study_effects, mapping=aes(ymin=diff_min, ymax=diff_max), color="red", size=0.75) +
    coord_flip()

#save.image("output/e3-1.RData")
#load("output/e3-1.RData")
