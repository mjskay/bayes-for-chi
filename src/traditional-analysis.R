library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(metafor)

theme_set(theme_bw())

#meta analysis
mm = rma(yi=diff, sei=se, data=diff_intervals)
smm = summary(mm)
diff_intervals %<>% rbind(data.frame(
        experiment="meta", diff = smm$b, diff_min = smm$ci.lb, diff_max = smm$ci.ub, se = smm$se))

#plot of difference
ggplot(diffs, aes(x=experiment, y=diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3) +
    geom_pointrange(data=diff_intervals, mapping=aes(ymin=diff_min, ymax=diff_max), color="red", size=0.75) +
    coord_flip()

#save.image("output/e3-1.RData")
#load("output/e3-1.RData")
