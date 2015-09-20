# Plots of effects from each analysis
# 
# Author: Matthew
###############################################################################

library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gamlss)

#load data
load("output/simulations.RData")
load("output/freq_effects.RData")
load("output/bayes_effects.RData")

theme_set(theme_light())

#simulation of interest
sim = "s8"
settings = attr(simulations, "settings")

#forest plot
forest_plot = function(df, density_data=NA) {
    p = ggplot(df, aes(x=interface, y=completed_lor))

    if (is.data.frame(density_data)) {
        p = p + geom_violin(aes(violinwidth=density), data=density_data, color=NA, fill="#cccccc")
    }
    
    p +
        geom_hline(yintercept=0, linetype="dashed") +
        geom_hline(yintercept=settings$treatment1_completed_lor, linetype="dashed", color="red") +
        geom_hline(yintercept=settings$treatment2_completed_lor, linetype="dashed", color="green") +
        geom_hline(yintercept=settings$treatment2_completed_lor - settings$treatment1_completed_lor, linetype="dashed", color="skyblue") +
        geom_pointrange(mapping=aes(ymin=completed_lor_min, ymax=completed_lor_max, color=interface), size=0.75) +
        scale_x_discrete(limits=rev(levels(freq_effects$interface))) +    #reverse treatment display order
        scale_color_discrete(guide=FALSE) + 
        facet_grid(experiment ~ ., drop=FALSE) +
        coord_flip() +
        ylim(-3,3)
}

#forest plot of traditional analysis
windows(width=6,height=7)
freq_effects %>%
    filter(simulation == sim) %>%
    forest_plot()

#generate densities of bayesian effects for plotting
bayes_densities = bayes_effects %>%
    filter(simulation == sim, interface != "control") %>%
    group_by(experiment, interface) %>%
    do({
        x = qTF(ppoints(1000), .$m, .$s, .$df)
        data.frame(
            completed_lor = x,
            density = dTF(x, .$m, .$s, .$df)
        )
    })

#forest plot of bayesian analysis
windows(width=6, height=7)
bayes_effects %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    forest_plot(bayes_densities)






#dotplot of effects from each experiment
dotplot = function(df) {
    ggplot(df, aes(x=completed_lor)) +
        geom_dotplot(aes(fill=interface, color=interface), binwidth=0.1, stackratio=.9, dotsize=1.2) +
        geom_vline(xintercept=0, linetype="dashed") +
        geom_vline(xintercept=settings$treatment1_completed_lor, linetype="dashed", color="red") +
        geom_vline(xintercept=settings$treatment2_completed_lor, linetype="dashed", color="green") +
        geom_vline(xintercept=settings$treatment2_completed_lor - settings$treatment1_completed_lor, linetype="dashed", color="skyblue") +
        scale_color_discrete(guide=FALSE) + 
        scale_fill_discrete(guide=FALSE) + 
        facet_grid(experiment ~ interface, drop=FALSE) + 
        xlim(-2,2)
}

#frequentist dotplot
windows()
dotplot(freq_effects)

#bayesian dotplot
windows()
bayes_effects %>%
    filter(interface != "control") %>%
    mutate(
        interface = factor(interface),
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    dotplot()


#error
effects_rmse_ = function(effects, interface_, true_effect) {
    effects %>% 
        filter(interface==interface_, experiment=="e4") %>% 
        summarise(rmse=sqrt(mean((completed_lor - true_effect)^2))) %$%
        cat(interface_, "\t", rmse, "\n")
}
effects_rmse = function(effects) {
    cat("E4 RMSE for", deparse(substitute(effects)), "\n")
    effects_rmse_(effects, "treatment1", settings$treatment1_completed_lor)
    effects_rmse_(effects, "treatment2", settings$treatment2_completed_lor)
    effects_rmse_(effects, "treatment2 - treatment1", settings$treatment2_completed_lor - settings$treatment1_completed_lor)
    cat("\n")
}
effects_rmse(freq_effects)
effects_rmse(bayes_effects)


