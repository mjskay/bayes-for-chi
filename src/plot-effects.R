# Plots of effects from each analysis
# 
# Author: Matthew
###############################################################################

library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gamlss)
library(grid)

treatment_colors = c("#e41a1c", "#377eb8", "#4daf4a")    

#settings
n_participants = 100; sim = "s8"
#n_participants = 20; sim = "s2"

#load data
load(paste0("output/n", n_participants, "/simulations.RData"))
load(paste0("output/n", n_participants, "/freq_effects.RData"))
load(paste0("output/n", n_participants, "/bayes_effects.RData"))

theme_set(theme_bw() + theme(
    panel.grid.major.y=element_line(color="lightgray", size=0.5), 
    panel.grid.major.x=element_blank(), 
    panel.grid.minor=element_blank(),
    axis.line=element_line(color="black"),
    panel.border=element_blank(),
    text=element_text(size=14),
    axis.text=element_text(size=rel(15/16)),
    axis.ticks.length=unit(8, "points"),
    line=element_line(size=.75)
))

#settings
settings = attr(simulations, "settings")

#forest plot
forest_plot = function(df, density_data=NA) {
    p = ggplot(df, aes(x=interface, y=completed_lor))

    if (is.data.frame(density_data)) {
        p = p + geom_violin(aes(violinwidth=density), data=density_data, color=NA, fill="#cccccc")
    }
    
    p +
        geom_hline(yintercept=0, color="darkgray", size=0.5) +
        geom_hline(yintercept=settings$treatment1_completed_lor, linetype="dashed", color=treatment_colors[[1]], size=0.5) +
        geom_hline(yintercept=settings$treatment2_completed_lor, linetype="dashed", color=treatment_colors[[2]], size=0.5) +
        geom_hline(yintercept=settings$treatment2_completed_lor - settings$treatment1_completed_lor, linetype="dashed", color=treatment_colors[[3]], size=0.5) +
        geom_pointrange(mapping=aes(ymin=completed_lor_min, ymax=completed_lor_max, color=interface), size=0.75) +
        scale_x_discrete(limits=rev(levels(df$interface))) +    #reverse treatment display order
        scale_color_manual(guide=FALSE, values=treatment_colors) + 
        facet_grid(experiment ~ ., drop=FALSE) +
        coord_flip() +
        ylim(-2.75,2.75)
}

#forest plot of traditional analysis
windows(width=7,height=7)
freq_effects %>%
    filter(simulation == sim) %>%
    mutate(interface = factor(interface, labels=c("fast-to-slow", "slow-to-fast", "slow-to-fast \U2212 fast-to-slow"))) %>%
    forest_plot()
ggsave(paste0("output/n", n_participants, "/forest-plot-freq.pdf"), width=7, height=7)

#generate densities of bayesian effects for plotting
bayes_densities = bayes_effects %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(interface = factor(interface, labels=c("fast-to-slow", "slow-to-fast", "slow-to-fast \U2212 fast-to-slow"))) %>%
    group_by(experiment, interface) %>%
    do({
        x = qTF(ppoints(1000), .$m, .$s, .$df)
        data.frame(
            completed_lor = x,
            density = dTF(x, .$m, .$s, .$df)
        )
    })

#forest plot of bayesian analysis
windows(width=7, height=7)
bayes_effects %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(interface = factor(interface, labels=c("fast-to-slow", "slow-to-fast", "slow-to-fast \U2212 fast-to-slow"))) %>%
    mutate(
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    forest_plot(bayes_densities)
ggsave(paste0("output/n", n_participants, "/forest-plot-bayes.eps"), width=7, height=7)






#dotplot of effects from each experiment
dotplot = function(df) {
    if (n_participants == 20) {
        xmax = 3.25
        dotsize = 1
    } else {
        xmax = 2
        dotsize = 0.9
    }
    ggplot(df, aes(x=completed_lor)) +
        geom_vline(xintercept=0, color="darkgray", size=0.5) +
        geom_dotplot(aes(fill=interface, color=interface), binwidth=0.1, stackratio=1, dotsize=dotsize) +
        geom_vline(xintercept=settings$treatment1_completed_lor, linetype="dashed", color=treatment_colors[[1]]) +
        geom_vline(xintercept=settings$treatment2_completed_lor, linetype="dashed", color=treatment_colors[[2]]) +
        geom_vline(xintercept=settings$treatment2_completed_lor - settings$treatment1_completed_lor, linetype="dashed", color=treatment_colors[[3]]) +
        scale_color_manual(guide=FALSE, values=treatment_colors) + 
        scale_fill_manual(guide=FALSE, values=treatment_colors) + 
        facet_grid(experiment ~ interface, drop=FALSE) + 
        xlim(-xmax, xmax)
}

#frequentist dotplot
windows(width=6, height=6)
dotplot(freq_effects)

#bayesian dotplot
windows(width=6, height=6)
bayes_effects %>%
    filter(interface != "control") %>%
    mutate(
        interface = factor(interface),
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    dotplot()


#error
effects_rmse_ = function(effects, interface_, true_effect, experiment_="e4") {
    effects %>% 
        filter(interface==interface_, experiment==experiment_) %>% 
        summarise(rmse=sqrt(mean((completed_lor - true_effect)^2))) %$%
        cat(experiment_, interface_, "\t", rmse, "\n")
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

#error in E1
cat("E1 RMSE for freq_effects")
effects_rmse_(freq_effects, "treatment1", settings$treatment1_completed_lor, "e1")

cat("E1 RMSE for bayes_effects")
effects_rmse_(bayes_effects, "treatment1", settings$treatment1_completed_lor, "e1")




data.frame(p = ppoints(50)) %>%
    mutate(x = qnorm(p)) %>%
    ggplot(aes(y=p, x=x)) +
    stat_function(fun=function(x) pnorm(x)) +
    geom_segment(aes(x=-3.5, xend=x, y=p, yend=p), color="red") +
    geom_segment(aes(x=x, xend=x, y=0, yend=p), color="red") +
    coord_cartesian(xlim=c(-3,3))
     

