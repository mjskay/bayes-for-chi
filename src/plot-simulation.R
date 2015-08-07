# Plots for a single simulation
# 
# Author: Matthew
###############################################################################

#simulation of interest
sim = "s1"

#forest plot
forest_plot = function(df, density_data=NA) {
    p = ggplot(df, aes(x=interface, y=completed_lor_diff))
    
    if (is.data.frame(density_data)) {
        p = p + geom_violin(aes(violinwidth=density), data=density_data, color=NA, fill="#cccccc")
    }
    
    p +
        geom_hline(yintercept=0, linetype="dashed") +
        geom_hline(yintercept=ss$treatment1_log_odds_ratio, linetype="dashed", color="red") +
        geom_hline(yintercept=ss$treatment2_log_odds_ratio, linetype="dashed", color="green") +
        geom_hline(yintercept=ss$treatment2_log_odds_ratio - ss$treatment1_log_odds_ratio, linetype="dashed", color="skyblue") +
        geom_pointrange(mapping=aes(ymin=completed_lor_diff_min, ymax=completed_lor_diff_max, color=interface), size=0.75) +
        scale_x_discrete(limits=rev(levels(freq_effects$interface))) +    #reverse treatment display order
        scale_color_discrete(guide=FALSE) + 
        facet_grid(experiment ~ ., drop=FALSE) +
        coord_flip() +
        ylim(-4,4)
}

#forest plot of traditional analysis
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
            completed_lor_diff = x,
            density = dTF(x, .$m, .$s, .$df)
        )
    })

#forest plot of bayesian analysis
bayes_effects %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    forest_plot(bayes_densities)
