# Plots for a single simulation
# 
# Author: Matthew
###############################################################################

#simulation of interest
sim = "s1"

#forest plot of traditional analysis
ss$study_effects %>%
    filter(simulation == sim) %>%
    ggplot(aes(x=interface, y=completed_lor_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=ss$treatment1_log_odds_ratio, linetype="dashed", color="red") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio, linetype="dashed", color="green") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio - ss$treatment1_log_odds_ratio, linetype="dashed", color="skyblue") +
    geom_pointrange(mapping=aes(ymin=completed_lor_diff_min, ymax=completed_lor_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$interface))) +    #reverse treatment display order
    scale_color_discrete(guide=FALSE) + 
    facet_grid(experiment ~ .) +
    coord_flip() +
    ylim(-4,4)

#forest plot of bayesian analysis
ss$b %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    ggeye(aes(x=interface, y=b, color=interface), fill="#cccccc") +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=ss$treatment1_log_odds_ratio, linetype="dashed", color="red") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio, linetype="dashed", color="green") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio - ss$treatment1_log_odds_ratio, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(factor(b_nc$interface)))) +    #reverse interface display order
    scale_color_discrete(guide=FALSE) + 
    facet_grid(experiment ~ ., drop=FALSE) +
    ylim(-4,4)



m %>%
    filter(simulation == sim, interface != "control") %>%
    mutate(
        #include (empty) meta experiment to line up plot with traditional analysis 
        experiment = factor(experiment, levels=c(levels(factor(experiment)), "meta"))
    ) %>%
    ggeye(aes(x=interface, y=b, color=interface), fill="#cccccc") +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=ss$treatment1_log_odds_ratio, linetype="dashed", color="red") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio, linetype="dashed", color="green") +
    geom_hline(yintercept=ss$treatment2_log_odds_ratio - ss$treatment1_log_odds_ratio, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(factor(b_nc$interface)))) +    #reverse interface display order
    scale_color_discrete(guide=FALSE) + 
    facet_grid(experiment ~ ., drop=FALSE) +
    ylim(-4,4)
