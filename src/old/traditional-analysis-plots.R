

#plot of treatment effects (all aligned)
ggplot(participant_effects, aes(x=interface, y=rating_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=e$treatment1_rating, linetype="dashed", color="red") +
    geom_hline(yintercept=e$treatment2_rating, linetype="dashed", color="green") +
    geom_point(alpha=0.25, size=3, color="#999999") +
    geom_pointrange(data=study_effects, mapping=aes(ymin=rating_diff_min, ymax=rating_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$interface))) +    #reverse treatment display order
    facet_grid(experiment ~ .) +
    coord_flip() +
    ylim(-4,5)

#plot of treatment effects (all aligned, no data), rating
ggplot(study_effects, aes(x=interface, y=rating_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=e$treatment1_rating, linetype="dashed", color="red") +
    geom_hline(yintercept=e$treatment2_rating, linetype="dashed", color="green") +
    geom_pointrange(mapping=aes(ymin=rating_diff_min, ymax=rating_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$interface))) +    #reverse treatment display order
    facet_grid(experiment ~ .) +
    coord_flip() +
    ylim(-2,3)

#plot of treatment effects (all aligned, no data), completed
ggplot(study_effects, aes(x=interface, y=completed_lor_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=e$treatment1_log_odds_ratio, linetype="dashed", color="red") +
    geom_hline(yintercept=e$treatment2_log_odds_ratio, linetype="dashed", color="green") +
    geom_pointrange(mapping=aes(ymin=completed_lor_diff_min, ymax=completed_lor_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$interface))) +    #reverse treatment display order
    facet_grid(experiment ~ .) +
    coord_flip() +
    ylim(-3,4)

ggsave("output/traditional-analysis.pdf")

#plot of treatment effects (multiple columns)
ggplot(participant_effects, aes(x=experiment, y=rating_diff)) + 
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    geom_hline(yintercept=1, linetype="dashed", color="skyblue") +
    geom_point(alpha=0.25, size=3, color="#999999") +
    geom_pointrange(data=study_effects, mapping=aes(ymin=rating_diff_min, ymax=rating_diff_max, color=interface), size=0.75) +
    scale_x_discrete(limits=rev(levels(study_effects$experiment))) +    #reverse experiment display order
    facet_wrap(~interface) +
    coord_flip() +
    ylim(-4,4)

#save.image("output/e1234-es0.3-1.RData")
#load("output/e3-1.RData")
