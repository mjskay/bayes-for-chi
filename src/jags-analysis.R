## JAGS analysis
source("src/jags-analysis-util.R")

#run models, each using the posterior from the previous
#as the prior for the next
m1 = run_jags_analysis(filter(df, experiment == "e1"))
m2 = run_jags_analysis(filter(df, experiment == "e2"),
    b1_prior = m1$b1_dist,
    b2_prior = m1$b2_dist,
    tau_prior = m1$tau_dist,
    participant_tau_prior = m1$participant_tau_dist
)
m3 = run_jags_analysis(filter(df, experiment == "e3"),
    b1_prior = m2$b1_dist,
    b2_prior = m2$b2_dist,
    tau_prior = m2$tau_dist,
    participant_tau_prior = m2$participant_tau_dist
)
m4 = run_jags_analysis(filter(df, experiment == "e4"),
    two_treatments = TRUE,
    b1_prior = m3$b1_dist,
    b2_prior = m3$b2_dist,
    tau_prior = m3$tau_dist,
    participant_tau_prior = m3$participant_tau_dist
)

#combine posteriors into one dataset
params = rbind.fill(
    cbind(experiment="e1", m1$params),
    cbind(experiment="e2", m2$params),
    cbind(experiment="e3", m3$params),
    cbind(experiment="e4", m4$params)
)

#plot posteriors together
ggposterior(params, aes(x=experiment, y=b2)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    ylim(-4,4)

ggposterior(params, aes(x=experiment, y=b3)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    geom_hline(yintercept=1.0, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    ylim(-4,4)

ggposterior(params, aes(x=experiment, y=b1)) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=0, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/participant_tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")
