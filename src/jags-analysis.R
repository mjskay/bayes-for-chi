## JAGS analysis
source("src/jags-analysis-util.R")

#run models, each using the posterior from the previous
#as the prior for the next
m1 = run_jags_analysis(filter(df, experiment == "e1"))
m2 = run_jags_analysis(filter(df, experiment == "e2"),
    b_priors = m2$b_posts,
    tau_prior = m1$tau_post,
    participant_tau_prior = m1$participant_tau_post
)
m3 = run_jags_analysis(filter(df, experiment == "e3"),
    b_priors = m2$b_posts,
    tau_prior = m2$tau_post,
    participant_tau_prior = m2$participant_tau_post
)
m4 = run_jags_analysis(filter(df, experiment == "e4"),
    b_priors = c(m3$b_posts,
        .(dnorm(0,0.01))
    ),
    tau_prior = m3$tau_post,
    participant_tau_prior = m3$participant_tau_post
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
    geom_hline(yintercept=1.0, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    ylim(-4,4)

ggposterior(params, aes(x=experiment, y=b3)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    geom_hline(yintercept=1.0, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    ylim(-4,4)

#difference in experiment 4
ggposterior(filter(params, experiment == "e4"), aes(x=experiment, y=b3 - b2)) +
    geom_hline(yintercept=0, linetype="dashed")


ggposterior(params, aes(x=experiment, y=b1)) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=0, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/participant_tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")
