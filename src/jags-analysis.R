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

#combine posteriors into one dataset
params = rbind(
    cbind(experiment="e1", m1$params),
    cbind(experiment="e2", m2$params),
    cbind(experiment="e3", m3$params)
)

#plot posteriors together
ggposterior(params, aes(x=experiment, y=b2)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    ylim(-4,4)

ggposterior(params, aes(x=experiment, y=b1)) +
    geom_hline(yintercept=0, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/tau))) +
    geom_hline(yintercept=1, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/participant_tau))) +
    geom_hline(yintercept=1, linetype="dashed")
