## JAGS analysis
source("src/jags-analysis-util.R")

#run models, each using the posterior from the previous
#as the prior for the next
m1 = run_jags_analysis(filter(df, experiment == "e1"))
m2 = run_jags_analysis(filter(df, experiment == "e2"),
    b_priors = m1$b_posts,
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
b = rbind.fill(
    cbind(experiment="e1", m1$b),
    cbind(experiment="e2", m2$b),
    cbind(experiment="e3", m3$b),
    cbind(experiment="e4", m4$b),
    #add contrast between treatments in last experiment
    extract_samples(m4$fit, b[interface] | interface) %>%
        mutate(
            experiment = "e4",
            b = treatment2 - treatment1,
            interface = "treatment2 - treatment1"
        )
)

#treatment effects
b_nc = filter(b, interface != "control")
ggposterior(b_nc, aes(x=interface, y=b)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_hline(yintercept=0.5, linetype="dashed", color="red") +
    geom_hline(yintercept=1.0, linetype="dashed", color="skyblue") +
    scale_x_discrete(limits=rev(levels(factor(b_nc$interface)))) +    #reverse interface display order
    facet_grid(experiment ~ .) +
    ylim(-4,4)

#within-participant sd
ggposterior(params, aes(x=experiment, y=sqrt(1/tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")

#within-participant precision, experiment 1 with prior
ggdensity(filter(params, experiment=="e1"), aes(x=tau)) +
    stat_function(fun=dgamma, args=list(1,1)) +
    xlim(0, 10)

#within-participant precision with analytical fit
ggdensity(filter(params, experiment=="e4"), aes(x=tau)) +
    stat_function(fun=dgamma, args=m4$tau_fit)

#between-participant sd
ggposterior(params, aes(x=experiment, y=sqrt(1/participant_tau))) +
    scale_x_discrete(limits=rev(levels(params$experiment))) +    #reverse experiment display order
    geom_hline(yintercept=1, linetype="dashed")

#within-participant precision, experiment 1 with prior
ggdensity(filter(params, experiment=="e1"), aes(x=participant_tau)) +
    stat_function(fun=dgamma, args=list(1,1)) +
    xlim(0, 10)

#between-participant precision with analytical fit
ggdensity(filter(params, experiment=="e4"), aes(x=participant_tau)) +
    stat_function(fun=dgamma, args=m4$participant_tau_fit)

    