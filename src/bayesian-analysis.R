## Utility functions for the JAGS analysis

library(MASS, pos=which(search() == "package:stats"))   #load MASS high up on search path to prevent MASS::select() from having priority
library(fitdistrplus)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(runjags)
library(metabayes)
library(tidybayes)

#scaled and shifted t distribution
dst <- function(x, m, s, df) dt((x-m)/s, df)/s

## pass in a data frame of one experiment to analyze, along with priors 
run_jags_analysis = function(df,
        final_model = FALSE,
        b_priors = .(
            dnorm(0,0.01),  #TODO: change to cauchy per Gelman
            dnorm(0,0.01)
        ),
        participant_sigma_prior = .(dunif(0,100)),
        participant_tau_prior = NA 
    ) {
        
    jags_model = metajags({
        #core model
        for (i in 1:n) {
            logit(p[i]) <- b[1] + 
                    ifelse(interface[i] > 1, b[interface[i]], 0) +
                    u[participant[i]]
            completed[i] ~ dbern(p[i])
        }
        
        #interface effects
        R(lapply(seq_along(b_priors), function(i) bquote(
            b[.(i)] ~ .(b_priors[[i]])
        )))
        
        #participant effects
        if (is.na(participant_tau_prior)) {
            participant_sigma ~ R(participant_sigma_prior)
            participant_tau <- 1/participant_sigma^2
        }
        else {
            participant_tau ~ R(participant_tau_prior)
        }
        for (k in 1:n_participant) {
            u[k] ~ dnorm(0, participant_tau)
        }
    })
    print(jags_model)
    data_list = df %>%
        select(completed, interface, participant) %>% 
        compose_data()

    m = list()  #returned model
    
    #fit jags model
    if (final_model) {
        burnin=500000
        sample=10000
        thin=50
    }
    else {
        burnin=10000
        sample=10000
        thin=1
    }
    m$fit = run.jags(
            model=code(jags_model), monitor=c("b", "participant_tau"), 
            burnin=burnin, sample=sample, thin=thin, 
            modules="glm", data=data_list, method="parallel",
            summarise=FALSE
        ) %>%
        apply_prototypes(df)

    #extract parameters and fit marginal posteriors to them
    within(m, { 
        params = as.data.frame(as.matrix(as.mcmc.list(fit)))
        
        b = extract_samples(fit, b[interface])
        b_fits = dlply(b, ~ interface, function(b) 
            as.list(fitdistr(b$b, dst, start=list(m=mean(b$b), s=sd(b$b), df=20))$estimate))
        b_posts = llply(b_fits, function(fit)
            with(fit, bquote(dt(.(m), .(1/s^2), .(df)))))

        participant_tau_fit = as.list(fitdist(params$participant_tau, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate)
        participant_tau_post = with(participant_tau_fit, bquote(dgamma(.(shape), .(rate))))
    })
}

#given a data.frame for a simulation, run the bayesian models for each experiment in it
bayesian_models_for_simulation = function(df, final_model=FALSE) {
    #analyze each experiment
    within(list(), {
        m1 = run_jags_analysis(filter(df, experiment == "e1"), final_model = final_model)
        m2 = run_jags_analysis(filter(df, experiment == "e2"), final_model = final_model,
            b_priors = m1$b_posts,
            participant_tau_prior = m1$participant_tau_post
        )
        m3 = run_jags_analysis(filter(df, experiment == "e3"), final_model = final_model,
            b_priors = m2$b_posts,
            participant_tau_prior = m2$participant_tau_post
        )
        m4 = run_jags_analysis(filter(df, experiment == "e4"), final_model = final_model,
            b_priors = c(m3$b_posts,
                    #normal prior with scale derived from posterior of previous treatment
                    #(sd of twice the approx top end of the 95% conf int)
                    bquote(dnorm(0, .(with(m3$b_fits$treatment1, 1/((m + s * 2) * 2) ^ 2))))
            ),
            participant_tau_prior = m3$participant_tau_post
        )
    })
}

#perform bayesian analysis on given set of simulations of experiments
bayesian_analysis = function(ss) {
    models = dlply(ss$data, ~ simulation, bayesian_models_for_simulation, 
        .progress=progress_win(title="Running Bayesian analysis..."))
    ss$params = ldply(models, .id="simulation", function (sim_models) 
        rbind.fill(
            cbind(experiment="e1", sim_models$m1$params),
            cbind(experiment="e2", sim_models$m2$params),
            cbind(experiment="e3", sim_models$m3$params),
            cbind(experiment="e4", sim_models$m4$params)
        ))
    ss$b = ldply(models, .id="simulation", function (sim_models) 
        rbind(
            cbind(experiment="e1", sim_models$m1$b),
            cbind(experiment="e2", sim_models$m2$b),
            cbind(experiment="e3", sim_models$m3$b),
            cbind(experiment="e4", sim_models$m4$b),
            #add contrast between treatments in last experiment
            cbind(experiment="e4", compare_levels(sim_models$m4$b, b, by=interface, 
                    comparison=.(treatment2 - treatment1)))
        ))

    ss
}
