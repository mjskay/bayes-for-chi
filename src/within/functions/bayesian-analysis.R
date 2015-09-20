# Functions for running the bayesian anlaysis 
# 
# Author: mjskay
###############################################################################

library(MASS, pos=which(search() == "package:stats"))   #load MASS high up on search path to prevent MASS::select() from having priority
library(gamlss)         #qTF
library(fitdistrplus)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(runjags)
library(coda)
library(metabayes)
library(tidybayes)

#fit a scaled and shifted t distribution
fit_t = function(x) {
    fit = gamlssML(x, family=TF)
    list(
       m = coef(fit, what="mu"),
       s = exp(coef(fit, what="sigma")),
       df = exp(coef(fit, what="nu"))     
    )
}

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
        b_fits = dlply(b, ~ interface, function(.) fit_t(.$b))
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
        e1 = run_jags_analysis(filter(df, experiment == "e1"), final_model = final_model)
        e2 = run_jags_analysis(filter(df, experiment == "e2"), final_model = final_model,
            b_priors = e1$b_posts,
            participant_tau_prior = e1$participant_tau_post
        )
        e3 = run_jags_analysis(filter(df, experiment == "e3"), final_model = final_model,
            b_priors = e2$b_posts,
            participant_tau_prior = e2$participant_tau_post
        )
        e4 = run_jags_analysis(filter(df, experiment == "e4"), final_model = final_model,
            b_priors = c(e3$b_posts,
                    #cauchy prior with scale derived from posterior of previous treatment
                    #(sd of the approx top end of the 95% credibility int)
                    bquote(dt(0, .(with(e3$b_fits$treatment1, 1 / max(abs(qTF(c(.025,.975), m, s, df)))))^2, 1))
            ),
            participant_tau_prior = e3$participant_tau_post
        )
    }) %>% rev()    #must reverse order to get e1, e2, e3, e4
}

#perform bayesian analysis on a given set of simulations
#save posterior to disk (for memory reasons) and then return
#parametric fits to posteriors with summaries
bayesian_effects_for_simulation = function(df, final_model=FALSE) {
    #fit bayesian models
    models = bayesian_models_for_simulation(df, final_model)
    
    #get effects from each study
    effects = ldply(models, .id="experiment", function(.) ldply(.$b_fits, as.data.frame))
    
    #add the difference between treatment2 and treatment1 in the last experiment 
    t2_t1 = compare_levels(models$e4$b, b, by=interface, 
        comparison=.(treatment2 - treatment1))
    t2_t1_fit = fit_t(t2_t1$b)
    effects %<>% rbind(
            cbind(experiment="e4", interface="treatment2 - treatment1", as.data.frame(t2_t1_fit))
        )
    
    #add the mean, max, min estimated differences (the intervals)
    effects %<>%
        mutate(
            completed_lor_diff = m, 
            completed_lor_diff_min = qTF(.025, m, s, df),
            completed_lor_diff_max = qTF(.975, m, s, df)
        )

    #save models to disk for later and then delete for memory
    save(models, file=paste0("output/bayesian_models_", df[1,"simulation"], ".RData"))
    rm("models")
    
    effects
}

#perform bayesian analysis on given set of simulations of experiments
bayesian_analysis = function(simulations, final_model=FALSE) {
    #get effects from models
    ddply(simulations, ~ simulation, function(df) bayesian_effects_for_simulation(df, final_model), 
        .progress=progress_win(title="Running Bayesian analysis..."))
}
