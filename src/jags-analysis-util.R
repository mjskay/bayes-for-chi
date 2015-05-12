## Utility functions for the JAGS analysis

library(MASS)
library(fitdistrplus)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(runjags)
library(metabayes)
library(tidybayes)

source("src/util.R")

#scaled and shifted t distribution
dst <- function(x, m, s, df) dt((x-m)/s, df)/s

## pass in a data frame of one experiment to analyze, along with 
## priors
run_jags_analysis = function(df,
        b_priors = list(
            .(dnorm(0,0.01)), 
            .(dnorm(0,0.01))
        ), 
        tau_prior = .(dgamma(2,2)),
        participant_tau_prior = .(dgamma(2,2))
    ) {
    jags_model = metajags_model({
        #core model
        for (i in 1:n) {
            rating[i] ~ dnorm(b[interface[i]] + u[participant[i]], tau)
        }
        
        #interface effects
        R(lapply(seq_along(b_priors), function(i) bquote(
            b[.(i)] ~ .(b_priors[[i]])
        )))
        
        #variance
        tau ~ R(tau_prior)
        
        #participant effects
        participant_tau ~ R(participant_tau_prior)
        for (k in 1:n_participant) {
            u[k] ~ dnorm(0, participant_tau)
        }
    })
            
    data_list = df %>%
        select(rating, interface, participant) %>% 
        compose_data() 

    m = list()  #returned model
    
    #fit jags model
    m$fit = run.jags(
            model=jags_model$code, monitor=c("b", "u", "tau", "participant_tau"), 
            burnin=500000, sample=10000, thin=50, modules="glm", data=data_list, method="parallel"
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

        tau_fit = as.list(fitdist(params$tau, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate)
        tau_post = with(tau_fit, bquote(dgamma(.(shape), .(rate))))
        
        participant_tau_fit = as.list(fitdist(params$participant_tau, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate)
        participant_tau_post = with(participant_tau_fit, bquote(dgamma(.(shape), .(rate))))
    })
}
