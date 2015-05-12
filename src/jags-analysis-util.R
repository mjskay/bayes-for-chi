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
        two_treatments = FALSE,
        b1_prior = .(dnorm(0,0.01)), 
        b2_prior = .(dnorm(0,0.01)), 
        b3_prior = .(dnorm(0,0.01)), 
        tau_prior = .(dgamma(2,2)),
        participant_tau_prior = .(dgamma(2,2))
    ) {
    jags_model = metajags_model({
        #core model
        for (i in 1:n) {
            rating[i] ~ dnorm(b1 + b2 * (interface[i] == 2) + 
                    (if (two_treatments) b3 * (interface[i] == 3) else 0) +
                    u[participant[i]], tau)
        }
        
        #intercept
        b1 ~ R(b1_prior)
        
        #variance
        tau ~ R(tau_prior)
        
        #interface effects
        b2 ~ R(b2_prior)
        if (two_treatments) b3 ~ R(b3_prior)
        
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
            model=jags_model$code, monitor=c("b1", "b2", if (two_treatments) "b3", "u", "tau", "participant_tau"), 
            burnin=500000, sample=10000, thin=50, modules="glm", data=data_list, method="parallel"
        ) %>%
        apply_prototypes(df)

    #extract parameters and fit marginal posteriors to them
    within(m, { 
        params = as.data.frame(as.matrix(as.mcmc.list(fit)))
        
        b1_post = fitdistr(params$b1, dst, start=list(m=mean(params$b1), s=sd(params$b1), df=20))$estimate
        b1_dist = with(as.list(b1_post), bquote(dt(.(m), .(1/s^2), .(df))))
         
        b2_post = fitdistr(params$b2, dst, start=list(m=mean(params$b2), s=sd(params$b2), df=20))$estimate
        b2_dist = with(as.list(b2_post), bquote(dt(.(m), .(1/s^2), .(df))))
        
        if (two_treatments) {
            b3_post = fitdistr(params$b3, dst, start=list(m=mean(params$b3), s=sd(params$b3), df=20))$estimate
            b3_dist = with(as.list(b3_post), bquote(dt(.(m), .(1/s^2), .(df))))
        }
        
        tau_post = fitdist(params$tau, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate
        tau_dist = with(as.list(tau_post), bquote(dgamma(.(shape), .(rate))))
        
        participant_tau_post = fitdist(params$participant_tau, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate
        participant_tau_dist = with(as.list(participant_tau_post), bquote(dgamma(.(shape), .(rate))))
    })
}
