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

source("src/util.R")

final_models = FALSE    #TODO: make use of this 

#scaled and shifted t distribution
dst <- function(x, m, s, df) dt((x-m)/s, df)/s

## pass in a data frame of one experiment to analyze, along with 
## priors
run_jags_analysis = function(df,
        b_priors = .(
            dnorm(0,0.01),  #TODO: change to cauchy per Gelman
            dnorm(0,0.01)
        ),
        participant_sigma_prior = .(dunif(0,100)),
        participant_tau_prior = NA 
#        tau_prior = .(dgamma(1,1)),
#        participant_tau_prior = .(dgamma(1,1))
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
    m$fit = run.jags(
            model=code(jags_model), monitor=c("b", "u", "participant_tau", "p"), 
#            burnin=500000, sample=10000, thin=50, 
            burnin=100000, sample=10000, thin=1, 
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
