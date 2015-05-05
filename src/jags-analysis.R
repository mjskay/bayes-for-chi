library(plyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lme4)
library(metabayes)
library(tidybayes)


jags_model = metajags_model({
        #core model
        for (i in 1:n) {
            rating[i] ~ dnorm(b1 + b2 * (interface[i] == 2) + u[participant[i]], tau)
        }
        
        #intercept
        b1 ~ dnorm(0,0.01)
        
        #variance
        sigma ~ dunif(0,10)
        tau <- 1/sigma^2
        
        #interface effects
        b2 ~ dnorm(0,0.01)
        
        #participant effects
        participant_sigma ~ dunif(0,10)
        participant_tau <- 1/participant_sigma^2
        for (k in 1:n_participant) {
            u[k] ~ dnorm(0, participant_tau)
        }
    })


data_list = experiments[[2]]$df %>%
    select(rating, interface, participant) %>% 
    compose_data() 
m = run.jags(model=jags_model$code, monitor=c("b1", "b2", "u", "sigma", "participant_sigma"), 
    burnin=100000, sample=10000, thin=10, modules="glm", data=data_list, method="parallel") 


fit = as.matrix(as.mcmc.list(m)) %>% 
    apply_prototypes(experiments[[1]]$df)

ip = extract_samples(fit, b2[])
dst <- function(x, m, s, df) dt((x-m)/s, df)/s
tfit = fitdistr(ip$b2, dst, start=list(m=mean(ip$b2), s=sd(ip$b2), df=20))$estimate
ggdensity(ip, aes(x=b2)) +
    stat_function(fun=function(x) dst(x, tfit[1], tfit[2], tfit[3]))

ggposterior(ip, aes(x=1, y=b2))


sigma = extract_samples(fit, sigma[])
igammafit = fitdist(sigma$sigma^2, "invgamma", start=list(alpha=1,beta=1),  method="mge", gof="CvM")$estimate
ggdensity(sigma, aes(x=sigma^2)) +
    stat_function(fun=function(x) pscl:::densigamma(x, igammafit[1], igammafit[2])) 

ggposterior(sigma, aes(x=1, y=sigma)) 


dinvgamma = pscl::densigamma
pinvgamma = pscl::pigamma
qinvgamma = pscl::qigamma

participant_sigma = extract_samples(fit, participant_sigma[])
igammafit = fitdist(participant_sigma$participant_sigma^2, "invgamma", start=list(alpha=1,beta=1), method="mge", gof="CvM")$estimate
ggdensity(participant_sigma, aes(x=participant_sigma^2)) +
    stat_function(fun=function(x) pscl::densigamma(x, igammafit[1], igammafit[2]))

ggposterior(participant_sigma, aes(x=1, y=participant_sigma))
