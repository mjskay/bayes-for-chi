#stan analysis of experimental data
#N.B. INCOMPLETE; USING JAGS NOW

stan_model = "
    data {
	int n;
	int n_participant;
	int participant[n];
	vector[n] rating;
	int interface[n];
    }
    parameters {
	real b0;
	real b2;
	vector[n_participant] u;
	real<lower=0> sigma;
	real<lower=0> participant_sigma;
    }
    model {
    #core model
    for (i in 1:n) {
    rating[i] ~ normal(b0 + b2 * (interface[i] == 2) + u[participant[i]], sigma);
    }
    
    #intercept
    b0 ~ normal(0,10);
    
    #variance
    sigma ~ uniform(0,10); 
    
    #interface effects
    b2 ~ normal(0,10);
    
    #participant effects
    participant_sigma ~ uniform(0,10);
    u ~ normal(0, participant_sigma);
    }"
m = stan(model_code=stan_model, data=data_list, iter=40000, thin=4) 
fit = m %>% 
    apply_prototypes(experiments[[1]]$df)
