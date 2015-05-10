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
    geom_hline(yintercept=0.5, linetype="dashed", color="skyblue") +
    ylim(-4,4)

ggposterior(params, aes(x=experiment, y=b1)) +
    geom_hline(yintercept=0, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/tau))) +
    geom_hline(yintercept=1, linetype="dashed")

ggposterior(params, aes(x=experiment, y=sqrt(1/participant_tau))) +
    geom_hline(yintercept=1, linetype="dashed")

ggposterior(params, aes(x=experiment, y=b1)) +
    geom_hline(yintercept=0)


ggdensity(m3$params, aes(x=b1)) +
    stat_function(fun=function(x) dst(x, m3$b1_post[1], m3$b1_post[2], m3$b1_post[3]))
ggposterior(m3$params, aes(x=1, y=b1))

ggdensity(m3$params, aes(x=b2)) +
    stat_function(fun=function(x) dst(x, m3$b2_post[1], m3$b2_post[2], m3$b2_post[3]))
ggposterior(m3$params, aes(x=1, y=b2))

ggdensity(m3$params, aes(x=tau)) +
    stat_function(fun=function(x) dgamma(x, m3$tau_post[1], m3$tau_post[2])) 
ggposterior(m3$params, aes(x=1, y=tau))
ggposterior(m3$params, aes(x=1, y=sqrt(1/tau)))



ggdensity(m3$params, aes(x=participant_tau)) +
    stat_function(fun=function(x) dgamma(x, m3$participant_tau_post[1], m3$participant_tau_post[2]))
ggposterior(m3$params, aes(x=1, y=participant_tau))
ggposterior(m3$params, aes(x=1, y=sqrt(1/participant_tau)))


mean. = 0
sd. = 5
df. = 20
x_pre = bquote(dt(.(mean.), .(1/sd.^2), .(df.)))

fit = run.jags(metajags_model(x ~ R(x_pre))$code, sample=10000, monitor="x")
params = extract_samples(fit, x[])
tfit = fitdistr(params$x, dst, start=list(m=mean(params$x), s=sd(params$x), df=20))$estimate

ggdensity(params, aes(x)) +
    stat_function(fun=function(x) dst(x, 0, 5, 20)) +
    stat_function(fun=function(x) dst(x, tfit[[1]], tfit[[2]], tfit[[3]]), color="red")


shape = 10
rate = 20
x_pre = bquote(dgamma(.(shape), .(rate)))

fit = run.jags(metajags_model(x ~ R(x_pre))$code, sample=10000, monitor="x")
params = extract_samples(fit, x[])
gammafit = fitdist(params$x, "gamma", start=list(shape=1, rate=1), method="mge", gof="CvM")$estimate

ggdensity(params, aes(x)) +
    stat_function(fun=function(x) dgamma(x, shape, rate)) +
    stat_function(fun=function(x) dgamma(x, gammafit[[1]], gammafit[[2]]), color="red")
