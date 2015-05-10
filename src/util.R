### Helper functions for making nice plots

library(coda)
library(modeest)

#function for use with stat_summary that returns the median and
#highest density interval of the data
median_hdi = function(x, ...) {
    HPDinterval(mcmc(x), ...) %>%
        data.frame() %>%
        transmute(ymin = lower, ymax = upper) %>% 
        cbind(y = median(x, ...))
}

#function for use with stat_summary that returns the mode and
#highest density interval of the data
mode_hdi = function(x, ...) {
    HPDinterval(mcmc(x), ...) %>% 
        data.frame() %>% 
        transmute(ymin = lower, ymax = upper) %>% 
        cbind(y = parzen(x, ...))
}

#simple posterior violin plot
ggposterior = function(.data, .aes) {
    ggplot(
            .data,
            .aes
        ) + 
        geom_violin(linetype=0, fill="#cccccc") + 
        stat_summary(fun.data=mode_hdi, color="red", size=0.75) +
        coord_flip() 
}

ggdensity = function(.data, .aes) {
    ggplot(
            .data,
            .aes
        ) + 
        stat_density(linetype=0, fill="skyblue")  
}
