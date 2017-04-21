library(dplyr)
histogramExponentialDistribution <- function(seed, lamba,sims,n) {
  
  set.seed(seed = seed)
  expdata <- data.frame(x=rexp(n*sims,rate=lamba))
  grph <- ggplot(expdata,aes(x=x))+
    geom_histogram(alpha = .20, binwidth=.5, colour = "black")+
    labs(x="Random exp (x)",y="Frequency")+
    theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = +1))+        
    ggtitle(expression(atop("Histogram representing the Exponential Distribution",
                            atop(italic("Simulated using 40,000 random exp values")))))
  grph
}