library(dplyr)
#expdatamns <- NULL
histogramExponentialDistribution <- function(seed, lamba,sims,n) {
  
  set.seed(seed = seed)
  expdata <- data.frame(x=rexp(n*sims,rate=lamba))
  grph <- ggplot(expdata,aes(x=x))+
    geom_histogram(alpha = .20, binwidth=.25, colour = "blue")+
    labs(x="Random exp (x)",y="Frequency")+
    theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = +1))+        
    ggtitle(expression(atop("Histogram representing the Exponential Distribution",
                            atop(italic("Simulated using 40,000 random exp values")))))
  grph
}

histogramSampleTheoreticalMeans <- function(seed, lamba,sims,n) {
  
  set.seed(seed = seed)
  mns = NULL
  for (i in 1 : sims) mns = c(mns, mean(rexp(40,rate=lamba)))
  expdatamns <- data.frame(xmns=mns)
  datamn <- round(mean(expdatamns$xmns),2)
  theoreticalMean <- round(1/lamba,2)
  
  #
  # Calculate Variance
  # 
  sampleVariance <- round(var(expdatamns$xmns),2)
  theta <- 1/lamba
  theoreticalVariance <- round(theta**2/n,2) # use the formula (theta**2)/n, where theta <- 1/lambda
  
  #
  # Calculate Stdev
  #
  sampleSTD <- round(sqrt(sampleVariance),2)
  theoreticalSTD <- round(sqrt(theoreticalVariance),2)
  
  #
  # Create the histogram and decorate with means, standard deviations
  #
  grph <- ggplot(expdatamns, aes(x = xmns)) + 
    geom_histogram(alpha = .20, binwidth=0.20, colour = "blue")+
    scale_x_continuous(breaks=2:8)+
    labs(x="Sample mean",y="Frequency")+
    theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = +1))+  
    
    geom_vline(xintercept = datamn, size=1.0, linetype=2, colour="green") +
    geom_vline(xintercept = theoreticalMean, size=1.0, linetype=2, colour="red") +
    
    geom_vline(xintercept = sampleSTD+datamn, size=1.0, linetype=2, colour="yellow") +
    geom_vline(xintercept = theoreticalSTD+theoreticalMean, size=1.0, linetype=2, colour="pink") +
    
    geom_vline(xintercept = datamn-sampleSTD, size=1.0, linetype=2, colour="yellow") +
    geom_vline(xintercept = theoreticalMean-theoreticalSTD, size=1.0, linetype=2, colour="pink") +
    
    annotate("text",x=7,y=25,label= "dashed green: sample mean\ndashed red: theoretical mean",
             size=3,color="black") +
    annotate("text",x=7,y=20,label= "dashed yellow: sample std\ndashed pink: theoretical std",
             size=3,color="black") +
    
    ggtitle(expression(atop("Histogram of Exponential Sample Means",
                            atop(italic("Sample size = 40, simulations = 1000"))))
            )
  
  print(grph)
  return(expdatamns)
}

histogramNormalDistribuion <- function(lamba = lamba, n = n, expdatamns = expdatamns) {
  stdev <- 1/lamba
  mn <- 1/lamba
  expdatamns<-mutate(expdatamns,nval=sqrt(n)*(expdatamns$xmns-mn)/stdev)
  
  grph <- ggplot(expdatamns, aes(x = nval)) + geom_histogram(alpha = .20, 
              binwidth=0.2, colour = "blue", aes(y = ..density..)) +
    stat_function(fun = dnorm, size = 2)+
    geom_vline(xintercept=mean(expdatamns$nval),size=1, colour="red", linetype=2)+
    labs(x="Normalized sample mean",y="Frequency")+
    theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = +1))+        
    ggtitle(expression(atop("Standard Normal Distribution of Exponential Distribution Sample Means",
                            atop(italic("Sample size = 40, simulations = 1000")))))
  grph
}