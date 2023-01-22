
####################################
# Function to calculate sample size assuming simple random sampling 
#
# Note: this function gives the equivalent output of the sampsi 
#       command in STATA
#
# Author: Slawa Rokicki
# Contact: slawa.rokicki@gmail.com
# date: 25.06.2013

# Obtained from http://rforpublichealth.blogspot.co.at/2013/06/sample-size-calculations-equivalent-to.html

sampsi.prop<-function(p1, p2, ratio=1, power=.90, alpha=.05, cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
  # Description:
  # function to calculate sample size assuming simple random sampling
  #
  # Args:   p1:         proportion 1
  #         p2:         proportion 2 or alternative proportion
  #         ratio:      
  #         power:      denotes the power of the study, this is set as 90% as default, so always set it to power = 0.5 when you are calculating the sample size in a survey
  #         alpha:      denotes the significance level
  #         cont.corr:  denotes whether or not continuity correction should be applied. This is applied as standard when using sampsi in Stata.
  #         two.sided:  denotes whether a two-sided test should be performed or not
  #         one.sample: denotes whether one sample within a population is being examined or not
  
  effect.size<-abs(p2-p1)
  avg.p<-(p1+ratio*p2)/(ratio+1)
  sd=ifelse(one.sample==FALSE, sqrt(ratio*p1*(1-p1)+p2*(1-p2)), sqrt(p2*(1-p2)))
  
  z.pow<-qt(1-power, df=Inf, lower.tail=FALSE)
  z.alph<-ifelse(two.sided==TRUE, qt(alpha/2, df=Inf, lower.tail=FALSE), qt(alpha, df=Inf, lower.tail=FALSE))
  ct<-(z.pow+z.alph)
  
  n1<-(z.alph*sqrt((ratio+1)*avg.p*(1-avg.p))+z.pow*sd)^2/(effect.size^2*ratio)
  n1.cont<-ifelse(cont.corr==FALSE, n1, (n1/4)*(1+sqrt(1+(2*(ratio+1))/(n1*ratio*effect.size)))^2)
  
  n<-(((z.alph*sqrt(p1*(1-p1)))+z.pow*sd)/effect.size)^2
  
  if(one.sample==FALSE){
    col1<-c("alpha", "power", "p1", "p2", "effect size", "n2/n1", "n1", "n2")
    col2<-c(alpha,  power, p1, p2, effect.size, ratio, ceiling(n1.cont), ceiling(n1.cont*ratio))
  }
  else{
    col1<-c("alpha", "power", "p", "alternative", "n")
    col2<-c(alpha, power, p1, p2, ceiling(n))
  }
  ret<-as.data.frame(cbind(col1, col2))
  ret$col2<-as.numeric(as.character(ret$col2))
  colnames(ret)<-c("Assumptions", "Value")
  
  description<-paste(ifelse(one.sample==FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of proportions", ifelse(cont.corr==FALSE, "without", "with"), "continuity correction")
  
  retlist<-list(description, ret)
  
  return(retlist)
}

# Example of use

# sampsi.prop(0.85, 0.81,power = 0.5, alpha = 0.05, cont.corr = TRUE, two.sided = TRUE, one.sample = TRUE)
