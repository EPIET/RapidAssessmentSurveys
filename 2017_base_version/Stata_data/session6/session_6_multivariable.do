//SESION 6_MULTIVARIABLE ANALYSIS

capture log close
set more off
set memory 10m

//Find the path of your working directory and change the working directory
//to your specified drive and directory.For example:
*cd "C:\your working directory path\"

log using "session8_multivariable.log", replace

**********************************
*TASK 6.1-Logistic Regression
**********************************
use vaccine6.dta, clear

//Logit and logistic command
logit vacful a1posy
di exp(0.8169064)
logistic vacful a1posy
xi:logistic vacful i.minority
char minority [omit] 3


//Likelihood Ratio Test
xi:logistic vacful i.minority osibling gender
estimates store full_model
xi:logistic vacful i.minority osibling if e(sample)
estimates store reduced_model
lrtest full_model reduced_model

//Automated stepwise regression
sw, pe(0.25):logistic vacful minority  osibling  mageg  educf a1x5g a1x7g a1posy a3x1g a3x6g
sw, pe(0.05):logistic vacful minority  osibling  mageg  educf a1x5g a1x7g a1posy a3x1g a3x6g

sw, pr(0.05):logistic vacful minority  osibling  mageg  educf a1x5g a1x7g a1posy a3x1g a3x6g

//Perform logistic regression using the weights and allowing for the clustering, 
//using the command svylogit and then test or testparm to perform adjusted Wald tests.

xi:logistic vacful i.minority i.osibling  i.mageg  i.educf [pweight=weight], cluster(school)
testparm _Iminority_*
testparm _Iosibling_*
testparm _Ieducf_*
testparm _Imageg_*

//Final models
xi:logistic vacful  i.minority  i.osibling  i.mageg  a1x5g  a3x1g [pweight=weight], cluster(school)
xi:logistic vactime  i.minority   i.osibling   i.educf  a3x1g [pweight=weight], cluster(school)

**********************************
*TASK 6.2-Binomial Regression
**********************************
//For comlete vaccination
xi:binreg vacful  i.minority  i.osibling  i.mageg  a1x5g  a3x1g [pweight=weight], cluster(school)  rr
//OR
xi:svy:glm vacful i.minority  i.osibling  i.mageg  a1x5g  a3x1g, family(binomial) link(log) eform   

//For timely vaccination
xi:binreg vactime  i.minority   i.osibling   i.educf  a3x1g [pweight=weight], cluster(school) rr
xi:svy:glm vactime i.minority   i.osibling   i.educf  a3x1g , family(binomial) link(log) eform   

log close
