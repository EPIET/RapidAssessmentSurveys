//SESION 5_PREDICTORS OF VACCINATION COVERAGE

capture log close
set more off
set memory 10m

//Find the path of your working directory and change the working directory
//to your specified drive and directory.For example:
*cd "C:\your working directory path\"

log using "session5_predictors.log", replace

**********************************
*TASK 5.1-Merge datasets
**********************************

// merging datasets
use quest.dta, clear
merge id using vaccine5.dta, sort
//id specifies the matching unique id variable in the two datasets
//you use the option "sort" in the "merge" command, because "id" has to be sorted first 
tab _merge

//Response rate
//tab vaccrec
//tab qsrec
keep if _merge==3 

//Continue with vaccine8 for simplicity reasons
clear
use vaccine6, clear

*****************************************
*TASK 5.1-Creat, Recode, Label variables
******************************************

//Recoding and labelling variables. For example:
//Recode mother's age to mother's agegroups (mage; <25, 25-29, 30+ years)

gen mageg2=mage
recode mageg2 1/24=1 25/29=2 30/max=3
label def mageg2 1 "<25" 2 "25-29" 3 "30+", modify
label values mageg2 mageg
tab mageg2

//Create dichotomous variables (0/1) that indicate agreement or disagreement with statements 
//on perceived benefits or harms of vaccination and barriers to immunization. 
//For example:
gen  a1x1gr= a1x1
recode a1x1gr 1/2=1 3/4=0
tab a1x1 a1x1gr

//To create similar variables for other statements, you may use a "loop" (foreach) command:
foreach VAR of varlist a1x2-a1x12 {
gen `VAR'gr=`VAR' 
recode `VAR'gr 1/2=1 3/4=0
label def agree 1 "1-agree" 0 "0-disagree", modify
label values `VAR'gr agree
tab `VAR' `VAR'gr
}

//To create similar variables for perceived barriers to vaccination, you may use:
foreach VAR of varlist a3x1-a3x7 {
gen `VAR'gr=`VAR' 
recode `VAR'gr 1/2=0 3/4=1
label def barrier 0 "0-no/small problem" 1 "1-average/big problem", modify
label values `VAR'gr barrier
tab `VAR' `VAR'gr
}

********************************
*TASK 5.1-Weighted proportions
********************************

//Calculate weighted proportions (and corresponding 95%CI).For example:
svyset [pweight=weight], psu(school) strata(strata)
svy:tab a1x5g, obs ci percent format (%9.1f)deft

//Complete Table 5.1
summ mage
foreach VAR of varlist minority mageg educf osib a1posyn a1posg uncritical a1x1g a1x2g a1x5g a3x1g a3x2g a3x4g {
svy:tab `VAR' , obs ci percent format (%9.1f)deft
}


*************************************
*TASK 5.2-Predictors of vaccination
*************************************
cs vacful a3x4g

//To adjust for the effect of the sampling design, use:
svyset [pweight=weight], psu(school) strata(strata)
svy:tab a3x4g vacful, obs row ci percent format (%9.1f)

//To calculate weighted proportions and PR from the weighted proportions, you may use svy:glm command:
xi:svy:glm vacful a3x4g, family(binomial) link(log) eform 

xi:svy:glm vacful a3x4g, family(binomial) link(log) eform 

svy:tab minority vacful, obs row ci percent format (%9.1f) 
xi:svy:glm vacful i.minority, family(binomial) link(log) eform 

//Potential associations for complete vaccination
foreach VAR of varlist minority mageg educf osib a1posyn a1posg uncritical a1x1g a1x2g a1x5g a3x1g a3x2g a3x4g {
svy:tab `VAR' vacful, obs row ci percent format (%9.1f) 
xi:svy:glm vacful i.`VAR', family(binomial) link(log) eform 
}

//Potential associations for timely vaccination
foreach VAR of varlist minority mageg educf osib a1posyn a1posg uncritical a1x1g a1x2g a1x5g a3x1g a3x2g a3x4g {
svy:tab `VAR' vactime, obs row ci percent format (%9.1f) 
xi:svy:glm vactime i.`VAR', family(binomial) link(log) eform 
}

