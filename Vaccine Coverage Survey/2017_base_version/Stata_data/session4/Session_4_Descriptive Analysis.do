// session4.do-DESCRIPTIVE ANALYSIS
//Date 10/6/2010
//Vaccine Coverage case study, 12/12/2006

capture log close
// This closes any open log files. 

set more off
// This tells Stata not to pause or display the --more-- message. more causes STATA to display     
// -more-and pause until any key is pressed.

 set memory 10m
// This changes the amount of memory allocated to STATA

//Find the path of your working directory and change the working directory
//to your specified drive and directory.For example:
*cd "C:\your working directory path\"

//open a new log file from now on
log using "descriptive.log", replace

//Open file
use vaccine4, clear

**********************************
*TASK 4.2-response rate-Table 4.1
**********************************

codebook school
//342 school classes participated

//Response rate
tab vaccrec

//Compare respondents and non-respondents.
//You may use the tabulate and summarize command, to complete Table 4.1. For example:
summ age if vaccrec==1
summ age if vaccrec==0

//To compare respondents and non-respondents, you may use:
//Student's t-test for comparison of means (for numeric variables)
 ttest age, by(vaccrec)
//Chi2 test for comparison of proportions: 
tab gender vaccrec, chi

//To complete Table 4.1, you may use:
foreach VAR of varlist gender urban minority country1 {
tab `VAR'
tab `VAR' vaccrec, col chi 
}



*****************************
//Main outcomes*
*****************************
*Compete vaccination status
//gen vacful=0 
//replace vacful=1 if (dtp5yn==1 & po5ful==1 & mmr2yn==1 & hibfulyn==1 & hbv3yn==1)
//replace vacful=. if (dtp5yn==. | po5ful==. | mmr2yn==. | hibfulyn==. | hbv3yn==.)
tab vacful, missing

*Age-appropriate vaccination status
//gen vactime=.
//replace vactime=1 if (dtp3by12==1 & dtp4by24==1  & po3by12==1 & po4by24==1 & mmr1by24==1  & hibprm12==1 & hbv3by24==1 )
//replace vactime=0 if (dtp3by12==0 | dtp4by24==0 | po3by12==0 | po4by24==0  | mmr1by24==0  | hibprm12==0 |hbv3by24==0 )
//replace vactime=. if (dtp3by12==. | dtp4by24==.  | po3by12==. | po4by24==.  | mmr1by24==. | hibprm12==. |hbv3by24==. )



**********************************************
*TASK 4.4- Sampling weights
**********************************************
drop if vaccrec==0  

//To create sampling weights for each stratum in your dataset, you may use the following:
gen     population=3282 if strata==11
replace population=1773 if strata==12
replace population=8609 if strata==21
replace population=3609 if strata==22
replace population=24311 if strata==31
replace population=6734 if strata==32
replace population=30345 if strata==41
replace population=12928 if strata==51
replace population=4813 if strata==52
replace population=3069 if strata==61
replace population=1132 if strata==62

sort strata
by strata: gen sample=_N 
by strata: gen samplef=sample/population
gen weight1=1/samplef

*******************************************
*TASK 4.5- Weighted proportions manually*
*******************************************

//You can use the tab command first
sort strata
by strata: tab mmr2yn
tab mmr2yn

//Using formula 4.2 and information on Table 4.3, you should have:
display (((3282*0.6797)+(1773*0.6388)+(8609*0.7714))/(3282+1773+8609))

//For the 11 strata in the study, you could type:
di(((3282*0.6797)+(1773*0.6388)+(8609*0.7714)+(3609*0.7209)+(24311*0.7824)+(6734*0.8396)+(30345*0.7667)+(12928*0.7214)+(4813*0.6621)+(3069*0.8257)+(1132*0.75))/ (100605))

*******************************************
*TASK 4.5- Estimate vaccination coverage*
*******************************************
//To calculate the weighted proportion and 95% CIs (taking into account the sampling weights): 
//First, tell STATA which sampling weights you used:
svyset, clear
svyset [pweight=weight]
//Use svy:tab command
svy:tab vacful, ci deff

//To estimate the proportion of children that were fully vaccinated, allowing for the weights and clustering:
//First, remove the previous settings:
svyset, clear
//Tell STATA that you used sample weights and cluster sampling:
svyset [pweight=weight], psu(school)
//Use svy:tab command:
svy: tab vacful, ci deff

//To estimate the proportion of children that were fully vaccinated, allowing for the weights clustering and stratification:
//Tell STATA that you used sample weights and stratified cluster sampling:
svyset [pweight=weight], psu(school) strata(strata)
svy:tab vacful, ci deff

//Calculate the vaccination coverage and complete Table 4.5.2.
//For example DTP-3:

svyset [pweight=weight], psu(school) strata(strata)
svy:tab dtp3yn, obs ci deff
svy:tab minority dtp3yn, obs row ci percent deff format(%9.2f) 
svy:tab urban dtp3yn, obs row ci percent deff format(%9.2f)

//Use the loop (foreach) command to estimate vaccination coverage for the other vaccines

foreach VAR of varlist dtp4yn dtp5yn mmr1yn mmr2yn hibprmyn hibfulyn hbv3yn mnc1yn pne1yn var1yn vacful vactime {
svyset [pweight=weight], psu(school) strata(strata)
svy:tab `VAR', obs ci deff
svy:tab minority `VAR',obs row ci percent deff format(%9.2f) 
svy:tab urban `VAR', obs row ci percent deff format(%9.2f)
}

*******************************************
*TASK 4.6- Age of vaccination*
*******************************************

//Use the command svy:tab to estimate the weighted proportion. 
foreach VAR of varlist hbv3by12 dtp4by24 mmr1by24 {
svy:tab `VAR',obs col ci percent format(%9.2f)deff
}

//Cumulative frequency plots for the distribution of age of vaccination
//Use the downloadable command called distplot.
//There are 2 options:
//If you have internet access, type findit distplot and download the ado and help file.
//If you do not have internet access, you may copy the “distplot.ado” file from “session 6” folder to the “personal” folder. To find where the “personal” folder is in your computer, type sysdir.
sysdir

//To create a cumulative frequency plot for the distribution of age of vaccination for MMR-1, DTP-4 and HepB-3, you may use:
distplot hbv3age mmr1age dtp4age,xla(12 24 36 48 60 72 84 96)
//Create cumulative frequency plots for the distribution of age of vaccination for HepB-3 for each minority group, you may use:
distplot hbv3age,by(minority) xla(12 24 36 48 60 72 84 96) yla(.2 .4 .6 .8 1)

**********************************************************
*TASK 4.7- Types of practices administering the vaccines*
**********************************************************

//To estimate the weighted proportion of practices administering MMR-1, allowing for the weights, stratification and clustering, you may use:
svy:tab mmr1plc,obs col ci percent format(%9.2f)deff
//Estimate the weighted proportion of practices administering MMR-1 by minority group, allowing for the weights, stratification and clustering, you may use:
svy:tab mmr1plc minority,obs col ci percent format(%9.2f)deff

capture log close



