##########################################.
## RAS session 4 Descriptive analysis ####
##########################################.

rm(list = ls())

source("N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/Useful R scripts/infe_functions.R")

# Packages required
required_packages <- c("foreign", "Hmisc", "dplyr", "ReporteRs", "survey", "gmodels")

for (i in seq(along = required_packages)) 
  library(required_packages[i], character.only = TRUE)


# Functions
big.table <- function(data) {
  count <- table(data)
  prop <- round(prop.table(count)*100, digits = 2)
  cbind(count,
        prop) 
}

label_table <- function(X){
  setFlexTableBorders(X,inner.vertical = borderProperties(style = "none"),inner.horizontal = borderProperties(style = "none"),outer.vertical = borderProperties(style = "none"),outer.horizontal = borderProperties(width = 2),body = T,header = T)
} 

label_footer <- function(X){
  setFlexTableBorders(X,inner.vertical = borderProperties(style = "none"),inner.horizontal = borderProperties(style = "none"),outer.vertical = borderProperties(style = "none"),outer.horizontal = borderProperties(style = "none"),footer = T)
}

svy_prop <- function(x, design) {
p1 <- round(svyciprop(as.formula(paste0( "~" , x)), design, na.rm = T) * 100, digits = 2)
p2 <- round(confint(p1) * 100, digits = 2)
p3 <- deff(round(svymean(as.formula(paste0( "~" , x)), design, na.rm = T, deff = T) * 100, digits = 2))
p4 <- cbind("Proportion" = p1, p2, "Design effect" = p3)
}



####################################.
#TASK 4.2-response rate-Table 4.1#####
####################################.

setwd("N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/EPIET/Learning R/R Case studies/RAS 2017/session4")

vaccine <- read.dta("vaccine4.dta", convert.factors = FALSE)


# View structure of your data set
str(vaccine)
summary(vaccine)
describe(vaccine)

# No. of school classes that participated
describe(vaccine$school)


# Response rate
big.table(vaccine$vaccrec)

# Compare respondents and non-respondents.
# You may use the describe command, to complete Table 4.1. For example:
summary(vaccine$age)
summary(vaccine$age[vaccine$vaccrec == 1])
summary(vaccine$age[vaccine$vaccrec == 0])


# To compare respondents and non-respondents, you may use:
# Student's t-test for comparison of means (for numeric variables)
ttestage <- t.test(age~vaccrec, var.equal = TRUE, data = vaccine)
ttestage2 <- c(ttestage$estimate, Pvalue = ttestage$p.value)    
ttestage2
                 

# Chi2 test for comparison of proportions:
genderresponse <- table(vaccine$gender, vaccine$vaccrec)
chisq.test(genderresponse) #  p-value is different to stata as the calculation is slightly different

# To complete Table 4.1, you may use:

vars <- c("gender", "urban", "minority", "country1")

output <- list()
for (var in vars) {
  total <- big.table(vaccine[,var])
  combo <- table(vaccine[,var], vaccine$vaccrec)
  prop <- round(prop.table(combo,2)*100,digits = 2)
  test <- chisq.test(combo)
  output[[var]] <- cbind(total, "Respondents (n)" = combo[,c(2)], "% respondents" = prop[,c(2)],"Non-respondents (n)" = combo[,c(1)], "% non-respondents" = prop[,c(1)], Pvalue = round(test$p.value, digits = 3))
}

output

output2 <- do.call(rbind, output[1:length(output)]) 

rownames(output2) <- c("Female", "Male", "Rural areas", "Urban areas", "General population", "Roma", "Greek Muslims", "Immigrants", "Other country", "Greece")
output2

# the p-value will never be identical due to the method used to calculate it

####################################.
#TASK 4.4-Sampling weights####
####################################.

# To create sampling weights for each stratum in your dataset, you may use the following:

popvar <- matrix(c(11,12,21,22,31,32,41,51,52,61,62,3282,1773,8609,3609,24311,6734,30345,12928,4813,3069,1132),
                     nrow = 11, ncol = 2, byrow = F,
                     dimnames = list(c(1:11), c("strata", "population")))


vaccine <- merge(vaccine, popvar[, c("strata", "population")], by = "strata", all.x = T)


# View(vaccine[,c("id","school", "strata","population")])

# The following will add a new variable called sample which is the total number of rows per strata
vaccine <- vaccine %>%
  dplyr::group_by(strata) %>%
  dplyr::mutate(sample = n())

vaccine$samplef <- vaccine$sample/vaccine$population

vaccine$weight1 <- 1/vaccine$samplef

# Create a data set that drops rows where vaccrec == 0
vacc_rec <- vaccine[vaccine$vaccrec == 1, ]


# Creating Table 4.4

# creates a table with unique values of population, sample, samplef and weight1 by strata
table1 <- vacc_rec[, c("strata", "population", "sample", "samplef", "weight1")] %>%
  group_by(strata) %>%
  distinct(population, sample, samplef, weight1)

table1

# creates a table with counts and proportions of mmr2yn (1 and 0) by strata
table2 <- vacc_rec[, c("strata", "mmr2yn")] %>%
  group_by(strata, mmr2yn) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(freq = round(n/sum(n) * 100, digits = 2))

table2
# Merges table1 and 2 but only takes value where mmr2yn = 1
table3 <- merge(table1, table2[table2$mmr2yn == 1, c("strata", "freq")], by = "strata")

# Rounds value of samplef
table3$samplef <- round(table3$samplef * 100, digits = 2)

## Adding additional row at end of table
table3 <- table3[1:12,]

# Label first cell in 12th row as Total
table3[12,1] <- "Total"

# Calculate sum of population and put value in 12th row, 2nd column
table3[12,2] <- colSums(table3[1:11, c("population"),drop = F])

# Obtain the unweighted proportion of MMR-2 vaccination coverage
unweighted <- prop.table(table(vacc_rec$mmr2yn))
unweighted  <- round(unweighted  * 100, digits = 2)
unweighted  <- as.data.frame(unweighted) 

# Incorporate this value into table 3
table3[12,6] <- unweighted[2,2]

table3

table3a <- FlexTable(table3,header.columns = F)
table3a <- addHeaderRow(table3a, text.properties = textBold(), value = c("Stratum", "Total number of 1st year pupils", "Number of pupils selected in sample", "Sampling fraction (%)", "Sampling weight", "MMR-2 vaccination coverage (%)"), colspan = c(1,1,1,1,1,1)) # this adds a new row with those headings and you specifiy over how many columns each heading should span

table3a <- label_footer(table3a) #  removed the label around the footer
table3a <- label_table(table3a) # formatted the table so that only the top and lower parts are neatly formatted

table3a

####################################.
# TASK 4.4- Weighted proportions manually####
####################################.

#Using formula 4.2 and information on Table 4.3, you should have:
((3282*0.6797) + (1773*0.6388) + (8609*0.7714))/(3282 + 1773 + 8609)

#For the 11 strata in the study, you could type:
((3282*0.6797) + (1773*0.6388) + (8609*0.7714) + (3609*0.7209) + (24311*0.7824) + (6734*0.8396) + (30345*0.7667) + (12928*0.7214) + (4813*0.6621) + (3069*0.8257) + (1132*0.75)) / (100605)


####################################.
# TASK 4.5- Estimated vaccination coverage ####
####################################.
# Need to vacful a factor and reorder in order to obtain proportion with CI
vacc_rec$vacful <- factor(vacc_rec$vacful, levels = c(1, 0))

simple <- prop.test(table(vacc_rec$vacful))

# Extract the proportion, confidence intervals from the simple table and 1 for the design effect
simple <- rbind(round(simple$estimate * 100, digits = 2),
                round(simple$conf.int[1] * 100, digits = 2),
                round(simple$conf.int[2] * 100, digits = 2),
                1)

# Make simple a data frame and use the transpose function to switch rows to columns
simple <- as.data.frame(t(simple))

# Change the names of simple data frame to match those of table 4
colnames(simple)[1:4] <- c("Proportion", "2.5%", "97.5%", "Design effect")


vacc_rec$vacful <- as.numeric(as.character(vacc_rec$vacful))


# Long way
design <- svydesign(ids = ~1, weights = ~weight1, data = vacc_rec)
a <- round(svyciprop(~vacful, design, na.rm = T) * 100, digits = 2)
b <- confint(a)
c <- deff(round(svymean(~vacful, design, na.rm = T, deff = T) * 100, digits = 2))
d <- cbind("Proportion" = a, "95% CI" = b, "Design effect" = c)
d

e <- svy_prop("vacful", design = design)
e

class(vacc_rec$vacful)
design2 <- svydesign(ids = ~school, weights = ~weight1, data = vacc_rec)
f <- svy_prop("vacful", design = design2)
f

design3 <- svydesign(ids = ~school, weights = ~weight1, strata = ~strata, data = vacc_rec)
g <- svy_prop("vacful", design = design3)


# Combine simple and table 4


table4 <- rbind(simple,e,f,g)
table4[, 2:4] <- round(table4[, 2:4], digits = 2)



#  Add in row labels
rownames(table4) <- c("Simple proportion", "+ sampling weight", "+ sampling weight + clustering", "+ sampling weight + clustering + stratification")


table4 <- FlexTable(table4,header.columns = F,  add.rownames = T)
table4 <- addHeaderRow(table4, text.properties = textBold(), value = c("",  "Fully vaccinated children %", "95% CI", "Design effect"), colspan = c(1,1,2,1)) # this adds a new row with those headings and you specifiy over how many columns each heading should span

table4 <- label_footer(table4) #  removed the label around the footer
table4 <- label_table(table4) # formatted the table so that only the top and lower parts are neatly formatted
table4



#Calculate the vaccination coverage and complete Table 4.5.2.
#For example DTP-3:

dtp <- svy_prop("dtp3yn", design = design3)
dtp

dtm <- svyby(~dtp3yn, ~minority, design3, svyciprop, vartype = "ci")
dtm <- round(dtm[,2:4] * 100, digits = 2)
dtm

dtu <- svyby(~dtp3yn, ~urban, design3, svyciprop, vartype = "ci")
dtu <- round(dtu[,2:4] * 100, digits = 2)
dtu

# Use the loop (foreach) command to estimate vaccination coverage for the other vaccines

vars <- c("dtp3yn", "dtp4yn", "dtp5yn", "mmr1yn", "mmr2yn", "hibprmyn", "hibfulyn", "hbv3yn", "mnc1yn", "pne1yn", "var1yn", "vacful", "vactime")

output <- list()
for (var in vars) {
  overall <- svy_prop(var, design = design3)
  output[[var]] <- overall
}

output

# Convert the list to a matrix to condense all the output
output2 <- do.call(rbind, output[1:length(output)]) 
output2 <- as.data.frame(output2)
output2$`2.5%` <- round(output2$`2.5%` * 100, digits = 3)
output2$`97.5%` <- round(output2$`97.5%` * 100, digits = 3)


# Making a loop using svyby with minority
vars <- c("dtp3yn", "vacful", "vactime")


output3 <- list()
  
for (var in vars) {
  a <- svyby(as.formula(paste0( "~" , var)), by = ~minority, design3, svyciprop, vartype = "ci", deff = T)
  colnames(a)[2:4] <- c("Proportion", "2.5%","97.5%")
  a <- round(a[,2:4]*100, digits = 1)
  output3[[var]] <- a
}  


output3 <- do.call(rbind, output3[1:length(output3)])


# Need to add an empty Design effect variable to be able to combine all tables later
output3$`Design effect` <- ""

# Making a loop using svyby with urban
vars <- c("dtp3yn", "vacful", "vactime")

output4 <- list()

for (var in vars) {
  b <- svyby(as.formula(paste0( "~" , var)), by = ~urban, design3, svyciprop, vartype = "ci")
  colnames(b)[2:4] <- c("Proportion", "2.5%","97.5%")
  b <- round(b[,2:4]*100, digits = 1)
  output4[[var]] <- b 
}  

output4 <- do.call(rbind, output4[1:length(output4)])


# Need to add an empty Design effect variable to be able to combine all tables later
output4$`Design effect` <- ""

# Combine the tables

finaltable <- rbind(output2[1,],
                    output3[1:4,],
                    output4[1:2,],
                    output2[2:12,],
                    output3[5:8,],
                    output4[3:4,],
                    output2[13,],
                    output3[9:12,],
                    output4[5:6,])


finaltable$`Design effect` <- as.numeric(finaltable$`Design effect`)

# Add appropriate rownames
rownames(finaltable) <- c("DTP-3 (overall)", "DTP-3 General population", "DTP-3 Roma", "DTP-3 Greek Muslims", "DTP-3 Immigrants", "DTP-3 Rural areas", "DTP-3 Urban areas", "DTP-4 (overall)","DTP-5 (overall)", "MMR-1 (overall)", "MMR-2 (overall)", "HiB- primary (overall)", "HiB- full (overall)", "HepB-3 (overall)", "MNC-1 (overall)", "PCV7-1 (overall)", "Var-1 (overall)", "Complete vaccination (overall)",  "Comp Vacc General population","Comp Vacc Roma", "Comp Vacc Greek Muslims", "Comp Vacc Immigrants", "Comp Vacc Rural areas", "Comp Vacc Urban areas", "Timely vaccination (overall)", "Timely vacc General population", "Timely vacc Roma", "Timely vacc Greek Muslims", "Timely vacc Immigrants", "Timely vacc Rural areas", "Timely vacc Urban areas")

# round values
finaltable[,2:4] <- round(finaltable[,2:4], digits = 2)

# convert NA values to "" 
finaltable[is.na(finaltable) == T ] <- "" 

table5 <- FlexTable(finaltable,header.columns = F, add.rownames = T)
table5 <- addHeaderRow(table5, text.properties = textBold(), value = c("",  "Weighted %", "95% CI", "Design effect"), colspan = c(1,1,2,1))
table5 <- label_footer(table5) #  removed the label around the footer
table5 <- label_table(table5) # formatted the table so that only the top and lower parts are neatly formatted
table5


# ####################################.
# ##TASK 4.6- Age of vaccination ####
# ####################################.
# # Use a loop to estimate the weighted proportion
# 
# vars <- c("hbv3by12", "dtp4by24", "mmr1by24", "mmr1yn", "mmr2yn", "hibprmyn", "hibfulyn", "hbv3yn", "mnc1yn", "pne1yn", "var1yn", "vacful", "vactime")
# 
# output <- list()
# for (var in vars) {
#   overall <- svy_prop(var, design = design3)
#   output[[var]] <- overall
# }
# 
# output
# 
# # Convert the list to a matrix to condense all the output
# output2 <- do.call(rbind, output[1:length(output)]) 
# 
# # This could be directly exported as is
# output2 <- as.data.frame(output2)

# lapply(vars, function(x) svyby(as.formula(paste0( "~" , x)), by = ~minority, design3, svyciprop, vartype = "ci"))
# 
# lapply(vars, function(x) svyby(as.formula(paste0( "~" , x)), by = ~urban, design3, svyciprop, vartype = "ci"))
