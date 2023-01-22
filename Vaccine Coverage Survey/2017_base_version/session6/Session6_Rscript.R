# Rapid assessment and survey Module
# Athens 2017
# Session 6


rm(list = ls())

# Packages required
required_packages <- c("foreign", "epiR", "multcomp", "broom", "leaps", "survey")

install.packages(required_packages)

for (i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)


#Case-study: Childhood Vaccination coverage survey in Greece, 2006 

# Functions
big.table <- function(data) {
  count <- table(data)
  prop <- round(prop.table(count)*100, digits = 2)
  cbind(count,
        prop) 
}

####################################.
# TASK 6.1- Logistic Regression####
####################################.

setwd("N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/EPIET/Learning R/R Case studies/RAS 2017/Session6")

vaccine6 <- read.dta("vaccine6.dta", convert.factors = FALSE)

# Logit and logistic command
model1 <- glm(vacful~a1posyn,
              data = vaccine6,
              family = binomial(link = "logit"))

model1op <- tidy(model1, conf.int = TRUE)
model1op

# Find the number of observations included in the model
nobs(model1)

model1exp <- tidy(model1, exponentiate = TRUE, conf.int = TRUE)
model1exp


model2 <- glm(vacful~factor(minority),
              data = vaccine6,
              family = binomial(link = "logit"))

model2op <- tidy(model2, exponentiate = TRUE, conf.int = TRUE)
model2op

# See the number of levels in the variable
big.table(vaccine6$minority)

# Change the reference level to 4th level of the variable ( !: reference designates the index and not the value)
vaccine6$minority2 <- relevel(factor(vaccine6$minority), ref = 4)


model3 <- glm(vacful~factor(minority2) + osibling + gender,
              data = vaccine6,
              family = binomial(link = "logit"))

model3op <- tidy(model3, exponentiate = TRUE, conf.int = TRUE)
model3op


# We want to use the same number of observations in model with gender as model without. We create esample (sample identifier), a set of row names which can be used to subset the corresponding dataframe
#
esample <- rownames(as.matrix(resid(model3)))
# this creates a matrix out of the residuals of model 3 with the column being the residuals for each observation in model 3
a <- as.matrix(resid(model3))


#Obtained from
#https://theesspreckelsen.wordpress.com/2016/08/10/estimation-sample-information-from-linear-regression-in-r-using-lm-aka-statas-esample/

model4 <- glm(vacful~factor(minority2) + osibling,
              data = vaccine6[esample,],
              family = binomial(link = "logit"))

model4op <- tidy(model4, exponentiate = TRUE, conf.int = TRUE)
model4op


#  Then test for the difference in the 2 models using anova test
anova(model3, model4, test = "Chisq")


# Automated stepwise regression
#http://www.stat.columbia.edu/~martin/W2024/R10.pdf 

# vars <- c("minority", "osibling", "mageg", "uncritical", "educf", "a1x1g", "a1x5g", "a1x7g", "a1posyn", "a3x1g", "a3x2g", "a3x5g", "a3x6g", "vacful")
# 
# vaccine6a <- vaccine6
# 
# for (var in vars) {
#   vaccine6a <- vaccine6a[!is.na(vaccine6a[,var]),] #Only keep the values of vaccine6new that are NOT equal to NA
# }
# 
# nrow(vaccine6a)
# # Should have 2583 observations after this loop


# Drop observations with missing data 
# List all the variables for which we want to include in our final regression model and for which we need to drop the missing values
vars <- c("minority2", "osibling", "mageg", "a1x5g", "a3x1g", "vacful")

# nrow(vaccine6[is.na(vaccine6$minority) == F & is.na(vaccine6$osibling) == F & is.na(vaccine6$mageg) == F & is.na(vaccine6$a1x5g) == F & is.na(vaccine6$a3x1g) == F & is.na(vaccine6$vacful) == F,])

# Create a new data set and assign the original data set to it!
vaccine6new <- vaccine6

for (var in vars) {
  vaccine6new <- vaccine6new[!is.na(vaccine6new[,var]),] #Only keep the values of vaccine6new that are NOT equal to NA
}

nrow(vaccine6new)
# Should have 3007 observations after this loop

design2 <- svydesign(ids = ~school, weights = ~weight, strata = ~strata, data = vaccine6new)


finalmodel <- svyglm(vacful~factor(minority2) + factor(osibling) + factor(mageg) + a1x5g + a3x1g,
                     data = vaccine6new,
                     family = quasibinomial(link = "logit"),
                     design = design2)

finalmodelop <- tidy(finalmodel, exponentiate = TRUE, conf.int = TRUE)
finalmodelop

nobs(finalmodel)

####################################.
# TASK 6.2- Binomial Regression####
####################################.


bin1 <- svyglm(vacful~factor(minority2) + factor(osibling) + factor(mageg) + a1x5g + a3x1g,
               data = vaccine6new,
               family = quasibinomial(link = "log"),
               design = design2)

bin1op <- tidy(bin1, exponentiate = TRUE, conf.int = TRUE)
bin1op 






