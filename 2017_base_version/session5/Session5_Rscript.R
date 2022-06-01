## Session 5 RAS

##Load required packages and functions####

required_packages <- c("foreign", "Hmisc", "dplyr", "survey", "gmodels", "epiR", "multcomp", "broom","leaps")

for (i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)
# Function to make tables with counts and proportions 
big.table <- function(data) {
  count <- table(data)
  prop <- round(prop.table(count)*100, digits = 2)
  cbind(count,
        prop) 
}

# Function used to calculate weighted proportions, CI and design effect for binary variables
svy_prop <- function(x, design) {
  p1 <- round(svyciprop(as.formula(paste0( "~" , x)), design, na.rm = T) * 100, digits = 2)
  p2 <- round(confint(p1)*100, digits = 2)
  p3 <- deff(round(svymean(as.formula(paste0( "~" , x)), design, na.rm = T, deff = T) * 100, digits = 2))
  p4 <- cbind("Proportion" = p1, "95% CI" = p2, "Design effect" = p3)
}


# Function used to calculate weighted proportions, CI and design effect for categorical variables
svy_prop_cat <- function(x, dataset, designids, designweight, designstrata){
  
  outer <- list() 
  
  dataset[,x] <- as.factor(dataset[,x])
  
  
  for (i in levels(dataset[,x])){
    dataset[,paste0(x, "temp")] <- ifelse(dataset[,x] == i, 1, 0)
    
    design <- svydesign(ids = as.formula(paste0("~",designids)), 
                        weights = as.formula(paste0("~",designweight)), 
                        strata = as.formula(paste0("~",designstrata)), 
                        data = dataset)
    overall <- svy_prop(paste0(var ,"temp"), design = design)
    
    rownames(overall) <- paste0(var,": ", i)
    
    outer[[var]][[i]] <- overall
  }
  do.call(rbind, outer[[1]][1:length(outer[[1]])])
} 






#Set working directory###########

setwd("C:/Users/Spina/Downloads/RAS 2017/Session5")

#Read datasets################

# quest <- read.dta("quest.dta", convert.factors = FALSE)
vaccine <- read.dta("vaccine5.dta", convert.factors = FALSE)

#Dont think need to merge shit....
# vaccine3 <- merge(vaccine, quest, by = "id", all.x = T)

#Task 5.1 Describe parental characteristics, beliefs towards vaccination#############

#Recode variables 

vaccine$mage2 <- cut(vaccine$mage,
                     breaks = c(0,24, 29, 100), 
                     labels = c("0-24", "25-29", ">30"))


vaccine$osib2 <- cut(vaccine$osib1, 
                     breaks = c(-1,0, 2,100),
                     labels = c("0", "1-2", ">3"))


#Weighted proportions 

design <- svydesign(ids = ~school, weights = ~weight, strata = ~strata, data = vaccine)



varsbin <- c("uncritical", "a1x1g", "a1x2g", "a1x5g", "a3x1g", "a3x2g", "a3x5g")

output <- list()
for (var in varsbin){
  overall <- svy_prop(var, design = design)
  output[[var]] <- overall
}

# Convert the list to a matrix to condense all the output
outputa <- do.call(rbind, output[1:length(output)]) 

# Convert the output to a data frame
outputa <- as.data.frame(outputa)




##Do the same for categorical variables 

varscat <- c("minority", "mage2", "osib2", "educf", "a1posg")



output2 <- list()
for (var in varscat){
  overall <- svy_prop_cat(var, dataset = vaccine, designids = "school", designweight = "weight", designstrata = "strata")
  output2[[var]] <- overall
}

# Convert the list to a matrix to condense all the output
output2a <- do.call(rbind, output2[1:length(output2)]) 

# Convert the output to a data frame
output2a <- as.data.frame(output2a)



#FIX THE MEANS#############

svymean(vaccine$mage, design = design, na.rm = T)




##Task 5.2 ##############################



#Calculate the prevalence ratios for 


design <- svydesign(ids = ~school, weights = ~weight, strata = ~strata, data = vaccine)


vars <- c("minority", "mageg", "educf", "osibg", 
          "a1posyn", "a1posg", "uncritical", 
          "a1x1g", "a1x2g", "a1x5g", "a3x1g", "a3x2g", "a3x5g")

output3 <- list()

for (var in vars) {
  
  form <- formula(paste0("vacful ~ factor(",var,")"))
  
  model <- svyglm(form,
                 data = vaccine,
                 family = quasibinomial(link = "log"),
                 design = design)
  
  model1 <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  output3[[var]] <- model1
}
















