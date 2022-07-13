########## Assignment 1: Solution
# Creator: Alex Hoagland, alcobe@bu.edu
# Created: 6/18/2022
# Last modified: 6/18/2022
#
# PURPOSE
#   Solutions for Assignment 1
#
# NOTES: 
#   - uses the Tidyverse package and Dplyr
################################################################################


##### Packages #####
#' Display name info
name <- Sys.info()
name[7]

# install.packages('tidyverse') # if needed, install the package
library(tidyverse) # call the relevant library
library(faux) # Useful package for simulating data
library(modelsummary) 
library(causaldata)
library(here)

# Load the data
library(readxl)
here::i_am("Assignments for 2021/Assignment1/Assignment1_Solution.R")
Dataset_1 <- read_excel(here("Assignments for 2021/Assignment1/Dataset 1.xlsx"))
##########


##### 1.-2. DAG -- multiple answers acceptable, not shown here ####
print("There are multiple acceptable answers for (1) and (2). I will skip these here.")
#####


##### 3. Summary Statistics ####
Dataset_1$HXPC2005 <- as.numeric(Dataset_1$HXPC2005) # There is a problem with HXPC not reading as numeric. Need to fix. 

library(psych)
sumtable <- data.frame(describe(Dataset_1[,c('LEBF20052','GDPPCUS2005','HXPC2005','TotFertRate2005',
                                               'PctUrb2005','PopGr2005')], 
                          fast=TRUE, na.rm=TRUE))
sumtable <- sumtable[,-c(1,7)] # Drop the vars and range columns
htmlTable::htmlTable(format(sumtable, digits = 3),
                     header=c("N","Mean","Standard Deviation", "Minimum", "Maximum", "Standard Error"),
                     rnames=c("LEBF", "GDPPC", "HXPC", "Total Fertility Rate", "% Urban", "Population Growth"),
                     caption="Summary statistics: Based on 2005 Data.") 
print("Note that the same size is not great here. Otherwise, there appears to be good variation on all variables, units look good, etc.")
##################################


##### 4. Univariate Regression ####
m1 <- lm(LEBF20052 ~ HXPC2005, data=Dataset_1)
msummary(list(m1),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "GDPPCUS2005" = "GDPPC"),
         gof_omit = 'AIC|BIC|RMSE') 
print("Regression notes: The effect of an increase in HXPC on LEBF is a precise 0---there is no estimated impact of GDPPC on LEBF.")
#################################


##### 5. Multivariate Regression ####
m2 <- lm(LEBF20052 ~ GDPPCUS2005 + HXPC2005, data=Dataset_1)
msummary(list(m1,m2),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "GDPPCUS2005" = "GDPPC", "HXPC2005"= "HXPC", "TotFertRate2005" = "Total Fertility Rate"),
         gof_omit = 'AIC|BIC|RMSE') 
print("Controlling for GDP per capita eliminates the relationship between HXPC and LEBF, but there is still a measurement error here.") 
###############################


##### 6. Transformations #####
hist(Dataset_1$LEBF20052) # Note that there is no skewness in LEBF, so no need for a transformation there
hist(Dataset_1$GDPPCUS2005) # Lots of skewness here, recommend a log transform
hist(Dataset_1$HXPC2005) # Lots of skewness here, recommend a log transform

# Transform the data
Dataset_1 <- Dataset_1 %>% mutate(ln_gdppc = log(GDPPCUS2005),
                                  ln_hxpc = log(HXPC2005))


# New regression
m3 <- lm(LEBF20052 ~ ln_gdppc + ln_hxpc, data=Dataset_1)
msummary(list(m1,m2,m3),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "ln_gdppc" = "ln(GDPPC)", "ln_hxpc"= "ln(HXPC)", 
                       "TotFertRate2005" = "Total Fertility Rate"),
         gof_omit = 'AIC|BIC|RMSE') 
print("After transforming the data, the results start to become more interpretable. Now, increasing GDP per capita by 10% is associated with almost a 1/2 year increase in life expectancy (0.41). However, there is no clear association between health expenditures and LEBF once we control for GDPPC.")
###############################


##### 7. Geographic Dummies #####
# Read in crosswalk
crosswalk <- read_excel(here("Assignments for 2021/Assignment1/Country-Continent_Crosswalk.xlsx"))

# Merge in info on continents and create dummies
Dataset_1 <- Dataset_1 %>% left_join(crosswalk, by=c("Country"))
Dataset_1 <- Dataset_1 %>% 
  mutate(con_Africa = (Continent == "Africa"),
         con_Asia = (Continent == "Asia"),
         con_Europe = (Continent == "Europe"),
         con_Oceania = (Continent == "Oceania"),
         con_SA = (Continent == "South America"))

# New regression
m4 <- lm(LEBF20052 ~ ln_gdppc + ln_hxpc + con_Africa + con_Asia + con_Europe + con_Oceania + con_SA, data=Dataset_1)
msummary(list(m1,m2,m3,m4),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "ln_gdppc" = "ln(GDPPC)", "ln_hxpc"= "ln(HXPC)", 
                       "TotFertRate2005" = "Total Fertility Rate",
                       "con_AfricaTRUE" = "Africa", "con_AsiaTRUE" = "Asia", "con_EuropeTRUE" = "Europe",
                       "con_OceaniaTRUE" = "Oceania", "con_SATRUE" = "South America"),
         gof_omit = 'AIC|BIC|RMSE') 
print("LEBF is significantly lower in African countries than in the rest of the world; no other significant differences are visible from this regression.")
###############################


##### 8. Interaction Terms ######
Dataset_1 <- Dataset_1 %>% mutate(interaction = ln_hxpc * con_Africa)

# New regression
m5 <- lm(LEBF20052 ~ ln_gdppc + ln_hxpc + con_Africa + con_Asia + con_Europe + con_Oceania + con_SA + interaction, data=Dataset_1)
msummary(list(m1,m2,m3,m4,m5),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "ln_gdppc" = "ln(GDPPC)", "ln_hxpc"= "ln(HXPC)", 
                       "TotFertRate2005" = "Total Fertility Rate",
                       "con_AfricaTRUE" = "Africa", "con_AsiaTRUE" = "Asia", "con_EuropeTRUE" = "Europe",
                       "con_OceaniaTRUE" = "Oceania", "con_SATRUE" = "South America",
                       "PctUrban2005" = "% Urban", "inter_hxpc_urban" = "HXPC * % Urban"),
         gof_omit = 'AIC|BIC|RMSE') 
print("Given the results in 7, it may be the case that health expenditures are high-return in areas with the lowest LEBF; hence, our interaction term looks at if increasing HXPC in African countries might improve LEBF. However, our regression still suggests no evidence that increasing health expenditures per capita is associated with lowering LEBF.")
###############################


###### 9. Identification Problems ######
print("The main identification problem in this instance is that GDPPC and HXPC are so tightly correlated, there is not enough varaition in one without the other to correctly identify causal relationships.")
ggplot(data=Dataset_1,aes(x=ln_hxpc)) + geom_point(aes(y=ln_gdppc)) + 
  theme_minimal() + labs(x="Ln(Health Expenditures per Capita)",y="Ln(GDPPC)",title="Correlation between X variables leaves backdoors open")
###############################


##### 10. Standard Errors #####
library(miceadds)

# Compare robust standard errors and standard errors clustered by continent
msummary(list(m5,m5,m5),
         vcov=c("classical","robust",~Continent),
         stars=c('*' = .1, '**' = .05, '***' = .01),
         fmt=2,
         statistic = c("s.e. = {std.error} (p = {p.value})","conf.int"),
         conf_level=.95,
         coef_rename=c("(Intercept)" = "Intercept", "ln_gdppc" = "ln(GDPPC)", "ln_hxpc"= "ln(HXPC)", 
                       "TotFertRate2005" = "Total Fertility Rate",
                       "con_AfricaTRUE" = "Africa", "con_AsiaTRUE" = "Asia", "con_EuropeTRUE" = "Europe",
                       "con_OceaniaTRUE" = "Oceania", "con_SATRUE" = "South America",
                       "PctUrban2005" = "% Urban", "inter_hxpc_urban" = "HXPC * % Urban"),
         gof_omit = 'AIC|BIC|RMSE') 
print("With robust standard errors, the impact of GDP on LEBF is no longer significant at the 90% confidence level. When clustering at the continent level, this result becomes more significant, and new continent relationships in LEBF emerge.") 
###############################