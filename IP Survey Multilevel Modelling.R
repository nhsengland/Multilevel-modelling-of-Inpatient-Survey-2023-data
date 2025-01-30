#Use multilevel modelling to analyse results of inpatients survey by demographic variables
#Response variable is mean score for each question, grouped by question number (level 1) and trust (level 2)
#Independent variables are proportion of respondents in each demographic group for each trust - we don't have demographic data at individual level
#Choice of variables largely down to those most correlated with question responses (analysis using Excel), with combinations and calculated variables based on what seems sensible

# Install and load packages
if(!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(readxl, openxlsx, here, lubridate, Rcpp, NHSRplotthedots, officer, rstudioapi,
               flextable, dplyr , tidyverse, odbc, DBI, RMySQL, zoo, ggthemes, ggplot2)
if(!require("lme4")) install.packages("lme4");library(lme4)
if(!require("lmerTest")) install.packages("lmerTest");library(lmerTest)

#define list of independent variables
independent_vars <- c(
                      "age3_tpc"                                                                              #Age 51-65
                      ,"age4_tpc"                                                                             #Age 66+
                      ,"eth1_tpc"                                                                             #White
                      ,"eth3_tpc"                                                                             #Asian
                      ,"eth4_tpc"                                                                             #Black
                      ,"eth6_tpc"                                                                             #Ethnicity not known
                      ,"non_white"                                                                            #Non-white ethnicity - calculated field
                      ,"gender_1_tpc"                                                                         #Gender same as sex at birth
                      ,"gender_3_tpc"                                                                         #Gender prefer not to say
                      ,"religion_1_tpc"                                                                       #No religion
                      ,"religion_4_tpc"                                                                       #Hindu
                      ,"religion_6_tpc"                                                                       #Muslim
                      ,"religion_7_tpc"                                                                       #Sikh
                      ,"religion_9_tpc"                                                                       #Religion prefer not to say
                      ,"non_christian_religion"                                                               #Religion (stated) other than Christian - calculated field
                      ,"any_religion"                                                                         #Any stated religion - calculated field
                      ,"sexuality_1_tpc"                                                                      #Heterosexual
                      ,"sexuality_5_tpc"                                                                      #Sexuality prefer not to say
                      ,"sample_route_admission_emergency_tpc"                                                 #Emergency admission
                      ,"sexatbirth1_tpc"                                                                      #Male at birth
                      ,"age3_tpc * age4_tpc"                                                                  #Age 51-65 and 66+
                      ,"eth1_tpc * eth3_tpc * eth4_tpc * eth6_tpc"                                            #Combination of ethnicities
                      ,"religion_1_tpc * religion_4_tpc * religion_6_tpc * religion_7_tpc * religion_9_tpc"   #Combination of religions
                      ,"age4_tpc * non_white"                                                                 #Age 66+ and non-white
                      )

#create empty lists to store summary statistics and random intercepts
summary_statistics<-list()
intercepts<-list()

#get the data from Excel
data <- read.xlsx(paste0(getwd(),"/IP SURVEY DATA 2023 for multilevel modelling.xlsx"),sheet = "Data for R")

#loop over each independent variable
for (var in independent_vars) {

  #run the model: independent variable is proportion of respondents with the selected characteristic, response variable is mean score for each question
  formula <- as.formula(paste("Mean_score ~", var, "+ (1 | trustname)"))
  model <- lmer(formula, data = data)

#get summary statistics and random intercepts
summary_statistics[[var]]<-summary(model)

#get random intercepts
intercepts[[var]]<-data.frame(ranef(model))
}
