#' ---
#' title: "Tutorial 1: Handling Data with tidyverse"
#' output: pdf_document
#' author: "Takoua Jendoubi"
#' date:   22 May 2020
#' ---
#' 

#' Libraries
library(readr)
library(tidyr)

#' # Import data
#' First column name might be invalid. Do not hesitate to remove it by hand in the file. 
#' Column name will be replaced by X1 using read_csv function.
indiv_data = read_csv(file="COVID19_open_line_list.csv", 
                      col_types= cols(data_moderator_initials = col_character()))

#' Now we have a tibble with 13,174 observations x 33 variables
indiv_data

#' # Age as a continuous variables
#' Let's now have a look at the age variable. 
table(indiv_data$age)
sum(is.na(indiv_data$age)) #looking at missing values: 11614
 
#' We have few options: 
#' Consider age as i) a categorical variable or ii) a continuous variable.
#' If we decide to go for "the categorical variable" option, there will be less missing data but a loss in precision.
#' If we decide to go for "the continuous variable" option, we will end up with more missing data on one hand but 
#' more precise level of measurements on the other hand.
#' ## Age as a continuous variable
#' We now need to carefully handle our variables. "Aug-68" and "Oct-19" are birthdates.
#' We will use these to compute age for the coresponding individuals.
indiv_data$age [indiv_data$age == "Aug-68"] = "51"
indiv_data$age [indiv_data$age == "Oct-19"] = "0.66"
#' construct a new variable
indiv_data$age_cont = as.numeric(as.character(indiv_data$age))
plot(table(indiv_data$age_cont))
sum(is.na(indiv_data$age_cont)) # This variable now has 12157 missing values

#' # Age as a categorical variable
#' Different options here. We can choose different categories and different numbers of categories. For instance here we can 
#' categorize into older than 30 years and younger than 30 years.
categorize = function(x) ifelse(length(x)==2, ifelse(x[1]>30,
                                                     ifelse(x[2]>30, TRUE, NA),
                                                     ifelse(x[2]<30, FALSE, NA)),
                                x[1]>30)

indiv_data$age_dis = sapply(strsplit(indiv_data$age, "-"), categorize)
plot(table(indiv_data$age_dis)) # This is highly imbalanced!
sum(is.na(indiv_data$age_dis)) # 11752 missing values

#' It's your turn now!
#' Have a look at other variables you might be intrested in in this data. 
#' You might also have a look at other datasets to clean and set ready for the analysis.



