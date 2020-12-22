# 304 cleaning document for the final project 
# Jack Edward Smith 
# 1005034400
# Date December 17 2020



# if you do not have the cesR installed please do 

#devtools::install_github("hodgettsp/cesR")



# download any packages you do not have 

# these are the packages used to clean the data 

library(cesR)
library(devtools)
library(tidyverse)

library(labelled)

library(haven)
library(dplyr)
library(plyr)


# CES CLEAN


get_ces("ces2019_web") # get the CES data 

ces2019web <- to_factor(ces2019_web)

#head(ces2019web)#show the head of the data if you want 

# reduce to variables of needed 
reduced_ces <-
  ces2019web %>%
  select(cps19_yob, cps19_gender, cps19_province, cps19_votechoice, cps19_citizenship)

# remove all non citizens from the data set 
reduced_ces<-
  reduced_ces%>%
  filter(cps19_citizenship == "Canadian citizen")


#gets rid of special characters in order to clean easier 
reduced_ces$cps19_votechoice <- str_replace_all(reduced_ces$cps19_votechoice,"[^[:alnum:]]", " ")


#change the name of the Bloc party 
reduced_ces$cps19_votechoice<- revalue(reduced_ces$cps19_votechoice, c("Bloc Qu b cois" = "Bloc Quebecois")) 


#change from year of birth to age in 2019 
reduced_ces$cps19_yob <- (2019 - as.numeric(reduced_ces$cps19_yob))


#remove any values with NA as requrired for logistic regresion 
reduced_ces <- na.omit(reduced_ces)

# make the party vote choice variables for logistic regression dependent variable 
# takes 1 if vote is yes for the party and 0 if other party 
reduced_ces<-
  reduced_ces %>%
  mutate(vote_con = 
           ifelse(cps19_votechoice=="Conservative Party", 1, 0))%>%
  mutate(vote_lib = 
           ifelse(cps19_votechoice=="Liberal Party", 1, 0))%>%
  mutate(vote_ndp = 
           ifelse(cps19_votechoice=="ndp", 1, 0)) %>%
  mutate(vote_bloc = 
           ifelse(cps19_votechoice=="Bloc Quebecois", 1, 0)) %>%
  mutate(vote_green = 
           ifelse(cps19_votechoice=="Green Party", 1, 0)) %>%
  mutate(vote_peoples = 
           ifelse(cps19_votechoice=="People s Party", 1, 0))

# remove people not old enough to vote 
reduced_ces<-
  reduced_ces%>%
  filter(cps19_yob > 17)


#change the name to make it easier to work with  
reduced_ces$cps19_gender<- revalue(reduced_ces$cps19_gender, c("Other (e.g. Trans, non-binary, two-spirit, gender-queer)" = "Other")) 




# change the name of the column title of required columns 
names(reduced_ces)[1] <- "Age"
names(reduced_ces)[2] <- "Gender"
names(reduced_ces)[3] <- "Province"

#revalue to match the census of male and female 
reduced_ces$Gender <- revalue(reduced_ces$Gender, c("A man" = "Male")) 
reduced_ces$Gender <- revalue(reduced_ces$Gender, c("A woman" = "Female")) 

#revalue to match the census which did not have individual territory data 
reduced_ces$Province <- revalue(reduced_ces$Province, c("Yukon" = "Northern Canada")) 
reduced_ces$Province <- revalue(reduced_ces$Province, c("Nunavut" = "Northern Canada")) 
reduced_ces$Province <- revalue(reduced_ces$Province, c("Northwest Territories" = "Northern Canada"))


# age group variable used in the next step to reduce amount of typing and clean code 
Ageg1 <- c(18:24)
Ageg2 <- c(25:34)
Ageg3 <- c(35:44)
Ageg4 <- c(45:54)
Ageg5 <- c(55:64)
Ageg6 <- c(65:74)
Ageg7 <- c(75:84)
ageg8 <- c(85:100)

# revalue the ages into groups that match the census 
reduced_ces <-
  reduced_ces%>%
  mutate(Age = case_when(Age %in% Ageg1 ~ "18 to 24", 
                         Age %in% Ageg2 ~ "25 to 34", 
                         Age %in% Ageg3 ~ "35 to 44",
                         Age %in% Ageg4 ~ "45 to 54", 
                         Age %in% Ageg5 ~ "55 to 64",
                         Age %in% Ageg6 ~ "65 to 74", 
                         Age %in% Ageg7 ~ "75 to 84"))

# remove the other genders from the data set 
reduced_ces <- 
  reduced_ces%>%
  filter(Gender != "Other")

# remove people who did not answer which party they wanted to vote for 
reduced_ces <- 
  reduced_ces%>%
  filter(cps19_votechoice != "Don t know  Prefer not to answer")


# make a new csv file 
write_csv(reduced_ces, "survey.ces.csv")



# CENSUS CLEAN

#census data set https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/cgi-bin/sdacensus/hsda?harcsda+cc16i

cancensus <- read.csv("census.csv") # load the census 

cancensus <- labelled::to_factor(cancensus)

#change the names of the columns that will be used 

names(cancensus)[2] <- "Age" 

names(cancensus)[4] <- "Gender"

names(cancensus)[83] <- "Province"

#reduce the census into a smaller data set with required columns 

reduced_census <- 
  cancensus %>%
  select(Age, Gender, Province)

# remove age groups that are not old enough to vote aka anyone under 18 

reduced_census <-
  reduced_census %>%
  filter(Age > 6)

# change genders to male and female from 1 and 2 

reduced_census$Gender <- mapvalues(reduced_census$Gender, from = c(1,2), to = c("Female", "Male"))

# change provinces from numbers to words 

reduced_census$Province <- mapvalues(reduced_census$Province, from = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 70), 
                                     to = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Northern Canada"))

# change age groups to make them easier to work with

reduced_census$Age <- mapvalues(reduced_census$Age, from = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,88), 
                                to = c("18 to 24","18 to 24", "25 to 34","25 to 34", "35 to 44", "35 to 44", "45 to 54", "45 to 54", "55 to 64", "55 to 64", "65 to 74", "65 to 74", "75 to 84","75 to 84","85 plus", "Not Available"))

# get the frequencies of each of the cells groups 

reduced_census1<- 
  reduced_census%>%
  group_by(Age)%>%
  count()

#remove any data that does not match up or is not available in the CES 

reduced_census1 <- 
  reduced_census1 %>% 
  filter(Age != "Not Available") %>%
  filter(Age != "85 plus")


#make a new csv
write_csv(reduced_census1, "census.can.csv")

```

