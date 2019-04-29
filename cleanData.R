#________________________________R Script for Cleaning Datafile_____________________________#
# #Install these packages if they are not already installed by uncommenting following section
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
#Loading Required Library's 
library(tidyverse)
library(readxl)
library(reshape2)

setwd("C:/Users/lakna/OneDrive/Desktop/TOT Analysis/DataFiles") #Change path to directory based on where the files are located in your computer
list.files(getwd())

#Loading Data Files
schoolType<-read_excel("SLC_SchoolTypes.xlsx")
schoolDemographic<-read_excel("Building Demographic Data 2006 - 2017.xls")
schoolAsessment<-read_excel("Building - State Assessment Results MSIP no A1 E2 vol EOC no Grade Level.xlsx")
schoolGrowth17<-read_excel("School_Growth_Measures_2017and2016.xlsx", sheet = 2)%>%rename(SCHOOL_NAME=`School Name`)
schoolGrowth16<-read_excel("School_Growth_Measures_2017and2016.xlsx", sheet = 3)%>%rename(SCHOOL_NAME=`School Name`)


#-----------------------------Cleaning School Type Dataset--------------------------------
schoolType<-schoolType %>%  # Adding a column to classify if schools are Elementary, High or Middle Schools.
  mutate(schoollevel = case_when( 
    grepl("ELE", schoolType$SCHOOL_NAME) ~ "ELEMENTARY",
    grepl("HIGH",schoolType$SCHOOL_NAME)~"HIGH",
    grepl("MIDDLE",schoolType$SCHOOL_NAME)~"MIDDLE"
  ))

write.csv(schoolType,"SchoolType.csv") #Manually Add ELEM,HIGH OR MIDDLE for school level for schools unable to be determined based on school name


#---------------------Cleaning SchoolDemographic Dataset------------------
stlSChoolDemographic<-schoolDemographic[grep("^115", schoolDemographic$COUNTY_DISTRICT_CODE), ] #Getting the schools that are based on STL using the first three digits of the COUNTY_DISTRICT_CODE 

                                                                                                
schoolDemographic<-stlSChoolDemographic%>%
                    select(YEAR,SCHOOL_CODE,COUNTY_DISTRICT_CODE,SCHOOL_NAME,ENROLLMENT_GRADES_K_12,LUNCH_COUNT_FREE_REDUCTED_PCT,ENROLLMENT_ASIAN_PCT,ENROLLMENT_BLACK_PCT,ENROLLMENT_HISPANIC_PCT,ENROLLMENT_INDIAN_PCT,ENROLLMENT_MULTIRACIAL_PCT,ENROLLMENT_PACIFIC_ISLANDER_PCT,ENROLLMENT_WHITE_PCT,ENROLLMENT_ELL_LEP_PCT,IEP_INCIDENCE_RATE)%>% 
                    filter(YEAR %in% c(2015,2016,2017))  #Keeping only data from 2015, 2016 and 2017 that's where I have the test scores available


#Combining SchoolDemographic dataset with schooltype dataset
combinedSchools<-dplyr::inner_join(schoolType,schoolDemographic,"SCHOOL_NAME")


# following dataframe containsschools that are in School_Type
# but dont't have corresponding data in 
# SchoolDemographic-checked a few of the names and seems like they have been closed off
# E.g Webster Middle School
test<-dplyr::anti_join(schoolType,combinedSchools,"SCHOOL_NAME") 
#---------------------Cleaning SchoolAsessment Dataset-------------------------------------
schoolAsessment<-schoolAsessment%>%select(YEAR,TYPE,CATEGORY,COUNTY_DISTRICT,DISTRICT_NAME,SCHOOL_NAME,CONTENT_AREA,BELOW_BASIC_PCT,BASIC_PCT,ADVANCED_PCT)%>%
                  filter(YEAR %in% c(2015,2016,2017),TYPE=="Total",CATEGORY=="MSIP Total no A1 E2 vol EOC")


                                  
schoolAsessment15<-schoolAsessment%>%select(YEAR,SCHOOL_NAME,CONTENT_AREA,BELOW_BASIC_PCT,BASIC_PCT,ADVANCED_PCT)%>%arrange(SCHOOL_NAME)%>%filter(YEAR=='2015')
schoolAsessment16<-schoolAsessment%>%select(YEAR,SCHOOL_NAME,CONTENT_AREA,BELOW_BASIC_PCT,BASIC_PCT,ADVANCED_PCT)%>%arrange(SCHOOL_NAME)%>%filter(YEAR=='2016')
schoolAsessment17<-schoolAsessment%>%select(YEAR,SCHOOL_NAME,CONTENT_AREA,BELOW_BASIC_PCT,BASIC_PCT,ADVANCED_PCT)%>%arrange(SCHOOL_NAME)%>%filter(YEAR=='2017')


asessment15<-dcast(melt(schoolAsessment15, id.var = c("YEAR", "SCHOOL_NAME", "CONTENT_AREA")), 
      SCHOOL_NAME ~ CONTENT_AREA + variable,fun.aggregate = toString)
asessment15<-asessment15%>%mutate(Year=2015)

asessment16<-dcast(melt(schoolAsessment16, id.var = c("YEAR", "SCHOOL_NAME", "CONTENT_AREA")), 
                   SCHOOL_NAME ~ CONTENT_AREA + variable,fun.aggregate = toString)
asessment16<-asessment16%>%mutate(Year=2016)

asessment17<-dcast(melt(schoolAsessment16, id.var = c("YEAR", "SCHOOL_NAME", "CONTENT_AREA")), 
                   SCHOOL_NAME ~ CONTENT_AREA + variable,fun.aggregate = toString)
asessment17<-asessment16%>%mutate(Year=2017)

#combinedAsessment<-rbind(asessment15,asessment16,asessment17) 
#rm(asessment15,asessment16,asessment17)


#---------------Combining Schooldemographic dataset with the asessment dataset 
combinedSchools15<-combinedSchools%>%filter(YEAR==2015)
finaldata15<-dplyr::inner_join(combinedSchools15,asessment15,"SCHOOL_NAME")
  
combinedSchools16<-combinedSchools%>%filter(YEAR==2016)
finaldata16<-dplyr::inner_join(combinedSchools16,asessment16,"SCHOOL_NAME") # The school name is messing things up

combinedSchools17<-combinedSchools%>%filter(YEAR==2017)
finaldata17<-dplyr::inner_join(combinedSchools17,asessment17,"SCHOOL_NAME") # The school name is messing things up


finaldata17[ finaldata17 == "*" ] <- NA   #Adding NA to replace "*" sign in data 
na_count<-sapply(finaldata17, function(y) sum(length(which(is.na(y))))) # Determining which columns have NA values
na_count
# Looks like enrollment black percentage and Enrollment_GRADES_K_12 percentage have no missing values.

write.csv(finaldata17,"finaldata17.csv")




