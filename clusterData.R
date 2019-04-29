#________________Script for Clustering observations into groups based on Demographic features____________________
#If the following library's are not installed, install them by uncommenting following code
# install.packages("dplyr")
# install.packages("ggplot")

#loading Required Librarys
library(dplyr)
library(ggplot2)

setwd("C:/Users/lakna/OneDrive/Desktop/TOT Analysis/DataFiles") #Change based on directory where the files are located in your computer
analysisData<-read.csv("finaldata17.csv")
analysisData[,7:32]<-sapply(analysisData[,7:32],as.numeric) # Converting required columns to be numeric
analysisData<-analysisData%>%filter(Mathematics_BASIC_PCT!=1 & Eng..Language.Arts_BASIC_PCT!=1)


#---------------Hierarachical Clustering of Schools by User Determined K(Number of Clusters,K=3) based on ENROLLMENT Numbers and BLACK student Population 

#Scaling the Enrollment and Black enrollment percentage since they are in different units
clusterData<-analysisData%>%mutate(ENROLLMENT_GRADES_K_12_SCALED=scale(ENROLLMENT_GRADES_K_12),ENROLLMENT_BLACK_PCT_SCALED=scale(ENROLLMENT_BLACK_PCT))
featurePair<-clusterData%>%select(ENROLLMENT_GRADES_K_12_SCALED,ENROLLMENT_BLACK_PCT_SCALED)
featurePair<-dist(featurePair,method="euclidean") #calculating a distance matrix between the two features

      clustering<-hclust(featurePair,method = "complete") #clustering the distance matrix using the complete linkage method
      cassigment<-cutree(clustering,k=3) 
      
analysisData<-mutate(analysisData,cluster=cassigment,cluster=as.factor(cluster))



#The following code shows the average enrollment rates and black student population between the 3 clusters
analysisData %>%
  group_by(cluster) %>%
dplyr::summarize(Mean_enrollment = mean(ENROLLMENT_GRADES_K_12),Mean_black=mean(ENROLLMENT_BLACK_PCT)) 


#Renaming clusters based on cluster characteristics
analysisData$cluster <- gsub("1", "LowEnroll-LowBlackStudentPop", analysisData$cluster)
analysisData$cluster <- gsub("2", "HighEnroll-HighBlackStudentPop", analysisData$cluster)
analysisData$cluster <- gsub("3", "LowEnroll-HighBlackStudentPop", analysisData$cluster)


analysisDataMath<-analysisData%>%arrange(cluster,desc(Mathematics_BASIC_PCT))
analysisDataEnglish<-analysisData%>%arrange(cluster,desc(Eng..Language.Arts_BASIC_PCT))


#The following piece of code returns the top two schools that have the highest number of students with BASIC math profeciency in 2017 

analysisDataMath%>%filter(cluster=="HighEnroll-HighBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Mathematics_BASIC_PCT)
analysisDataMath%>%filter(cluster=="LowEnroll-HighBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Mathematics_BASIC_PCT)
analysisDataMath%>%filter(cluster=="LowEnroll-LowBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Mathematics_BASIC_PCT)


#The following piece of code returns the top two schools that have the highest number of students with BASIC English profeciency in 2017 

analysisDataEnglish%>%filter(cluster=="HighEnroll-HighBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Eng..Language.Arts_BASIC_PCT)
analysisDataEnglish%>%filter(cluster=="LowEnroll-HighBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Eng..Language.Arts_BASIC_PCT)
analysisDataEnglish%>%filter(cluster=="LowEnroll-LowBlackStudentPop")%>%head(2)%>%select(SCHOOL_NAME,Eng..Language.Arts_BASIC_PCT)


#-----------------------------Visualizing Findings---------------------

p1<-ggplot(analysisDataMath, aes(x=cluster, y=Mathematics_BASIC_PCT, fill=cluster)) +
  geom_boxplot()+ggtitle("Distribution of the number of Students with Basic Proficiency in Mathematics")
p1+geom_jitter()

p2<-ggplot(analysisDataEnglish, aes(x=cluster, y=Mathematics_BASIC_PCT, fill=cluster)) +
  geom_boxplot()+ggtitle("Distribution of the number of Students with Basic Proficiency in English")+scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9"))
p2+geom_jitter()
