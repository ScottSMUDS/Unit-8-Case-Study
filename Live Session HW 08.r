

##INTSTALL/LOAD PACKAGES
install.packages ("RCurl")
install.packages ("dplyr")
install.packages ("Hmisc")
install.packages("formattable")
library(dplyr)
library(dplyr)
library(RCurl)
library(Hmisc)

##DOWNLOAD FILE
x <- getURL("http://stat.columbia.edu/~rachel/datasets/nyt1.csv")
getwd()
setwd("C:/Users/mscott4/Documents/Personal")
##READ FILE/ASSIGN VARIBLE TO FILE OUTPUT
x <- read.csv(textConnection(x), header = TRUE, stringsAsFactors = FALSE)
##ANALYSIS/EXPLORATION/VALIDATE
head(x)
tail(x)
str(x)
summary(x)
##SELECT/SPECIFY VARIABLES NEEDED
x<-select(x, Age,Gender ,Impressions,  Clicks, Signed_In)
## CUT FUNCTION APPLIED ON THE AGE, ADD NEW VARIABLE NAME AGE_GROUP
x$Age_Group <- cut(x$Age, c(-Inf, 17, 24, 34, 44, 54, 64, Inf))
##cATEGORIZE AGE_GROUP
levels(x$Age_Group) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
##ANALYSIS/EXPLORATION/VALIDATE
summary(x)
## FILTER DATASET FOR IMPRESSIONS>0
impsub <- subset(x, Impressions>0)
##ANALYSIS/EXPLORATION/VALIDATE
summary(impsub)
## ADD NEW VARIABLE 'CTR' DERIVED AS CLICKS/IMPRESSIONS
impsub$CTR <- impsub$Clicks/impsub$Impressions
##ANALYSIS/EXPLORATION/VALIDATE
summary(impsub)
head(impsub)

##INTSTALL/LOAD PACKAGES
install.packages ("ggplot2")
library(ggplot2)
##plot distribution of impressions and CTR by Age Group
qplot(Impressions,CTR, data = impsub, color = Age_Group)
qplot(Impressions,CTR, data = impsub, facets = .~ Age_Group)
##plot distribution of impressions and the count by Age Group
qplot(Impressions,data = impsub, binwidth= .75 ,fill = Age_Group)

##ADD VARIABLE TO CUT "CTR" BY SPECIFIED RANGE
impsub$user_seg <- cut2(impsub$CTR, c(-Inf , .2 , .4 , .6 , .8 , Inf))
##ASSSIGNED NAMES TO CUT RANGE
levels(impsub$user_seg) <- c("CTR< 0.2", "0.2<=CTR <0.4", "0.4<= CTR<0.6", "0.6<=CTR<0.8", "CTR>0.8")
summary(impsub)
head(impsub)
str(impsub)


males <- subset(impsub, Gender==1)
total_males <- sum(males$Gender)
##Total_males
Total_males

total_Impressions <- sum(males$Impressions)
##total_Impressions
total_Impressions
total_Clicks<- sum(males$Clicks)
##total_Clicks
total_Clicks

total_Signed_In<- sum(males$Signed_In)
##total_Signed_In
total_Signed_In


mean_age <- mean(impsub$Age)
##mean_age
mean_age
mean_Impressions <- mean(impsub$Impressions)
mean_Clicks<- mean(impsub$Clicks)
mean_CTR<- mean(impsub$CTR)

##percent of male and signed in 
percent(sum(impsub$Gender)/nrow(impsub))
percent(sum(impsub$Signed_In)/nrow(impsub))

##GRoup by Age_Group
tapply( impsub$Impressions,impsub$Age_Group, mean )
tapply( impsub$Clicks,impsub$Age_Group, mean )
tapply( impsub$CTR,impsub$Age_Group, mean )
##GRoup by Age_Group
tapply( impsub$Gender,impsub$Age_Group, sum )
tapply(impsub$Gender , impsub$Age_Group,length )
tapply( impsub$Gender,impsub$Age_Group, sum )/ tapply(impsub$Gender , impsub$Age_Group,length )
percent (tapply( impsub$Gender,impsub$Age_Group, sum )/ tapply(impsub$Gender , impsub$Age_Group,length ))
##GRoup by Age_Group
tapply( impsub$Signed_In,impsub$Age_Group, sum )
tapply(impsub$Signed_In , impsub$Age_Group,length )
tapply( impsub$Signed_In,impsub$Age_Group, sum )/ tapply(impsub$Signed_In , impsub$Age_Group,length )
percent (tapply(impsub$Signed_In,impsub$Age_Group, sum)/tapply(impsub$Signed_In , impsub$Age_Group,length ))

##CTRGroupvs AgeGroupcounts
qplot(user_seg, data = impsub,   fill = Age_Group)

##plot distribution of impressions and CTR by Age Group
qplot(Impressions,CTR, data = impsub, color = Age_Group)
qplot(Impressions,CTR, data = impsub, facets = .~ Age_Group)
##plot distribution of impressions and CTR by user_seg
qplot(Impressions,CTR, data = impsub, facets = .~ user_seg)






