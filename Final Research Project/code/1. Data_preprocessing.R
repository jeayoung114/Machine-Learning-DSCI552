##LassoElasticRidge - regression on MSI
# Rework of Greenson2009 with ML methods.

#What are we learning by doing this regression
#install.packages("summarytools")
#install.packages("magick")
library(summarytools)
#install.packages("data.table")
library(data.table)
#install.packages("glmnet")
library(glmnet)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("Matrix")
library(Matrix)
#install.packages("sqldf")
library(sqldf)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ROCR")
library(ROCR)
rm(list = ls())
#Get the MECC_TME data 

setwd("/Users/kevinrayjon/Documents/USC_MS_ADS/Spring_2021/DSCI_552/Project/Data")

#install.packages("/net/isi-dcnl/ifs/user_data/Gruber_Grp/Group/tools/utilVM_0.1.0.tar.gz", type="source")
#library(utilVM)

mydat <- read.delim("ads9.csv",sep=",",stringsAsFactors = FALSE)

# retain only the important columns

xdat_start <- sqldf("select resp_label, Side1, OverallGroupstage, AngiolymphInvasion, 
                         InvasiveGrowthPattern,MSI,
                         Plasma, GarlandNecrosis, Mucinous, 
                         SignetRing, Grade1, HistoHetero, tilsmax2, sex, agedx,
                         crohn_consensus
                      from mydat
                  where Side1 in ('Left','Right')
                  and OverallGroupStage in ('I','II','III','IV')
                 and AngioLymphInvasion in ('Yes','No')
                 and InvasiveGrowthPattern in ('Expansile','Expansile and Invasive','Invasive')
                 and GarlandNecrosis in ('Yes','No','Yes(Focal)')
                 and SignetRing in ('Yes','No','Yes(Focal)')
                 and Mucinous in ('Yes','No','Yes(Focal)')
                 and Grade1 in ('Moderate','Well','Poor')
                 and HistoHetero in ('Yes','No')
                 and tilsmax0 in ('Any TILS','No TILS')
                 and MSI in (0,1)")


set.seed(1618)
#install.packages("dplyr")
xdat_in <- dplyr::sample_frac(xdat_start,0.8)

#Make a vector of the dependent variable
y.vec <- xdat_in$MSI                 
as.data.frame(table(xdat_start$crohn_consensus))

#Evaluate and transform the independent variables
#table(xdat_in$Side1)

#table(xdat_in$Side1)
#table(xdat_in$OverallGroupStage)
#table(xdat_in$AngiolymphInvasion)
#table(xdat_in$InvasiveGrowthPattern)
#table(xdat_in$GarlandNecrosis)
#table(xdat_in$SignetRing)
#table(xdat_in$Mucinous)
#table(xdat_in$Grade1)
#table(xdat_in$HistoHetero)
#table(mydat$tilsmax0)
#table(mydat$MSI)

hist(mydat$agedx)

xdat_in$Decade<-round((xdat_in$agedx)/10,0)
table(xdat_in$Decade)

xdat_in$Under50<-ifelse(xdat_in$agedx<=50,1,0)
table(xdat_in$Under50)


table(mydat$sex)


xdat_in$Female <- ifelse(xdat_in$sex=="Female",1,0)
table(xdat_in$Female)

xdat_in$Crohn <- ifelse(xdat_in$crohn_consensus=="Yes",1,0)

table(xdat_in$Crohn)

xdat_in$CrohnMissing <- ifelse(xdat_in$crohn_consensus %in% c("Yes","No"),0,1)

table(xdat_in$CrohnMissing)

#xdat_in$Differentiation <- ifelse(xdat_in$Grade1=="Well",2,
#                                  ifelse(xdat_in$Grade1=="Moderate",1,0))

xdat_in$ModDiff <- ifelse(xdat_in$Grade1=="Moderate",0,1)

table(xdat_in$tilsmax2)
xdat_in$TILS<- ifelse(xdat_in$tilsmax2=="2 and Over",1,0)

xdat_in$HistHet<- ifelse(xdat_in$HistoHetero=="Yes",1,0)

xdat_in$RightSide <- ifelse(xdat_in$Side1=="Right",1,0)

xdat_in$NoNecrosis <- ifelse(xdat_in$GarlandNecrosis=="Yes"|xdat_in$GarlandNecrosis=="Yes(Focal)",0,1)

xdat_in$Signet <- ifelse(xdat_in$SignetRing=="Yes"|xdat_in$SignetRing=="Yes(Focal)",1,0)

xdat_in$Mucin <- ifelse(xdat_in$Mucinous=="Yes"|xdat_in$Mucinous=="Yes(Focal)",1,0)

xdat_in$AngioLymph <- ifelse(xdat_in$AngiolymphInvasion=="Yes",1,0)

xdat_in$Expansile <- ifelse(xdat_in$InvasiveGrowthPattern=="Invasive",0,1)

xdat_in$Stage12 <- ifelse(xdat_in$OverallGroupStage %in% c("I","II"),1,0)
