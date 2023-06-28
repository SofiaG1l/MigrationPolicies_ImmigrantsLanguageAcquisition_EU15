
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: March 6th, 2022.
# 
# Description: Code to replicate the article:
#   "EU-15 Immigrants Language Acquisition on Twitter"
# 
# Computer Environment:
#   - Windows 
#   - R - 4.1.1 (2021)
#   - Rstudio (1.4.1717)
#   - Microsoft Windows 10 Enterprise
# 
# R Packages:
#   - ggplot2 (3.3.5)
#   - tidyverse (1.3.1)
#   - ggridges (0.5.3)
#   - RColorBrewer (1.1-2)
# 
##################################

rm(list = ls())
gc()

library(ggplot2)
library(tidyverse)
library(ggridges)
library(RColorBrewer)

#### Functions ####
#### Checking Origins and Destinations ####
Dest_Fun<-function(x,CODE_TWEET) {
  BOL=(CODE_TWEET[x-1]==CODE_TWEET[x] & CODE_TWEET[x-2]==CODE_TWEET[x-1])
  if(BOL){
    return(CODE_TWEET[x])    
  }
  else{
    x=x-1
    BOL=(CODE_TWEET[x-1]==CODE_TWEET[x] & CODE_TWEET[x-2]==CODE_TWEET[x-1])
    if(BOL)
      return(CODE_TWEET[x]) 
    else
      return(NA)
  }
}

Org_Fun<-function(x,CODE_TWEET) {
  BOL=(CODE_TWEET[x+1]==CODE_TWEET[x] & CODE_TWEET[x+2]==CODE_TWEET[x+1])
  if(BOL){
    return(CODE_TWEET[x])    
  }
  else{
    x=x+1
    BOL=(CODE_TWEET[x+1]==CODE_TWEET[x] & CODE_TWEET[x+2]==CODE_TWEET[x+1])
    if(BOL){
      return(CODE_TWEET[x]) 
    }
    else{
      x=x+1
      BOL=(CODE_TWEET[x+1]==CODE_TWEET[x] & CODE_TWEET[x+2]==CODE_TWEET[x+1])
      if(BOL)
        return(CODE_TWEET[x]) 
      else
        return(NA)
    }
  }
}

# Type of migration:
# If ORG_TWEET==DESTINATION => ORG_USER!=ORG_TWEET and ORG_TWEET!="Unknown"
Is_Mig<-function(ORG_TWEET,ORG_USER,DESTINATION){
  if(!is.na(ORG_TWEET)){
    if(ORG_TWEET!=DESTINATION){
      return("Trajectory")
    }else{
      if(!is.na(ORG_USER) & ORG_USER!="Unknown"){
        if(ORG_USER!=ORG_TWEET)
          return("Censored")
        else
          return("Unknown")
      }
      else{
        return("Unknown")
      }
    }
  }else{
    return("Unknown")
  }
}

#### Opening the data bases ####
COUNT_LANGUAGE<-data.frame(row.names = c("DK","DE","AT","NL","GB","FR","FI","LU","PT","BE","GR","IE","IT","ES","SE"),
                           LANG1=c("da","de","de","nl","en","fr","fi","lu","pt","de","el","en","it","es","sv"),
                           LANG2=c("da","de","de","nl","en","fr","fi","de","pt","fr","el","ga","it","ca","sv"),
                           LANG3=c("da","de","de","nl","en","fr","fi","fr","pt","fr","el","en","it","eu","sv"))


# Subregions
REGIONS<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/Subregions.csv")
REGIONS<-REGIONS[,c(2,4)]
REGIONS<-REGIONS[!is.na(REGIONS$CODE),]
REGIONS<-rbind(REGIONS,c("Europe","XK")) #"Kosovo"
REGIONS<-rbind(REGIONS,c("Europe","GG")) #"Guernsey" 
REGIONS<-rbind(REGIONS,c("Europe","JE")) #"Jersey"
REGIONS<-rbind(REGIONS,c(NA,"Unknown"))

# List of files 
setwd("p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Possible_Migrants_Final/")
FILES<-list.files()
FILES1=which(str_detect(FILES,"IMMIG"))
COUNTRIES<-c("DK","DE","AT","NL","GB","FR","FI","PT","BE","GR","IE","IT","ES","SE") # "LU",
FILES2=which(FILES%in%paste0(COUNTRIES,".csv"))

# First the Truncated data:
EFE<-FILES[FILES2]

BASE<-read.csv(EFE[1],stringsAsFactors = FALSE)

BASE$MIGRANT="Censored"

for(efe in EFE[2:length(EFE)]){
  BASE0<-read.csv(efe,stringsAsFactors = FALSE)
  BASE0$MIGRANT="Censored"
  BASE<-rbind(BASE,BASE0)
}

# Second the Trajectories:
EFE<-FILES[FILES1]

for(efe in EFE){
  BASE0<-read.csv(efe,stringsAsFactors = FALSE)
  BASE0$MIGRANT="Trajectory"
  BASE<-rbind(BASE,BASE0)
}


# Reordering by USER, YEAR, MONTH
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(ROWS = row_number())

# Namibia code is "NA"!!
BASE[is.na(BASE$CODE_TWEET),]$CODE_TWEET<-"NA"

## Keeping only those that moved to EU-15
BASE<-BASE%>%
  filter(DESTINATION%in%c("DK","DE","AT","NL","GB","FR","FI","LU",
                          "PT","BE","GR","IE","IT","ES","SE"))

# Creating Date column
# Proxy Day
# BASE$ROWS2<-ifelse(BASE$ROWS<30,BASE$ROWS,BASE$ROWS-30)
# BASE$ROWS2<-ifelse(BASE$ROWS2<10,paste0("0",BASE$ROWS2),paste0(BASE$ROWS2))
# Month
BASE$MONTH=ifelse(BASE$MONTH<10,paste0("0",BASE$MONTH),paste0(BASE$MONTH))
# Year
BASE$YEAR=paste0(BASE$YEAR)


#### Checking Origins and Destinations ####

#### Tracking time until changing profile location ####
# Adding dichotomic variable: time until change user location
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E1 = CODE_TWEET==DESTINATION)

# E2 => 0 when Trajectory & (CODE_TWEET==DESTINATION)
# E2 => >0 when Censored & (CODE_TWEET==DESTINATION)
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E2 = cumsum(E1))

# Adding probability of change
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E3 = (lag(E1)!=E1)&(lag(E2)<1))

#### Tracking time until changing profile location ####
# Adding dichotomic variable: time until change user location
BASE<-BASE%>%
  group_by(USER,USER_ID,ROWS)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E1L = LANG_TWEETS%in%COUNT_LANGUAGE[DESTINATION,])

# Adding probability of change
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E21L = E1L & E1)

# CumSum
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E22L = cumsum(E21L)) # ROWSL = row_number(),

# CumSum
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  mutate(E3L = E22L==1&lag(E22L)==0)%>%
  mutate(E3L=ifelse(is.na(E3L),FALSE,E3L))

# Reordering by USER, YEAR, MONTH
BASE<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)


#### The Clusters ####
PROHB<-c("AT","DE","DK")
CONDT<-c("NL","GB","FR")
INSUL<-c("ES","LU","IT","GR")
ENAB<-c("PT","FI","IE","BE","SE")

TOG<-c(PROHB,CONDT,INSUL,ENAB)
CODE_TWEETS=unique(BASE$CODE_TWEET)
COMP_TOG=CODE_TWEETS[!CODE_TWEETS%in%TOG]

TYPE<-data.frame(DESTINATION=c(TOG,COMP_TOG),
                 TYPE=c(rep("PROHB",3),rep("CONDT",3),rep("INSUL",4),rep("ENAB",5),
                        rep("OTHER",length(COMP_TOG))))

BASE<-BASE%>%right_join(TYPE,by = "DESTINATION")


# The Host-Languages
COUNT_LANGUAGE<-data.frame(row.names = c("DK","DE","AT","NL","GB","FR","FI","LU","PT","BE","GR","IE","IT","ES","SE"),
                           LANG1=c("da","de","de","nl","en","fr","fi","lu","pt","de","el","en","it","es","sv"),
                           LANG2=c("da","de","de","nl","en","fr","fi","de","pt","fr","el","ga","it","ca","sv"),
                           LANG3=c("da","de","de","nl","en","fr","fi","fr","pt","fr","el","en","it","eu","sv"))

##### Transforming data into Survival type ####

# Merging data with regions
BASE<-merge(BASE,REGIONS,by.x = "ORIGIN",by.y = "CODE")

BASE$E3[is.na(BASE$E3)]<-FALSE

# Reordering by USER, YEAR, MONTH
BASE<-BASE[order(BASE$USER,BASE$ROWS),]
names(BASE)
summary(BASE)

BASE_S<-BASE%>%
  group_by(USER,USER_ID)%>%
  arrange(YEAR,MONTH, .by_group = TRUE)%>%
  summarise(gender=SEX[1],
            datei=paste0(MONTH[ROWS==1],"-",YEAR[ROWS==1]),
            datem=paste0(MONTH[E1 & E2==1],"-",YEAR[E1 & E2==1]),
            datef=paste0(MONTH[max(ROWS)],"-",YEAR[max(ROWS)]),
            totalRows=max(ROWS),
            totalTweets=sum(counts),
            timeL=ifelse(any(E3L),ROWS[E3L],max(ROWS)), # Related to language
            dateL=ifelse(any(E3L),paste0(MONTH[E3L],"-",YEAR[E3L]),paste0(MONTH[max(ROWS)],"-",YEAR[max(ROWS)])),
            e3L=ifelse(any(E3L),TRUE,FALSE),
            timeS=ifelse(any(E3),ROWS[E3],max(ROWS)), # Related to settling down
            dateS=ifelse(any(E3),paste0(MONTH[E3],"-",YEAR[E3]),paste0(MONTH[max(ROWS)],"-",YEAR[max(ROWS)])),
            e3S=ifelse(any(E3),TRUE,FALSE),
            lng_tweeti=factor(LANG_TWEETS[ROWS==1]),
            lng_tweett=factor(ifelse(any(E3),LANG_TWEETS[E3],LANG_TWEETS[max(ROWS)])),
            lng_tweetf=factor(LANG_TWEETS[max(ROWS)]),
            code_useri=CODE_USER[ROWS==1],
            code_usert=ifelse(any(E3),CODE_USER[E3],CODE_USER[max(ROWS)]),
            code_userf=CODE_USER[max(ROWS)],
            code_tweeti=CODE_TWEET[ROWS==1],
            code_tweetf=CODE_TWEET[max(ROWS)],
            origin=ORIGIN[1],
            destination=DESTINATION[1],
            migrant=factor(MIGRANT[1]),
            region=factor(REGION[1]),
            TYPE=factor(TYPE[1]))


BASE_S[!BASE_S$gender%in%c("female","male"),]$gender=NA
BASE_S$gender<-factor(BASE_S$gender)

#### Calculating real number of months between events ####
# https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

BASE_S<-BASE_S%>%
  mutate(datei=as.Date(paste0("01-",datei),"%d-%m-%Y"),
         datem=as.Date(paste0("01-",datem),"%d-%m-%Y"),
         datef=as.Date(paste0("01-",datef),"%d-%m-%Y"),
         dateL=as.Date(paste0("01-",dateL),"%d-%m-%Y"),
         dateS=as.Date(paste0("01-",dateS),"%d-%m-%Y"))%>%
  mutate(timeL=ifelse(elapsed_months(dateL,datem)==0,0.5,elapsed_months(dateL,datem)))%>%
  mutate(timeS=ifelse(elapsed_months(dateS,datem)==0,0.5,elapsed_months(dateS,datem)))%>%
  mutate(timeTOTAL=elapsed_months(datef,datei))


#### Filtering full tweets database ####
BASE<-BASE%>%filter(USER%in%BASE_S$USER)

#### Saving the Data ####
save(BASE, BASE_S, file="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Mig_Processed_Final.RData")



