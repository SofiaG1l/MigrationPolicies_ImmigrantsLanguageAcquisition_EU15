
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
#   - tidyverse (1.3.1)
#   - foreign (X.X.X)
# 
##################################

rm(list = ls())
gc()

library(tidyverse)
library(foreign)
# library(utils)


set.seed(155)


#### Opening the Data ####
# DB with all the info from the possible Migrants
load(file="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Mig_Processed_Final.RData")

#### Merging Regions ####
REGIONS<-read.csv("DATA/Subregions_2.csv")
REGIONS<-REGIONS[,c(2,4)]
REGIONS<-REGIONS[!is.na(REGIONS$CODE),]
REGIONS<-rbind(REGIONS,c("Europe","XK")) #"Kosovo"
REGIONS<-rbind(REGIONS,c("Europe","GG")) #"Guernsey"
REGIONS<-rbind(REGIONS,c("Europe","JE")) #"Jersey"
REGIONS<-rbind(REGIONS,c(NA,"Unknown"))

BASE<-merge(BASE,REGIONS,by.x = "ORIGIN",by.y = "CODE")
BASE_S<-merge(BASE_S,REGIONS,by.x = "origin",by.y = "CODE")

BASE_S$REGION<-factor(BASE_S$REGION,
                      levels=c("Europe","Africa","Asia","S. Am. and Carib.","N. Am. and Oc."),
                      labels=c("Europe","Africa","Asia","S.Am. and Carib.","N.Am. and AU+NZ."))


#### Filtering only those which language tweet is valid language country ####

LANGS<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/DATA/World_Languages.csv",stringsAsFactors = FALSE)
LANGS<-LANGS%>%
  unite("z", LANG_CODE1:LANG_CODE6, remove = FALSE,na.rm = TRUE)

LANGS_V=LANGS$z
names(LANGS_V)<-LANGS$CODE

# Check if language first tweet is valid lang. origin
BASE_S<-BASE_S%>%
  mutate(NoLangValid=!str_detect(LANGS_V[origin], as.character(lng_tweeti), negate = FALSE))

# Remove those migrant==censored and LangValid==FALSE
BASE_S<-BASE_S%>%filter(!(migrant=="Censored"&NoLangValid==TRUE))
BASE<-BASE%>%filter(USER%in%BASE_S$USER)


## Checking in Lang origin differs from destination
COUNT_LANGUAGE<-data.frame(row.names = c("DK","DE","AT","NL","GB","FR","FI","LU","PT","BE","GR","IE","IT","ES","SE"),
                           LANG1=c("da","de","de","nl","en","fr","fi","lb","pt","de","el","en","it","es","sv"),
                           LANG2=c("da","de","de","nl","en","fr","fi","de","pt","fr","el","ga","it","ca","sv"),
                           LANG3=c("da","de","de","nl","en","fr","fi","fr","pt","nl","el","en","it","eu","sv"))

## Checking 1st lang. differs from destination
Check_Lang_OD<-function(x,y){
  return(any(str_detect(x,
                        as.character(as.vector(COUNT_LANGUAGE[y,])), negate = FALSE)))
}


DIFFS<-rep(0,dim(BASE_S)[1])

for (i in 1:dim(BASE_S)[1]){
  DIFFS[i]=Check_Lang_OD(BASE_S$lng_tweeti[i],BASE_S$destination[i])
}

BASE_S$LANG_OD_eq2=DIFFS

BASE_S<-BASE_S%>%filter(LANG_OD_eq2==0)
BASE<-BASE%>%filter(USER%in%BASE_S$USER)

#### Adding Lexical Distance ####
# Language provides new series for:
# Common Official Language (COL),
# Common Spoken Language (CSL),
# Common Native Language (CNL) and
# in the case of Linguistic Proximity, LP1 and LP2,
# the unadjusted values that serve for constructing two different measures,
# which we label PROX1 and PROX2. This series are available for 195 countries.

# Adding the ISO 2 codes
CODES<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/DATA/Country_Code.csv")
CODES<-CODES[,c(3,2)]

# In the data Belgium and Luxemburg are together!!
# The ling_web.dta comes from:
# http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=19. Accessed April 12, 2021.
DB_LANG<-read.dta("p300199/GilClavel_3Article/2_DataAnalysis/DATA/ling_web.dta")

#Correction for Belgium and Luxembourg
CODES%>%filter(CODE1%in%c("LU","BE"))
# Replacing strange code "BLX" with "BEL" and "LUX
UNO<-DB_LANG%>%filter(iso_o=="BLX"|iso_d=="BLX")
UNO<-UNO%>%mutate(iso_o=ifelse(iso_o=="BLX","BEL",iso_o),
                  iso_d=ifelse(iso_d=="BLX","BEL",iso_d))

DOS<-DB_LANG%>%filter(iso_o=="BLX"|iso_d=="BLX")
DOS<-DOS%>%mutate(iso_o=ifelse(iso_o=="BLX","LUX",iso_o),
                  iso_d=ifelse(iso_d=="BLX","LUX",iso_d))

# Deleting from original data
DB_LANG<-DB_LANG%>%filter(!(iso_o=="BLX"|iso_d=="BLX"))
# Checking that they complement
dim(UNO)[1]==dim(DOS)[1]
(dim(DB_LANG)[1]+dim(UNO)[1])==37830

# Adding new info
DB_LANG<-rbind(DB_LANG,UNO,DOS)

# Merging!
DB_LANG<-DB_LANG%>%
  left_join(CODES,by=c("iso_o"="CODE2"))%>%
  left_join(CODES,by=c("iso_d"="CODE2"))
ene<-which(colnames(DB_LANG)%in%c("CODE1.x","CODE1.y"))
colnames(DB_LANG)[ene]<-c("code_o","code_d")
head(DB_LANG)

## Merging with BASE_S
head(BASE_S)
BASE_S<-BASE_S%>%
  left_join(DB_LANG[,5:15],c("origin"="code_o","destination"="code_d"))

## Some are missing!!
(BASE_S%>%filter(is.na(col))%>%count(origin,destination))
FALTAN<-BASE_S%>%filter(is.na(col))%>%count(origin)
CODES%>%filter(CODE1%in%FALTAN$origin)
sum(FALTAN$n)

# Removing NAs
BASE_S<-BASE_S%>%filter(!is.na(col))

# Inverse of proximity! => prox2(AU,DE)=0!!
BASE_S<-BASE_S%>%mutate(prox2_2=1-prox2)
BASE_S<-BASE_S%>%mutate(prox1_2=1-prox1)

#### Adding English: dummy of first tweet english ####
BASE_S<-BASE_S%>%mutate(english=factor(lng_tweeti=="en"))
# Adding Not Valid Language and English
BASE_S<-BASE_S%>%mutate(Other_Eng=I(english==TRUE&NoLangValid==TRUE))


#### Adding Geographical Distance ####
# The Gravity_V202102.csv comes from:
# http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=19. Accessed April 12, 2021.
GRAVITY<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/DATA/Gravity_V202102.csv",stringsAsFactors = FALSE)

GRAVITY<-GRAVITY%>%
  filter(year==2019)%>%
  left_join(CODES,by=c("iso3_o"="CODE2"))%>%
  left_join(CODES,by=c("iso3_d"="CODE2"))
ene<-which(colnames(GRAVITY)%in%c("CODE1.x","CODE1.y"))
colnames(GRAVITY)[ene]<-c("code_o","code_d")
head(GRAVITY)

GRAVITY<-GRAVITY%>%
  dplyr::select(code_o,code_d,dist,distcap)

BASE_S<-BASE_S%>%
  left_join(GRAVITY,c("origin"="code_o","destination"="code_d"))

#### Size based on size folders ####
dirs="p300199/GilClavel_3Article/1_DataHandling/PROCESSED/Users_Classified_20210118/"
SIZE_FOLS<-file.info(paste0(dirs,list.files(dirs, all.files = TRUE, recursive = TRUE)))

SIZE_FOLS$NAME<-str_remove(row.names(SIZE_FOLS), dirs)
row.names(SIZE_FOLS)<-NULL
SIZE_FOLS<-SIZE_FOLS%>%mutate(NAME_2=str_sub(NAME,1,2))
WorldLangsS<-SIZE_FOLS%>%group_by(NAME_2)%>%summarise(SIZE=sum(size))

BASE_S<-BASE_S%>%left_join(WorldLangsS,by=c("origin"="NAME_2"))
BASE_S<-BASE_S%>%left_join(WorldLangsS,by=c("destination"="NAME_2"))
BASE_S<-BASE_S%>%mutate(ratio=log(SIZE.x/SIZE.y))

#### Adding Percentage People Speak Foregin Language ####
# The data comes from:
# Eurostat. (2021). Number of foreign languages known (self-reported) by sex. 
# [Dataset page]. Retrieved November 17, 2021, from Eurostat Data Browser website: 
# https://ec.europa.eu/eurostat/databrowser/view/EDAT_AES_L21/default/table?lang=en&category=sks.sks_ssr.sks_ssaes.edat_aes_l2. 
PER_FOR<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/DATA/PercentageForeignLanguage.csv",
                  stringsAsFactors = FALSE)

BASE_S<-BASE_S%>%
  left_join(PER_FOR[,-c(1,3)],by=c("destination"="CODE"))

BASE_S<-BASE_S[,c("USER","gender",
                  "migrant","origin","destination","REGION","TYPE",
                  "datei","datem","datef","e3L","dateL","timeL",
                  "lng_tweeti","lng_tweett","lng_tweetf","NoLangValid","Other_Eng",
                  "totalRows","totalTweets",
                  "ratio","distcap","prox2",
                  "english","ONE","TWO","THREE_MORE")]

#### Keeping only Trajectory Migrants 
BASE_S<-BASE_S%>%filter(migrant=="Trajectory")

dim(BASE_S)

#### Adding extra columns with CIVIX and CPI ####

CIVIX_CPI<-read.csv("p300199/GilClavel_3Article/2_DataAnalysis/DATA/Goodman_Howard.csv")

head(CIVIX_CPI)

BASE_S<-BASE_S%>%left_join(CIVIX_CPI[,c(2,5,6)],by = c("destination"="alpha.2"))

#### Centering the continues values ####
BASE_S$ratio<-(BASE_S$ratio-mean(BASE_S$ratio,na.rm=TRUE))/sqrt(var(BASE_S$ratio,na.rm=TRUE))
BASE_S$prox2<-(BASE_S$prox2-mean(BASE_S$prox2,na.rm=TRUE))/sqrt(var(BASE_S$prox2,na.rm=TRUE))
BASE_S$dist<-(BASE_S$dist-mean(BASE_S$dist,na.rm=TRUE))/sqrt(var(BASE_S$dist,na.rm=TRUE))
BASE_S$distcap<-(BASE_S$distcap-mean(BASE_S$distcap,na.rm=TRUE))/sqrt(var(BASE_S$distcap,na.rm=TRUE))
BASE_S$ONE<-as.double(BASE_S$ONE)
BASE_S$TWO<-as.double(BASE_S$TWO)
BASE_S$THREE_MORE<-as.double(BASE_S$THREE_MORE)
BASE_S$ONE_MORE=BASE_S$ONE+BASE_S$TWO+BASE_S$THREE_MORE
BASE_S$ONE_MORE<-(BASE_S$ONE_MORE-mean(BASE_S$ONE_MORE,na.rm=TRUE))/sqrt(var(BASE_S$ONE_MORE,na.rm=TRUE))

#### The Dichotomic
BASE_S$Other_Eng<-factor(BASE_S$Other_Eng,c(FALSE,TRUE))


save(BASE, BASE_S, file="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Mig_Processed_TOMODEL_Final.RData")


