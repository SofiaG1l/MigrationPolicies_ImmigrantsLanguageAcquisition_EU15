
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
#   - stringi (1.7.8)
# 
##################################

rm(list = ls())
gc()

library(tidyverse)
library(stringi)

#### Functions ####
# This function selects the most probable language/country based on the 
# most used language. Options need to be as string: "es;en;fr".
CHOOSE_0<-function(DATA,COLUMN){
  
  DATA0<-DATA%>%
    group_by(USER,USER_ID)%>%
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(nchar=str_length(get(COLUMN)))%>%
    # First compare with r+1
    mutate(COLUMN2=ifelse(nchar==2,get(COLUMN),
                          ifelse(get(COLUMN)=="Unknown"|is.na(str_extract(get(COLUMN),lead(get(COLUMN)))),
                                 get(COLUMN),str_extract(get(COLUMN),lead(get(COLUMN))))))%>%
    # Second compare with r-1
    mutate(nchar=str_length(COLUMN2))%>%
    mutate(COLUMN3=ifelse(nchar==2,COLUMN2,
                          ifelse(COLUMN2=="Unknown"|is.na(str_extract(COLUMN2,lag(COLUMN2))),
                                 COLUMN2,str_extract(COLUMN2,lag(COLUMN2)))))%>%
    mutate(!!COLUMN := COLUMN3)%>%
    dplyr::select(-c(COLUMN2,COLUMN3,nchar))
  
  return(as.data.frame(DATA0))
}

# The next function sandwiches the "un" from TWEET_LANG:
CHOOSE_1<-function(DATA,COLUMN){

  DATA0<-DATA%>%
    group_by(USER,USER_ID)%>%
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    # First compare with r+1 and r-1
    mutate(COLUMN2=ifelse(get(COLUMN)!="un",get(COLUMN),
                        ifelse(lag(get(COLUMN))==lead(get(COLUMN))&
                                 !is.na(lag(get(COLUMN))==lead(get(COLUMN))),
                               lag(get(COLUMN)),get(COLUMN))))%>%
    # Second compare with r-1
    mutate(COLUMN2=ifelse(COLUMN2!="un",COLUMN2,
                ifelse(lag(COLUMN2)!="un" & !is.na(lag(COLUMN2)),lag(COLUMN2),COLUMN2)))%>%
    # Third compare with r+1
    mutate(COLUMN2=ifelse(COLUMN2!="un",COLUMN2,
                ifelse(lead(COLUMN2)!="un" & !is.na(lead(COLUMN2)),lead(COLUMN2),COLUMN2)))%>%
    mutate(!!COLUMN := COLUMN2)%>%
    dplyr::select(-c(COLUMN2))
  
  return(as.data.frame(DATA0))
}

# The loop to sandwich until convergence
CHOOSE<-function(DATA,COLUMN,TYPE=0){
  
  DATA1<-as.data.frame(DATA)
  
  if(TYPE==0){
    DATA2<-CHOOSE_0(DATA,COLUMN)
  }else{
    DATA2<-CHOOSE_1(DATA,COLUMN)
  }
  
  equal1=Inf
  equal2=sum(drop(DATA2[,COLUMN])%s!=%drop(DATA1[,COLUMN]))
  
  iter=0
  
  while(abs(equal1-equal2)>0){
    # Save previous value
    equal1=equal2 
    DATA1<-DATA2
    
    # Update Info  
    if(TYPE==0){
      DATA2<-CHOOSE_0(DATA2,COLUMN)
    }else{
      DATA2<-CHOOSE_1(DATA2,COLUMN)
    }
    
    equal2=sum(DATA2[,COLUMN]%s!=%DATA1[,COLUMN])
    iter=iter+1
  }
  
  print(paste0("Iters: ",iter,".\t Difs: ",abs(equal1-equal2)))
  
  return(COLUMN=DATA2[,COLUMN])
}


#### External Data ####
COUNT_LANGUAGE<-data.frame(row.names = c("DK","DE","AT","NL","GB","FR","FI","LU","PT","BE","GR","IE","IT","ES","SE"),
                           LANG1=c("da","de","de","nl","en","fr","fi","lu","pt","de","el","en","it","es","sv"),
                           LANG2=c("da","de","de","nl","en","fr","fi","de","pt","fr","el","ga","it","ca","sv"),
                           LANG3=c("da","de","de","nl","en","fr","fi","fr","pt","fr","el","en","it","eu","sv"))


#### This comes from the folders with all the users from the same countries #####

Dir_2_save="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/"

setwd("p300199/GilClavel_3Article/1_DataHandling/PROCESSED")

FILES<-c("DK","DE","AT","NL","FR","FI","LU","GB","PT","BE","GR","IE","IT","ES","SE") # 
FILES<-paste0(FILES,".csv")

for(efe in FILES){
  
  if(nchar(efe)==6){
    efe2<-str_sub(efe, 1, 2)
  }else{
    efe2<-str_sub(efe, 1, 7)
  }
  
  print(paste("Processing",efe))
  
  BASE<-read.csv(efe)
  
  head(BASE)
  
  # Removing those that have never moved from efe
  BASE_DIF<-BASE%>%
    group_by(USER,USER_ID)%>%
    filter(!all(COUNTRY_TWEET==COUNTRY_USER)&!all(CODE_USER%in%c(efe2,"Unknown")))
  
  # Reordering by USER, YEAR, MONTH
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID)%>% 
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(ROWS = row_number())
  
  # Clean my predicted language 
  BASE_DIF[,"LANG_MINE"]<-CHOOSE(BASE_DIF,"LANG_MINE")
  
  BASE_DIF<-BASE_DIF%>%
    mutate(LANG_MINE=ifelse(str_length(LANG_MINE)>2,"Unknown",LANG_MINE))
  
  # If LANG_TWEETS=="un" then sandwich
  BASE_DIF[,"LANG_TWEETS"]<-CHOOSE(BASE_DIF,"LANG_TWEETS",TYPE=1)
  
  # Replace "un" from "LANG_TWEETS" with mine, when mine!="Unknown":
  BASE_DIF<-BASE_DIF%>%mutate(LANG_TWEETS=
                                ifelse(LANG_TWEETS=="un"&LANG_MINE!="Unknown",LANG_MINE,LANG_TWEETS))
  
  # Summarizing info by month
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(CODE_TWEET,wt = counts,name = "ntweet")%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(CODE_USER,wt = ifelse(CODE_USER!="Unknown",counts,0),name = "nuser")%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(LANG_TWEETS,wt = ifelse(LANG_TWEETS!="un",counts,0),name = "nltweet")%>% # Lang. from Twitter
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(LANG_USER,wt = counts,name = "nluser")%>%
    group_by(USER,USER_ID,SEX,YEAR,MONTH)%>%
    summarise(CODE_TWEET=CODE_TWEET[which.max(ntweet)],
              COUNTRY_TWEET=COUNTRY_TWEET[which.max(ntweet)],
              CODE_USER=CODE_USER[which.max(nuser)],
              LANG_TWEETS=LANG_TWEETS[which.max(nltweet)],
              COUNTRY_USER=COUNTRY_USER[which.max(nuser)],
              LANG_USER=LANG_USER[which.max(nluser)],
              counts=sum(counts))
  
  # Reordering by USER, YEAR, MONTH
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID)%>% 
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(ROWS = row_number())
  
  # Looking for migrants
  BASE_DIFcode<-BASE_DIF%>%
    group_by(USER,USER_ID)%>%
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(LAG=lag(CODE_USER),LEAD=lead(CODE_USER))%>%
    mutate(MATCHES=(LAG%s==%CODE_USER)&LAG%s==%LEAD)%>%
    mutate(MATCHES=ifelse(LAG!="Unknown",MATCHES,NA))%>% # Not consider the "Unknown"s!
    mutate(ROW_MATCH=ifelse(MATCHES,ROWS,NA))%>%
    filter(!all(is.na(ROW_MATCH)))%>%# Remove all those that moved randomly
    mutate(ORIGIN=min(ROW_MATCH,na.rm = TRUE))%>%
    mutate(ORIGIN=CODE_USER[min(ROW_MATCH,na.rm = TRUE)],DESTINATION=CODE_TWEET[1])%>%
    filter(ORIGIN!=DESTINATION&ORIGIN!="Unknown")%>%
    filter(ROWS%in%c((min(ROW_MATCH,na.rm = TRUE)-1):max(ROWS)))%>%
    mutate(ROWS = row_number())%>%
    filter(max(ROWS)>5)%>%
    dplyr::select(-c(LAG,LEAD,MATCHES,ROW_MATCH))
  
  uno<-BASE_DIF%>%group_by(USER_ID)%>%count(LANG_TWEETS)%>%filter(n==max(n))%>%dplyr::select(-n)
  dos<-BASE_DIF%>%group_by(USER_ID)%>%count(LANG_USER)%>%filter(n==max(n))%>%dplyr::select(-n)
  
  both<-uno%>%left_join(dos)
  
  both<-both%>%
    mutate(IS_TWEETS=LANG_TWEETS%in%COUNT_LANGUAGE[efe2,],
           IS_USER=LANG_USER%in%COUNT_LANGUAGE[efe2,])%>%
    filter(!IS_TWEETS&!IS_USER)
  
  USER_WHICH<-BASE_DIFcode%>%
    filter(USER_ID%in%both$USER_ID)
  
  print(paste0(efe," has n_users: ",length(unique(USER_WHICH$USER))))
  
  write.csv(x = USER_WHICH,file = paste0(Dir_2_save,efe2,".csv"),row.names = FALSE)
}


#### This comes from the files ALL_COUNTRIES_IMMIG ####
Dir_2_save="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Possible_Migrants_Final/"

setwd("p300199/GilClavel_3Article/1_DataHandling/PROCESSED")

FILES<-list.files()[str_detect(list.files(),"IMMIG")]

for(efe in FILES){
  
  efe2<-str_sub(efe, 1, 7)
  
  print(paste("Processing",efe))
  
  BASE<-read.csv(efe)
  
  head(BASE)
  
  BASE_DIF<-BASE
  
  BASE_DIF<-BASE_DIF%>%
    group_by(USER)%>% 
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(ROWS = row_number())
  
  # Clean my predicted language 
  BASE_DIF[,"LANG_MINE"]<-CHOOSE(BASE_DIF,"LANG_MINE")
  
  BASE_DIF<-BASE_DIF%>%
    mutate(LANG_MINE=ifelse(str_length(LANG_MINE)>2,"Unknown",LANG_MINE))
  
  # Replace "un" from "LANG_TWEETS" with mine, when mine!="Unknown":
  BASE_DIF<-BASE_DIF%>%mutate(LANG_TWEETS=
                                ifelse(LANG_TWEETS=="un"&LANG_MINE!="Unknown",LANG_MINE,LANG_TWEETS))
  
  # If LANG_TWEETS=="un" then sandwich
  BASE_DIF[,"LANG_TWEETS"]<-CHOOSE(BASE_DIF,"LANG_TWEETS",TYPE=1)
  
  # Summarizing info by month
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(CODE_TWEET,wt = counts,name = "ntweet")%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(CODE_USER,wt = ifelse(CODE_USER!="Unknown",counts,0),name = "nuser")%>%
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(LANG_TWEETS,wt = ifelse(LANG_TWEETS!="un",counts,0),name = "nltweet")%>% # Lang. from Twitter
    group_by(USER,USER_ID,YEAR,MONTH)%>%
    add_count(LANG_USER,wt = counts,name = "nluser")%>%
    group_by(USER,USER_ID,SEX,YEAR,MONTH)%>%
    summarise(CODE_TWEET=CODE_TWEET[which.max(ntweet)],
              COUNTRY_TWEET=COUNTRY_TWEET[which.max(ntweet)],
              CODE_USER=CODE_USER[which.max(nuser)],
              LANG_TWEETS=LANG_TWEETS[which.max(nltweet)],
              COUNTRY_USER=COUNTRY_USER[which.max(nuser)],
              LANG_USER=LANG_USER[which.max(nluser)],
              counts=sum(counts))
  
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID)%>% 
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(ROWS = row_number())
  
  # Looking for migrants
  BASE_DIF<-BASE_DIF%>%
    group_by(USER,USER_ID)%>%
    arrange(YEAR,MONTH, .by_group = TRUE)%>%
    mutate(LAG=lag(CODE_TWEET),LEAD=lead(CODE_TWEET))%>%
    mutate(MATCHES=(LAG%s==%CODE_TWEET)&LEAD%s==%LAG)%>%
    mutate(ROW_MATCH=ifelse(MATCHES,ROWS,NA))%>%
    filter(!all(is.na(ROW_MATCH)))%>%# Remove all those that moved randomly
    mutate(ORIGIN=min(ROW_MATCH,na.rm = TRUE),
           DESTINATION=max(ROW_MATCH,na.rm = TRUE))%>%
    filter(DESTINATION>ORIGIN)%>%
    filter(ROWS%in%c((ORIGIN[1]-1):(DESTINATION[1]+1)))%>%
    mutate(ROWS = row_number())%>%
    filter(max(ROWS)>5)%>% # i.e. at least 6 months of info
    mutate(ORIGIN=CODE_TWEET[1],DESTINATION=CODE_TWEET[max(ROWS)])%>%
    filter(ORIGIN!=DESTINATION)%>%
    dplyr::select(-c(LAG,LEAD,MATCHES,ROW_MATCH))
  
  print(paste0(efe," has n_users: ",length(unique(BASE_DIF$USER))))
  
  write.csv(BASE_DIF,paste0(Dir_2_save,efe2,".csv"),row.names = FALSE)

}






