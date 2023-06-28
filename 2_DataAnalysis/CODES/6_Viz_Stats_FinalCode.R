
library(ggplot2)
library(tidyverse)
library(ggridges)
library(RColorBrewer)

options(scipen=999)

# Cleaning the Environment
rm(list = ls())
gc()

## Opening the Data
load(file="2_DataAnalysis/PROCESSED/Mig_Processed_TOMODEL_Final.RData")
BASE_S<-BASE_S%>%filter(migrant=="Trajectory")


#### Language Plots ####

LANGS<-read.csv("2_DataAnalysis/DATA/World_Languages.csv",stringsAsFactors = FALSE)
LANGS<-LANGS%>% 
  unite("z", LANG_CODE1:LANG_CODE6, remove = FALSE,na.rm = TRUE)

LANGS_V=LANGS$z
names(LANGS_V)<-LANGS$CODE

# Check if language first tweet equals language from country of origin
BASE_S<-BASE_S%>%
       filter(str_detect(LANGS_V[origin], as.character(lng_tweeti), negate = FALSE))

BASE<-BASE%>%filter(USER%in%BASE_S$USER)


#### Plotting Language ####
DB_ALL<-BASE

DB_ALL$LANG_TWEETS<-as.factor(DB_ALL$LANG_TWEETS)
DB_ALL$LANG_TWEETS<-droplevels(DB_ALL$LANG_TWEETS)
DT_ALL<-xtabs(counts~LANG_TWEETS,DB_ALL)
MEASURE=quantile(DT_ALL)[4]
names(DT_ALL)<-ifelse(DT_ALL<MEASURE,"other",names(DT_ALL))
DT_ALL<-data.frame(LANG=names(DT_ALL),COUNT=c(DT_ALL))

levels(DB_ALL$LANG_TWEETS)<-DT_ALL$LANG

DT_ALL<-DT_ALL%>%
  group_by(LANG)%>%
  summarise(COUNT=sum(COUNT))

DT_ALL<-DT_ALL[order(-DT_ALL$COUNT),]

COLORS=c(brewer.pal(10,"Set3"),
         brewer.pal(8,"Dark2"),
         brewer.pal(10,"Paired"),"#232326")
names(COLORS)<-levels(DB_ALL$LANG_TWEETS)


DB_ALL$LANG_TWEETS<-as.character(DB_ALL$LANG_TWEETS)

DB_ALL$LANG_TWEETS<-factor(DB_ALL$LANG_TWEETS,levels = DT_ALL$LANG)

DB_ALL%>%
  filter(YEAR<2017)%>%
  ggplot(aes(as.factor(MONTH),weight=counts,fill=LANG_TWEETS))+
  geom_bar(position = "fill")+
  scale_fill_manual(values = COLORS)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 18),
        legend.position = "bottom",
        panel.background = element_rect(linetype = "solid",
                                        colour = "grey"))+
  facet_grid(.~YEAR,scales = "free_y")+
  scale_y_continuous(labels = scales::percent)+
  labs(x="month",y="%")+
  guides(fill=guide_legend(title = "Lang.\nTweets",
                           byrow = TRUE, nrow=2))
  
# ggsave("2_DataAnalysis/PROCESSED/IMAGES/GilClavel_FigA1.png",units = "cm",width = 30,height = 20)


#### Some statistics from the users ####

VIS1=BASE%>%
  group_by(USER)%>%
  summarise(MEAN_TW_MONTH=mean(counts),
            TOTAL_TW=sum(counts),
            TOTAL_MT=n())

VIS1%>%
  ungroup()%>%
  summarise(mean(MEAN_TW_MONTH))

VIS1%>%
  ungroup()%>%
  summarise(mean(TOTAL_TW))

VIS1%>%
  ungroup()%>%
  summarise(mean(TOTAL_MT))

#### By Cluster

CLUSTER_COLOR=c("Conditional"="#FB8072",
                "Insular"="#8DD3C7",
                "Prohibitive"="#BEBADA",
                "Enabling"="#4DAF4A")

VIS_RR=BASE%>%
  group_by(USER,TYPE)%>%
  summarise(MEAN_TW_MONTH=mean(counts),
            TOTAL_TW=sum(counts),
            TOTAL_MT=n())

VIS_RR$TYPE=ifelse(VIS_RR$TYPE=="CONDT","Conditional",
                   ifelse(VIS_RR$TYPE=="INSUL","Insular",
                          ifelse(VIS_RR$TYPE=="PROHB","Prohibitive","Enabling")))

VIS_RR$TYPE=factor(VIS_RR$TYPE,
                   levels = rev(c("Conditional","Enabling","Insular","Prohibitive")))

A=VIS_RR%>%
  ungroup()%>%
  ggplot(aes(MEAN_TW_MONTH,TYPE,fill=TYPE))+ 
  geom_density_ridges() + 
  theme_minimal()+
  theme(legend.position="none",
        text = element_text(size = 6,lineheight=7),
        axis.text = element_text(size = 8,lineheight=9),
        axis.title = element_text(size = 9,lineheight=10))+
  scale_fill_manual(values = CLUSTER_COLOR)+
  labs(y=NULL,x="a) Users' Mean Number of Tweets per Month")

B=VIS_RR%>%
  ungroup()%>%
  ggplot(aes(TOTAL_TW,TYPE,fill=TYPE))+ 
  geom_density_ridges() + 
  theme_minimal()+
  theme(legend.position="none",
        text = element_text(size = 6,lineheight=7),
        axis.text = element_text(size = 8,lineheight=9),
        axis.title = element_text(size = 9,lineheight=10))+
  scale_fill_manual(values = CLUSTER_COLOR)+
  labs(y=NULL,x="b) Users' Total Number of Tweets")

C=VIS_RR%>%
  ungroup()%>%
  ggplot(aes(TOTAL_MT,TYPE,fill=TYPE))+ 
  geom_density_ridges() + 
  theme_minimal()+
  theme(legend.position="none",
        text = element_text(size = 6,lineheight=7),
        axis.text = element_text(size = 8,lineheight=9),
        axis.title = element_text(size = 9,lineheight=10))+
  scale_fill_manual(values = CLUSTER_COLOR)+
  labs(y=NULL,x="c) Users' Total Number of Months")

# png('2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure_I.png',
#     units = "cm",width = 15*1.3,height = 15*1,res = 200)
# # cairo_ps('2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure_I.eps',
# #   fallback_resolution=300,width = 6.08,height = 4.728,bg="white")
# gridExtra::grid.arrange(A,B,C,
#                         ncol=1,nrow=3,
#                         layout_matrix = rbind(c(1),c(2),c(3)),
#                         widths=c(10),
#                         heights = c(8,8,8))
# dev.off()



