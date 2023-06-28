
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: March 6th, 2022.
# 
# Description: Code to replicate the article:
#   Gil-Clavel, S., Grow, A., & Bijlsma, M. J. (2023). Migration Policies and Immigrants’ Language Acquisition in EU-15: 
#   Evidence from Twitter. Population and Development Review. https://doi.org/10.1111/padr.12574
# 
# Computer Environment:
#   - Windows 
#   - R - 4.1.1 (2021)
#   - Rstudio (1.4.1717)
#   - Microsoft Windows 10 Enterprise
# 
# R Packages:
#   - SurvRegCensCov (1.4)
#   - drc (3.0-1)
#   - ggplot2 (3.3.5)
#   - tidyverse (1.3.1)
#   - survminer (0.4.9)
#   - RColorBrewer (1.1-2)
# 
##################################

# Cleaning the Environment
rm(list = ls())
gc()

# Opening Packages
library(SurvRegCensCov)
library(drc)
library(ggplot2)
library(tidyverse)
library(survminer)
library(RColorBrewer)

# Fixing random uniform values
set.seed(44)


#### Opening the already processed data ####
load(file="2_DataAnalysis/PROCESSED/Mig_Processed_TOMODEL_Final.RData")
BASE_S<-BASE_S%>%filter(migrant=="Trajectory")

## Opening CIVIX_CPI:
CIVIX_CPI<-read.csv("2_DataAnalysis/DATA/Goodman_Howard.csv")


#### Ploting Kaplan-Meier curve ####

fit <- survfit(Surv(timeL,e3L)~1, data = BASE_S[,c("timeL","e3L")]) # ,subset = I(col==0)

# pdf("Processed_Data/Images/PHDS.pdf",width = 12.5,height = 8)
(FIT_P<-ggsurvplot(
  fit,
  data = BASE_S,
  xlab = "Time in months",
  xlim=c(0,50),break.x.by=5,
  size = 1,                 # change line size
  # palette = as.vector(COLORS),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = FALSE,              # Add p-value
  pval.coord = c(0,0.25),   # p-value -> c(x,y)
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  # legend.labs =levels(BASE_S$TYPE),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw(base_size = 15)      # Change ggplot2 theme
))
# dev.off()

SURV_KM<-FIT_P$data.survplot
# Adjusting exp. model to survival curves
# Checking with linear regression
REG<-SURV_KM%>%
  filter(!surv%in%c(0,1))%>%
  mutate(lnln=log(-log(surv)),lnt=log(time))

#### Checking Weibull Best Fit ####

sum_exp<-function(NAME,DATA,shape=1,lambda=0.05,t1=NULL,t2=NULL){
  if(!is.null(t1) & !is.null(t2)){
    DATA<-DATA%>%filter(time>=t1 & time<=t2)
  }else{
    if(!is.null(t1) & is.null(t2)){
      DATA<-DATA%>%filter(time>=t1)
    }else{
      if(is.null(t1) & !is.null(t2))
        DATA<-DATA%>%filter(time<=t2)
    }
  }
  
  MODEL<-drm(formula = paste0(NAME,"~ time"),data=DATA,fct = W2.2())
  
  RSS.p <- sum(residuals(MODEL)^2,na.rm = TRUE)
  TSS <- sum((DATA[,NAME] - mean(DATA[,NAME],na.rm = TRUE))^2,na.rm = TRUE)
  R2<-1 - (RSS.p/TSS)
  return(list(R2=round(R2,digits = 4),
              k=round(coef(MODEL)[1],digits = 4),
              haz=round(coef(MODEL)[2],digits = 4),
              summary=summary(MODEL)))
}

head(REG)

ggplot(REG,aes(lnt,lnln))+
  geom_point()+ 
  geom_smooth(method='lm',color="black")+
  theme_minimal(base_size = 15)+theme(legend.position = "top")+
  xlab("ln(t)")+ylab("ln(-ln(S))")

# ggsave("2_DataAnalysis/PROCESSED/IMAGES/KM_WEIB_Linear_bw.png",width = 10,height = 5)

summary(lm(lnln~lnt,REG))
betas=coef(lm(lnln~lnt,REG))
CONDT=sum_exp("surv",REG,lambda = exp(betas[1]/betas[2])[[1]],shape = betas[2][[1]])
print(paste0("lamda=",1/CONDT$haz,"; shape=",CONDT$k))
print(paste0("lamda=",exp(betas[1]),"; shape=",betas[2]))


#### Kaplan Meier with Weibull adjusted ####
(KM<-ggplot(SURV_KM,aes(x=time,y=surv))+
   geom_point()+geom_line()+
   geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3)+
   ylab("Survival probability")+xlab("Time in months")+
   theme_minimal()+
   theme(plot.title = element_text(hjust = 0.5),
         text = element_text(size=15),
         legend.position = "top")
)

# ggsave("2_DataAnalysis/PROCESSED/IMAGES/KM_WEIB_1.png",width = 15,height = 10) #units = "cm",

### Adding the weibull estimate dists.
(KM<-KM+stat_smooth(method = 'drm', 
                    formula = "y~x",
                    se = FALSE,color="black",
                    method.args = list(fct = W2.2()))) # W2.2()
# ggsave("2_DataAnalysis/PROCESSED/IMAGES/KM_WEIB_CIVIX_CPI_bw.png",width = 15,height = 10)


#### Variables Names #### 

VARnames=c("TYPEENAB\n[PT,FI,IE,BE,SE]"="ENAB",
           "TYPEINSUL\n[GR,ES,IT,LU]"="INSUL",
           "TYPECONDT\n[NL,GB,FR]"="CONDT",
           "Other_EngTRUE"="First tweet\nEnglish*:True",
           "ONE_MORE:englishTRUE=ONE_MORE:englishTRUE",
           "englishTRUE"="First tweet\nEnglish:True",
           "ONE_MORE"="%People Speak\none or more\nForeign Languages",
           "distcap"="Geographical\nDistance","dist"="Geog. Dist.",
           "Other_EngTRUE"="English: No. Org.",
           "NoLangValidTRUE"="No Val.Lang.:Yes",
           "prox1"="prox",
           "prox2"="Linguistic\nDistance",
           "gendermale"="Gender:Male",
           "prox1_1"="prox", 
           "cle"="cle","lp1"="lp1","prox2_2"="prox",
           "csl:cnl"="csl:cnl","csl"="csl","cnl"="cnl","col"="col",
           "CLOSENESS"="Lang. Closs.",
           "ratio"="#Users Origin to\nDestination Ratio",
           "CIVIX:CPI"="CIVIX x CPI",
           "CIVIX"="Civic-Integration\nIndex(CIVIX)",
           "CPI"="Citizenship-Policy\nIndex(CPI)",
           "destinationGB"="GB","destinationNL"="NL","FR"="FR",
           "destinationFI"="FI","destinationIE"="IE",
           "destinationPT"="PT","destinationSE"="SE","BE"="BE",
           "destinationGR"="GR","destinationIT"="IT","ES"="ES",
           "destinationDE"="DE","destinationDK"="DK","AT"="AT")

### AFT Model: Including Gender ####
# This figure is part of the Online Supplemental Material 

AllWeiG<-WeibullReg(Surv(timeL,e3L) ~ ratio+distcap+prox2+ gender +
                     ONE_MORE+CIVIX*CPI,
                   conf.level = 0.95,data = BASE_S) # TYPE+


AllWeiG$ETR

n=dim(AllWeiG$summary$table)[1]
HZ_AllWeiG<-as.data.frame(AllWeiG$ETR)
HZ_AllWeiG$COEFS<-rownames(HZ_AllWeiG)
HZ_AllWeiG$`p-value<`<-
  c("***","**","*","."," ")[findInterval(as.data.frame(AllWeiG$summary$table)$p[-c(1,n)], 
                                         sort(c(1, 0.1, 0.05, 0.01, 0.001, 0))) ]

HZ<-HZ_AllWeiG
rownames(HZ)<-NULL

# Including sample size
HZ$COEFS<-factor(HZ$COEFS,levels = names(VARnames),labels = VARnames)

ggplot(HZ, aes(x=COEFS, y=ETR)) + #
  geom_errorbar(aes(ymin=LB, ymax=UB), width=.6,position = position_dodge(1),
                na.rm = TRUE)+
  geom_point(size=1,position = position_dodge(1))+
  coord_flip()+ 
  guides(shape=guide_none(),color=guide_none())+
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8,margin = c(100,10,10,10)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.25, 
                                          linetype = 'dashed',
                                          colour = "gray"), 
        panel.grid.major.x = element_line(size = 0.25, 
                                          linetype = 'dashed',
                                          colour = "gray"),
        strip.background = element_rect(color = NA,
                                        fill = NA, size = 1),
        axis.line = element_line(color = 'black'))

# ggsave("2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure_A3b.png",
#        dpi = 600,width = 12,height = 9,units = "cm")

### AFT Model: Without Gender ####

AllWei<-WeibullReg(Surv(timeL,e3L) ~ ratio+distcap+prox2+
                     ONE_MORE+CIVIX*CPI,
                   conf.level = 0.95,data = BASE_S) # TYPE+


AllWei$ETR

n=dim(AllWei$summary$table)[1]
HZ_AllWei<-as.data.frame(AllWei$ETR)
HZ_AllWei$COEFS<-rownames(HZ_AllWei)
HZ_AllWei$`p-value<`<-
  c("***","**","*","."," ")[findInterval(as.data.frame(AllWei$summary$table)$p[-c(1,n)], 
                                         sort(c(1, 0.1, 0.05, 0.01, 0.001, 0))) ]

HZ<-HZ_AllWei
rownames(HZ)<-NULL

# Including sample size
HZ$COEFS<-factor(HZ$COEFS,levels = names(VARnames),labels = VARnames)

ggplot(HZ, aes(x=COEFS, y=ETR)) + #
  geom_errorbar(aes(ymin=LB, ymax=UB), width=.6,position = position_dodge(1),
                na.rm = TRUE)+
  geom_point(size=1,position = position_dodge(1))+
  coord_flip()+ 
  guides(shape=guide_none(),color=guide_none())+
  geom_hline(yintercept = 1, linetype="dashed")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8,margin = c(100,10,10,10)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(size = 0.25, 
                                          linetype = 'dashed',
                                          colour = "gray"), 
        panel.grid.major.x = element_line(size = 0.25, 
                                          linetype = 'dashed',
                                          colour = "gray"),
        strip.background = element_rect(color = NA,
                                        fill = NA, size = 1),
        axis.line = element_line(color = 'black'))

# ggsave("2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure_3.png",
#        dpi = 600,width = 12,height = 9,units = "cm")


#### Heat Map ####

### Relation between HR and AFT:
# Here the scale p = PROHB_W$summary$scale^-1
# HR: exp(B_j)=exp(-A_j*p)
#   exp(-coef(PROHB_W$summary)*PROHB_W$summary$scale^-1)
# AFT: exp(A_j)
#   exp(coef(PROHB_W$summary))

# BASE_S$english<-as.double(BASE_S$english)-1

# colnames(BASE_S)[which(names(BASE_S)=="english")]="englishTRUE"

COEFS=coef(AllWei$summary)
c(COEFS)%*%rep(1,length(COEFS))

MATRIX=BASE_S%>%
  mutate("(Intercept)"=1,"CIVIX:CPI"=CIVIX*CPI)%>%
  dplyr::select(names(COEFS))


BASE_S$AFT=round(exp(as.matrix(MATRIX)%*%COEFS),2)

#### Simulating some values ####

## Functions ##

set.seed(415682)

SIMS<-function(VARIABLE,N){
  seq(min(VARIABLE),max(VARIABLE),length.out=N)
}

GROUPS_VALS<-function(GROUP,ii){
  DD<-data.frame(GROUP=rep(GROUP,10),
                 CIVIX=rnorm(10,VALS[ii,2],VALS[ii,3]),
                 CPI=rnorm(10,VALS[ii,4],VALS[ii,5]))
  return(DD)
}

c(COEFS)

ene=1000
DATASIM<-expand.grid(CIVIX=SIMS(BASE_S$CIVIX,ene),
                     CPI=SIMS(BASE_S$CPI,ene))

DATASIM<-data.frame(ratio=round(rep(mean(BASE_S$ratio),ene),4),
                    distcap=round(rep(mean(BASE_S$distcap),ene),4),
                    prox2=round(rep(mean(BASE_S$prox2),ene),4),
                    ONE_MORE=round(rep(mean(BASE_S$ONE_MORE),ene),4),
                    CIVIX=DATASIM$CIVIX,
                    CPI=DATASIM$CPI)

CIVIX_CPI_AFT<-CIVIX_CPI%>%
  mutate("(Intercept)"=1,
         ratio=mean(BASE_S$ratio),
         distcap=mean(BASE_S$distcap),
         prox2=mean(BASE_S$prox2),
         ONE_MORE=mean(BASE_S$ONE_MORE),
         "CIVIX:CPI"=CIVIX*CPI)%>%
  dplyr::select(names(COEFS))

CIVIX_CPI_AFT$AFT=round(exp(as.matrix(CIVIX_CPI_AFT)%*%COEFS),2)

CIVIX_CPI$AFT=CIVIX_CPI_AFT$AFT

# The matrix of values
MATRIX=DATASIM%>%
  mutate("(Intercept)"=1,"CIVIX:CPI"=CIVIX*CPI)%>%
  dplyr::select(names(COEFS))

# Multiplying to obtain the AFT
DATASIM$AFT=round(exp(as.matrix(MATRIX)%*%COEFS),2)

head(CIVIX_CPI)

VALS<-CIVIX_CPI%>%
  group_by(GROUP)%>%
  summarise(meanCIVIX=mean(CIVIX),sdCIVIX=sd(CIVIX),
            meanCPI=mean(CPI),sdCPI=sd(CPI))
VALS<-as.data.frame(VALS)


CIVIX_CPI2<-rbind(CIVIX_CPI[,4:6],
                  GROUPS_VALS("CONDT",1),
                  GROUPS_VALS("ENABL",2),
                  GROUPS_VALS("INSUL",3),
                  GROUPS_VALS("PROHB",4))

CIVIX_CPI2$GROUP2=factor(CIVIX_CPI2$GROUP,
                         levels = c("CONDT","ENABL","INSUL","PROHB"),
                         labels = c("Conditional","Enabling",
                                    "Insular","Prohibitive"))

# For the legends
FILL_LABELS<-round(exp(pretty(log(DATASIM$AFT),6)))
FILL_LABELS2<-paste0("(",FILL_LABELS[-7],",",FILL_LABELS[-1],"]")
names(FILL_LABELS)<-FILL_LABELS2


(p0<-ggplot(DATASIM, aes(CPI, CIVIX))+
    theme_minimal(base_size = 12)+
    geom_contour_filled(aes(z=AFT),breaks= FILL_LABELS) + #
    geom_label(data=CIVIX_CPI,aes(x=CPI,y=CIVIX,check_overlap=TRUE,
                                  inherit.aes=TRUE,
                                  label=paste(alpha.2,round(AFT),sep = ": ")))+ # 
    stat_ellipse(data=CIVIX_CPI2, 
                 aes(CPI, CIVIX, group=GROUP2,color=GROUP2),
                 type = "norm",size=1.5)+
    labs(x="CPI",y="CIVIX")+
    # update_geom_defaults("label", list(size = 3))+
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=12))+
    scale_x_continuous(expand = c(0, 0),limits = c(-0.5,6))+ 
    scale_y_continuous(expand = c(0, 0)))

p0.1<-p0+
  guides(color="none",
         fill=guide_legend(title = "AFT",order = 2,nrow = 1))+
  theme(legend.position = "bottom",
        text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

p1.1<-p0+
  guides(color=guide_legend(title = "Group",order = 1,
                            override.aes = list(legend.key.color="white")),
         fill="none")+
  theme(legend.position = "bottom",
        text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

# Removing the legends to place them together
leg1=get_legend(p0.1)
leg2=get_legend(p1.1)

# png('2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure_4.png',
#     units = "cm",width = 15*1.3,height = 15*1.3,res = 200)
gridExtra::grid.arrange(p0,leg2,leg1,
                        ncol=1,nrow=3,
                        layout_matrix = cbind(1:3),
                        widths=c(5),
                        heights = c(5,0.3,0.3))
# dev.off()
