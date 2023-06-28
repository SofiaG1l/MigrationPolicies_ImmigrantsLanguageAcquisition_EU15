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
#   - circlize (0.4.15)
#   - RColorBrewer (1.1-2)
# 
##################################
# Cleaning the Environment
rm(list = ls())
gc()

# This code was adapted from:
# https://stackoverflow.com/questions/39504096/r-circlize-chord-diagram-with-log-scale

library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(circlize)

#### Opening the Data ####
load(file="p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/Mig_Processed_TOMODEL_Final.RData")

BASE_S<-BASE_S%>%filter(migrant=="Trajectory")

# Arranging Columns
names(BASE_S)

#### Labels ####
CLUSTERS<-c("Conditional [NL,GB,FR]","Insular [GR,ES,IT,LU]",
            "Enabling [PT,FI,IE,BE,SE]","Prohibitive [AT,DK,DE]")
names(CLUSTERS)<-c("CONDT","INSUL","ENAB","PROHB")

REGIONS<-c("Europe","Africa","Asia","LAC","N.America and AU+NZ")
names(REGIONS)<-c("Europe","Africa","Asia","S.Am. and Carib.","N.Am. and AU+NZ.")

#### From Region to Cluster ####

df <- BASE_S%>%
  ungroup()%>%
  count(REGION,TYPE)
colnames(df)[1]<-"region"

df$region<-as.character(df$region)
df$region<-REGIONS[df$region]

df$TYPE<-as.character(df$TYPE)
df$TYPE<-CLUSTERS[df$TYPE]

df["n"] = log10(df["n"])

country = unique(c(df[[1]], df[[2]]))
color=c("#1B9E77","#E6AB02","#7570B3","#E7298A","#D95F02",
        "#FB8072","#8DD3C7","#BEBADA","#4DAF4A")
df1 <- data.frame(country, color,stringsAsFactors = FALSE)
names(color)<-country

# For the journal:
# cairo_ps('p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure2.eps',
#          fallback_resolution=300,width = 6.08,height = 6.08,bg="white")

# pdf("p300199/GilClavel_3Article/2_DataAnalysis/PROCESSED/IMAGES/GilClavel_Figure2.pdf",
#     width = 15,height = 15)
circos.clear()
circos.par(start.degree = 90, gap.degree = 5, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4),cex=0.45)

res = chordDiagram(x = df, grid.col = color, transparency = 0.25,
                   order = country, directional = 1,
                   direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
                   annotationTrack = c("grid"), annotationTrackHeight = c(0.05, 0.1),
                   link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)



circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    country = df1$country[df1$country == sector.index]
    
    circos.text(x = mean(xlim), y = 1.5, 
                labels = country, facing = "bending", adj = c(0.5, 0), cex = 2, niceFacing = TRUE)
    
  }
)

for(i in seq_len(nrow(res))) {
  circos.text(x = res$x1[i] - res$value11[i]/2, y = 0.5, round(10^(res$value1[i])), facing = "inside",
              niceFacing = TRUE, adj = c(0.5, 0.5), cex = 1.5, col = "black", sector.index = res$rn[i],
              track.index = 1)
  circos.text(x = res$x2[i] - res$value1[i]/2, y = 0.5, round(10^(res$value1[i])), facing = "inside",
              niceFacing = TRUE, adj = c(0.5, 0.5), cex = 1.5, col = "black", sector.index = res$cn[i],
              track.index = 1)
}

# dev.off()






