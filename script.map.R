### Figure 3
### Map with WCM phenotype

library(ggmap)
library(maps)
library(mapdata)
library(tidyverse)
library(sf)
library(rnaturalearth)

worldmap <- ne_countries(scale = 'medium', type = 'map_units',returnclass = 'sf')

setwd("~/Documents/PhD_KSU/Projects/5.A.TAUSCHII/1.WCM/2020/Map")
passport <- read.csv("long_lat_alt.csv", header = T, as.is = T, fill = T)
lininfo = pheno
str(lininfo)

passport <- merge(lininfo, passport, by = 'taxa')
passport <- data.frame(passport, 'WCM' = 'S', stringsAsFactors = F)
passport$'WCM'[passport$wcm.cat == 1] = 'R'
passport$'WCM'=as.factor(passport$'WCM')


## color based on QDR pheno
colnames(passport)=c("taxa","bw_number","Lineage","WCM","GBS_Haplotype",
                     "WGS_Haplotype","altitude","Latitude","Longitude")

setwd("~/Documents/PhD_KSU/PhD_Documments/Thesis/WCM/plots")
jpeg('Figure3.jpeg', width = 12, height = 6.5, units = 'in', res = 300)
pdf('Figure3.pdf', width = 10, height = 4.5)
p2=ggplot() + geom_sf(data = worldmap) +  theme_bw() +
  coord_sf(xlim = c(30, 80), ylim = c(29, 46), expand = FALSE) +
  geom_point(data=passport,mapping = aes(x=Longitude, y=Latitude,color=WCM,shape=Lineage),cex=2,stroke=1.5, alpha=0.9) +
  scale_shape_manual(values=c(1,2)) +
  scale_color_gradient2(midpoint = mean(passport$WCM)-0.6, low = "green4", mid = "yellow2" , high = "red") +
  annotate(geom="text", x=35, y=39, label="TURKEY", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 38.5, y = 35, label = "SYRIA", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 43, y = 33, label = "IRAQ", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 54, y = 33, label = "IRAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 65, y = 33.5, label = "AFGHANISTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 71, y = 31, label = "PAKISTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 59, y = 39.5, label = "TURKMENISTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 62, y = 43, label = "UZBEKISTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 75, y = 42, label = "KYRGYZSTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 72, y = 38.5, label = "TAJIKISTAN", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 85, y = 36.5, label = "CHINA", color="grey50", size=3,fontface=2) +
  annotate(geom = "text", x = 69, y = 45, label = "KAZAKHSTAN", color="grey50", size=3,fontface=2)
dev.off()
