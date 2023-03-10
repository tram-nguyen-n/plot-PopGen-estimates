## Plot Relatedness Identity-by-Descent ##
## JAN 2023 ##
library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gridExtra)

setwd("~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/IBD/")
mydir= "~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/IBD/" #change to respective path
OUTPATH="~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/IBD/PLOTS"

#ABS C, ABS H, ONF, JDSP, PLE, SPSP
#colorlist<-c("#1b6ca8", "#7CBBDE", "#438876", "#61C3AA", "#C06029", "#F6B38C", "#B88706", "#E8B120", "#CD4262", "#E18DA0")
colorlist<-c("#3AB495", "#3AB495","#268FC8", "#268FC8", "#E6A908", "#E6A908", "#C06029", "#C06029", "#CD4262", "#CD4262")
## Across populations

#load in all IBD files
ibd_unfil<-read.delim("breeders_snps_hwe_noZ_genomind15_noLD.genome", as.is=T, header=T, sep = "")

##INDIVIDUALS
all.inds<-read.delim("~/Box Sync/PhD/Projects/Statewide WGS/Novogene/SAMPLE INFO/All_Novogene_individs_WGS20x_August2022.txt", as.is=T, header = T)

ABS_now<-all.inds[which(all.inds$Category == "ABS Contemporary"),] #30 individuals
ABS_hist<-all.inds[which(all.inds$Category == "ABS Historic"),] #27 individuals

JDSP_now<-all.inds[which(all.inds$Category == "JDSP Contemporary"),] #39 individuals
JDSP_hist<-all.inds[which(all.inds$Category == "JDSP Historic"),] #12 individuals

PLE_now<-all.inds[which(all.inds$Category == "PLE Contemporary"),] #26 individuals
PLE_hist<-all.inds[which(all.inds$Category == "PLE Historic"),] #25 individuals

ONF_now<-all.inds[which(all.inds$Category == "ONF Contemporary"),] #26 individuals
ONF_hist<-all.inds[which(all.inds$Category == "ONF Historic"),] #24 individuals

SPSP_hist<-all.inds[which(all.inds$Category == "SPSP Historic"),] #35 individuals
SPSP_now<-all.inds[which(all.inds$Category == "SPSP Contemporary"),] #18 individuals

#subset out populations (1990s and 2017)
#1990s

ABShist.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% ABS_hist$Novogene.ID & ibd_unfil$IID2 %in% ABS_hist$Novogene.ID),]
PLEhist.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% PLE_hist$Novogene.ID & ibd_unfil$IID2 %in% PLE_hist$Novogene.ID),]
JDSPhist.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% JDSP_hist$Novogene.ID & ibd_unfil$IID2 %in% JDSP_hist$Novogene.ID),]
ONFhist.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% ONF_hist$Novogene.ID & ibd_unfil$IID2 %in% ONF_hist$Novogene.ID),]
SPSPhist.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% SPSP_hist$Novogene.ID & ibd_unfil$IID2 %in% SPSP_hist$Novogene.ID),]

#get a vector of population labels to add onto dataframe




### AVERAGE YOUR IBD PER POPULATION
AvAhist_ID.list <- unique(c(ABShist.ibd$IID1, ABShist.ibd$IID2))
mean.IBD.c <- sapply(AvAhist_ID.list, function(c.id) 
{mean(ABShist.ibd$PI_HAT[which(ABShist.ibd$IID1 == c.id | ABShist.ibd$IID2 == c.id)])})
AvAhist_avg <- data.frame(mean.IBD.c)

#add row and column names
rownames(AvAhist_avg) <- c()
AvAhist_avg$ID <- AvAhist_ID.list
AvAhist_avg <- AvAhist_avg[, c("ID", "mean.IBD.c")]


### AVERAGE YOUR IBD PER POPULATION
PvPhist_ID.list <- unique(c(PLEhist.ibd$IID1, PLEhist.ibd$IID2))
mean.IBD.c <- sapply(PvPhist_ID.list, function(c.id) 
{mean(PLEhist.ibd$PI_HAT[which(PLEhist.ibd$IID1 == c.id | PLEhist.ibd$IID2 == c.id)])})
PvPhist_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(PvPhist_avg) <- c()
PvPhist_avg$ID <- PvPhist_ID.list
PvPhist_avg <- PvPhist_avg[, c("ID", "mean.IBD.c")]


### AVERAGE YOUR IBD PER POPULATION
OvOhist_ID.list <- unique(c(ONFhist.ibd$IID1, ONFhist.ibd$IID2))
mean.IBD.c <- sapply(OvOhist_ID.list, function(c.id) 
{mean(ONFhist.ibd$PI_HAT[which(ONFhist.ibd$IID1 == c.id | ONFhist.ibd$IID2 == c.id)])})
OvOhist_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(OvOhist_avg) <- c()
OvOhist_avg$ID <- OvOhist_ID.list
OvOhist_avg <- OvOhist_avg[, c("ID", "mean.IBD.c")]


### AVERAGE YOUR IBD PER POPULATION
JvJhist_ID.list <- unique(c(JDSPhist.ibd$IID1, JDSPhist.ibd$IID2))
mean.IBD.c <- sapply(JvJhist_ID.list, function(c.id) 
{mean(JDSPhist.ibd$PI_HAT[which(JDSPhist.ibd$IID1 == c.id | JDSPhist.ibd$IID2 == c.id)])})
JvJhist_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(JvJhist_avg) <- c()
JvJhist_avg$ID <- JvJhist_ID.list
JvJhist_avg <- JvJhist_avg[, c("ID", "mean.IBD.c")]


### AVERAGE YOUR IBD PER POPULATION
SvShist_ID.list <- unique(c(SPSPhist.ibd$IID1, SPSPhist.ibd$IID2))
mean.IBD.c <- sapply(SvShist_ID.list, function(c.id) 
{mean(SPSPhist.ibd$PI_HAT[which(SPSPhist.ibd$IID1 == c.id | SPSPhist.ibd$IID2 == c.id)])})
SvShist_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(SvShist_avg) <- c()
SvShist_avg$ID <- SvShist_ID.list
SvShist_avg <- SvShist_avg[, c("ID", "mean.IBD.c")]

#load in your data 
histibd.avg<- data.frame(IBD = c(AvAhist_avg$mean.IBD.c, OvOhist_avg$mean.IBD.c, JvJhist_avg$mean.IBD.c, PvPhist_avg$mean.IBD.c, SvShist_avg$mean.IBD.c), Population=rep(c("ABSvABS", "ONFvONF", "JDSPvJDSP", "PLEvPLE", "SPSPvSPSP"), c(length(AvAhist_avg$mean.IBD.c), length(OvOhist_avg$mean.IBD.c), length(JvJhist_avg$mean.IBD.c), length(PvPhist_avg$mean.IBD.c), length(SvShist_avg$mean.IBD.c))))

histibd.avg$IBD <- as.numeric(as.character(histibd.avg$IBD))

histibd.avg$Population <- factor(histibd.avg$Population, levels = c("ABSvABS", "ONFvONF", "JDSPvJDSP", "PLEvPLE", "SPSPvSPSP"))
histibd.avg<- histibd.avg[order(histibd.avg$Population), ]


require(ggplot2) 

hist.ibd.plot <- ggplot(data=histibd.avg, aes(x=histibd.avg$Population, y=histibd.avg$IBD, fill=Population)) +
  geom_boxplot() +
  xlab("")+ ylab("Average pairwise IBD") +
  scale_fill_manual(values=c("#268FC8", "#3AB495", "#F07834", "#E6A908", "#CD4262"))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.y=element_text(size = 14))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 12, 10, 22))
hist.ibd.plot<- hist.ibd.plot + ggtitle("Average relatedness in historic 1990s samples") + 
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust = 3))
hist.ibd.plot
#ggsave(hist.ibd.plot, file="IBDhist_3Jun20.png", height = 5, width = 7, units = "in", dpi=600)



############# Now do IBD for 2017 #############
#subset out populations (1990s and 2017)
#2017
ABSnow.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% ABS_now$Novogene.ID & ibd_unfil$IID2 %in% ABS_now$Novogene.ID),]
PLEnow.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% PLE_now$Novogene.ID & ibd_unfil$IID2 %in% PLE_now$Novogene.ID),]
JDSPnow.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% JDSP_now$Novogene.ID & ibd_unfil$IID2 %in% JDSP_now$Novogene.ID),]
ONFnow.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% ONF_now$Novogene.ID & ibd_unfil$IID2 %in% ONF_now$Novogene.ID),]
SPSPnow.ibd <-ibd_unfil[which(ibd_unfil$IID1 %in% SPSP_now$Novogene.ID & ibd_unfil$IID2 %in% SPSP_now$Novogene.ID),]

#get a vector of population labels to add onto dataframe
ABSnow.pop<-c(rep("ABS2017", nrow(ABSnow.ibd)))
PLEnow.pop<-c(rep("PLE2017", nrow(PLEnow.ibd)))
ONFnow.pop<-c(rep("ONF2017", nrow(ONFnow.ibd)))
JDSPnow.pop<-c(rep("JDSP2017", nrow(JDSPnow.ibd)))
SPSPnow.pop<-c(rep("SAV2017", nrow(SPSPnow.ibd)))


### AVERAGE YOUR IBD PER POPULATION
AvAnow_ID.list <- unique(c(ABSnow.ibd$IID1, ABSnow.ibd$IID2))
mean.IBD.c <- sapply(AvAnow_ID.list, function(c.id) 
{mean(ABSnow.ibd$PI_HAT[which(ABSnow.ibd$IID1 == c.id | ABSnow.ibd$IID2 == c.id)])})
AvAnow_avg <- data.frame(mean.IBD.c)

#add row and column names
rownames(AvAnow_avg) <- c()
AvAnow_avg$ID <- AvAnow_ID.list
AvAnow_avg <- AvAnow_avg[, c("ID", "mean.IBD.c")]

#----------------------------
### AVERAGE YOUR IBD PER POPULATION

PvPnow_ID.list <- unique(c(PLEnow.ibd$IID1, PLEnow.ibd$IID2))
mean.IBD.c <- sapply(PvPnow_ID.list, function(c.id) 
{mean(PLEnow.ibd$PI_HAT[which(PLEnow.ibd$IID1 == c.id | PLEnow.ibd$IID2 == c.id)])})
PvPnow_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(PvPnow_avg) <- c()
PvPnow_avg$ID <- PvPnow_ID.list
PvPnow_avg <- PvPnow_avg[, c("ID", "mean.IBD.c")]

#----------------------------
### AVERAGE YOUR IBD PER POPULATION
OvOnow_ID.list <- unique(c(ONFnow.ibd$IID1, ONFnow.ibd$IID2))
mean.IBD.c <- sapply(OvOnow_ID.list, function(c.id) 
{mean(ONFnow.ibd$PI_HAT[which(ONFnow.ibd$IID1 == c.id | ONFnow.ibd$IID2 == c.id)])})
OvOnow_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(OvOnow_avg) <- c()
OvOnow_avg$ID <- OvOnow_ID.list
OvOnow_avg <- OvOnow_avg[, c("ID", "mean.IBD.c")]

#----------------------------
### AVERAGE YOUR IBD PER POPULATION
JvJnow_ID.list <- unique(c(JDSPnow.ibd$IID1, JDSPnow.ibd$IID2))
mean.IBD.c <- sapply(JvJnow_ID.list, function(c.id) 
{mean(JDSPnow.ibd$PI_HAT[which(JDSPnow.ibd$IID1 == c.id | JDSPnow.ibd$IID2 == c.id)])})
JvJnow_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(JvJnow_avg) <- c()
JvJnow_avg$ID <- JvJnow_ID.list
JvJnow_avg <- JvJnow_avg[, c("ID", "mean.IBD.c")]

#-------------------------------------
### AVERAGE YOUR IBD PER POPULATION
SvSnow_ID.list <- unique(c(SPSPnow.ibd$IID1, SPSPnow.ibd$IID2))
mean.IBD.c <- sapply(SvSnow_ID.list, function(c.id) 
{mean(SPSPnow.ibd$PI_HAT[which(SPSPnow.ibd$IID1 == c.id | SPSPnow.ibd$IID2 == c.id)])})
SvSnow_avg <- data.frame(mean.IBD.c)
#add row and column names
rownames(SvSnow_avg) <- c()
SvSnow_avg$ID <- SvSnow_ID.list
SvSnow_avg <- SvSnow_avg[, c("ID", "mean.IBD.c")]


#load in your data 
nowibd.avg<- data.frame(IBD = c(AvAnow_avg$mean.IBD.c, OvOnow_avg$mean.IBD.c, JvJnow_avg$mean.IBD.c, PvPnow_avg$mean.IBD.c, SvSnow_avg$mean.IBD.c), Population=rep(c("ABSvABS", "ONFvONF", "JDSPvJDSP", "PLEvPLE", "SPSPvSPSP"), c(length(AvAnow_avg$mean.IBD.c), length(OvOnow_avg$mean.IBD.c), length(JvJnow_avg$mean.IBD.c), length(PvPnow_avg$mean.IBD.c), length(SvSnow_avg$mean.IBD.c))))

nowibd.avg$IBD <- as.numeric(as.character(nowibd.avg$IBD))
nowibd.avg$Population <- factor(nowibd.avg$Population, levels = c("ABSvABS", "ONFvONF", "JDSPvJDSP", "PLEvPLE", "SPSPvSPSP"))
nowibd.avg<- nowibd.avg[order(nowibd.avg$Population), ]


require(ggplot2) 

now.ibd.plot <- ggplot(data=nowibd.avg, aes(x=nowibd.avg$Population, y=nowibd.avg$IBD, fill=Population)) +
  geom_boxplot() +
  xlab("Pairwise comparisons")+ ylab("Average pairwise IBD") +
  scale_fill_manual(values=c("#268FC8", "#3AB495", "#F07834", "#E6A908", "#CD4262"))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.y=element_text(size = 14))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 12, 10, 22))
now.ibd.plot<- now.ibd.plot + ggtitle("Average relatedness in contemporary 2017 samples") + 
  theme(plot.title = element_text(size=18, hjust = 0.5, vjust = 3))
now.ibd.plot
#ggsave(now.ibd.plot, file="IBDnow_3Jun20.png", height = 5, width = 7, units = "in", dpi=600)

grid.arrange(hist.ibd.plot, now.ibd.plot, nrow = 2)


'################### COMBINE IBD YEARS ######################'
histibd.avg$Population<-rep(c("ABSvABS 1990", "ONFvONF 1990", "JDSPvJDSP 1990", "PLEvPLE 1990", "SPSPvSPSP 1990"), c(length(AvAhist_avg$mean.IBD.c), length(OvOhist_avg$mean.IBD.c), length(JvJhist_avg$mean.IBD.c), length(PvPhist_avg$mean.IBD.c), length(SvShist_avg$mean.IBD.c)))

nowibd.avg$Population<-rep(c("ABSvABS 2017", "ONFvONF 2017", "JDSPvJDSP 2017", "PLEvPLE 2017", "SPSPvSPSP 2017"), c(length(AvAnow_avg$mean.IBD.c), length(OvOnow_avg$mean.IBD.c), length(JvJnow_avg$mean.IBD.c), length(PvPnow_avg$mean.IBD.c), length(SvSnow_avg$mean.IBD.c)))

combined_ibd<-rbind(histibd.avg, nowibd.avg)

#reorder our populations to plot them 
combined_ibd$Population <- factor(combined_ibd$Population, levels = c("ONFvONF 1990", "ONFvONF 2017", "ABSvABS 1990", "ABSvABS 2017", "PLEvPLE 1990", "PLEvPLE 2017", "JDSPvJDSP 1990", "JDSPvJDSP 2017", "SPSPvSPSP 1990", "SPSPvSPSP 2017"))
combined_ibd<- combined_ibd[order(combined_ibd$Population), ]

comp<- list(c("ABSvABS 1990", "ABSvABS 2017"), c("ONFvONF 1990", "ONFvONF 2017"))

combined_ibd.plot<-ggplot(data=combined_ibd, aes(x=Population, y=IBD, fill=Population)) + 
  geom_boxplot(alpha=0.95) +
  xlab("Pairwise comparisons")+ ylab("Average pairwise IBD") +
  scale_fill_manual(values=colorlist)+
  theme_bw() +
  ylim(c(-0.005, 0.32))+
  #stat_compare_means(comparisons = comp, method = "t.test", na.rm = T, label = "p.signif", size = 6)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=14, angle = 30, vjust = 0.5), axis.text.y=element_text(size = 14))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 12, 10, 22))
combined_ibd.plot
#combined_ibd.plot<- combined_ibd.plot + ggtitle("Average pairwise IBD across all samples") + 
  #theme(plot.title = element_text(size=18, hjust = 0.5))
combined_ibd.plot <- combined_ibd.plot + geom_jitter(size=1.5, shape=19, position=position_jitter(0))
combined_ibd.plot

ggsave(combined_ibd.plot, file="PLOTS/PairIBD_noStats.pdf", height = 8, width = 14, units = "in", device="pdf", dpi=800)




######---------------------------- Check ABS and PLE COMPARISON -----------------------------######

## test whether ABS and PLE are signficantly different
wilcox.test(AvAhist_avg$mean.IBD.c, PvPhist_avg$mean.IBD.c, alternative = "less") # in 1990 ABS was significantly LOWER than PLE
wilcox.test(AvAnow_avg$mean.IBD.c, PvPnow_avg$mean.IBD.c, alternative = "less") # LOWER than PLE
# in both years, ABS lower in per pop IBD which matches our PLE paper


