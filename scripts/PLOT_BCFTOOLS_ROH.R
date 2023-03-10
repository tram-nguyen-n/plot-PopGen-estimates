## PLOT BCFTOOLS ROHS ##
# JANUARY 8, 2023
# files were produced on the lm11 "make_segment_file.r" and downloaded for this script to local
setwd("~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/ROH/BCFTOOLS")
library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gridExtra)

# same tone
colorlist<-c("#3AB495", "#3AB495", "#268FC8", "#268FC8","#E6A908", "#E6A908", "#C06029", "#C06029", "#CD4262", "#CD4262")
# two tone
colorlist<-c("#0c9c5f", "#61C3AA", "#1b6ca8", "#7CBBDE", "#e09719", "#f6cd61","#e07838", "#eb9967", "#CD4262", "#ff7393")


#########################################################################################################################
######################################### GENERATE PER INDIVIDUAL ROH SUMMARY FILE ######################################
#########################################################################################################################
all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
all_segments<- all_segments[order(all_segments$Population), ]

# mean length
totLength <- all_segments %>% group_by(Sample, Population) %>% 
  summarise(total=sum(Length),
            .groups = 'drop')

# tot NSNPS
totSNPs<- all_segments %>% group_by(Sample, Population) %>% 
  summarise(total=sum(NSNP),
            .groups = 'drop')

# mean length
mLength <- all_segments %>% group_by(Sample, Population) %>% 
  summarise(meanBP=mean(Length),
            .groups = 'drop')

# numROHs
totROH<- all_segments %>% group_by(Sample, Population) %>% tally()

# merge all together
roh.ind <- cbind(totROH, totSNPs$total, totLength$total, mLength$meanBP)
colnames(roh.ind) <- c("Sample", "Population", "NumROH", "NSNPS", "TotLENGTH", "MeanLENGTH")

# get number of individuals per pop
table(roh.ind$Population)
#ONFhist  ONFcontemp     ABShist  ABScontemp     PLEhist  PLEcontemp    JDSPhist JDSPcontemp    SPSPhist SPSPcontemp 
#22          26          28          28          25          22          14          48          18          10

roh.ind$ROHperInd <- NA

# divide NumROH by number of individuals in pop
# need to figure out an easier way to do -- but for now doing it manually JAN 2023
c.df <- subset(roh.ind, roh.ind$Population == "SPSPcontemp")
roh.ind$ROHperInd[roh.ind$Population == "SPSPcontemp"]  <- c.df$NumROH/10

# add Column for age
contemp<-grep("contemp", x = roh.ind$Population)
hist<-grep("hist", x = roh.ind$Population)
roh.ind$Age[contemp] <- "Contemporary"
roh.ind$Age[hist] <- "Historic"

roh.ind$Age <- as.factor(roh.ind$Age) 

#write.table(roh.ind, "IndROH_bcftools_2022.txt", col.names = T, row.names = F, quote = F, sep="\t")


################################# PLOT NUM ROH PER INDIVIDUAL ##########################################
roh.ind <- read.table("IndROH_bcftools_2022.txt", header=T, as.is=T, sep="\t")
roh.ind$Population <- factor(roh.ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
roh.ind<- roh.ind[order(roh.ind$Population), ]

p <- ggplot(roh.ind, aes(x=Population, y=ROHperInd, fill=Population))+
  geom_bar(stat = "identity", position="dodge", alpha=1)+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("") + ylab("ROH segments per individual")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-20, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  ggtitle("ROH lengths per individual")+
  #scale_x_discrete(labels=c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))
p <- p+ scale_fill_manual(values=colorlist)
p

ggsave(p, filename = "PLOTS/all_ROH_counts.png", height = 8, width=12, units = "in", device = "png", dpi=800)



######################## RAW ALL SEGMENT FILE WITHOUT ANY ROH QUALITY FILTERING ##########################
raw <- read.table("RAW_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
raw$Population <- factor(raw$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
raw<- raw[order(raw$Population), ]

# mean length
totLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(Length),
            .groups = 'drop')

totSNPs<- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(NSNP),
            .groups = 'drop')

mLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(meanBP=mean(Length),
            .groups = 'drop')

totROH<- raw %>% group_by(Sample, Population) %>% tally()

# merge all together
roh.ind.raw <- cbind(totROH, totSNPs$total, totLength$total, mLength$meanBP)
colnames(roh.ind.raw) <- c("Sample", "Population", "NumROH", "NSNPS", "TotLENGTH", "MeanLENGTH")

# add Column for age
roh.ind.raw$Age <- "NA"
contemp<-grep("contemp", x = roh.ind.raw$Population)
hist<-grep("hist", x = roh.ind.raw$Population)
roh.ind.raw$Age[contemp] <- "Contemporary"
roh.ind.raw$Age[hist] <- "Historic"
roh.ind.raw$Age <- as.factor(roh.ind.raw$Age) 

ggplot(roh.ind.raw, aes(x=TotLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl



######################## HIGH QUALITY ROH ONLY ########################
raw <- read.table("RAW_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
raw$Population <- factor(raw$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
raw<- raw[order(raw$Population), ]

raw <- subset(raw, raw$Quality >=20 & raw$NSNP >= 100)

# mean length
totLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(Length),
            .groups = 'drop')

totSNPs<- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(NSNP),
            .groups = 'drop')

mLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(meanBP=mean(Length),
            .groups = 'drop')

totROH<- raw %>% group_by(Sample, Population) %>% tally()

# merge all together
roh.ind.raw <- cbind(totROH, totSNPs$total, totLength$total, mLength$meanBP)
colnames(roh.ind.raw) <- c("Sample", "Population", "NumROH", "NSNPS", "TotLENGTH", "MeanLENGTH")

# add Column for age
roh.ind.raw$Age <- "NA"
contemp<-grep("contemp", x = roh.ind.raw$Population)
hist<-grep("hist", x = roh.ind.raw$Population)
roh.ind.raw$Age[contemp] <- "Contemporary"
roh.ind.raw$Age[hist] <- "Historic"
roh.ind.raw$Age <- as.factor(roh.ind.raw$Age) 

ggplot(roh.ind.raw, aes(x=MeanLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  ggtitle("Quality >=20 & NSNP >= 100")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl



######################## ROH PER INDIVIDUAL OF ONLY SEGMENTS > 50kb ####################
raw <- read.table("RAW_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
raw$Population <- factor(raw$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
raw<- raw[order(raw$Population), ]

raw <- subset(raw, raw$Length >= 100000) # to match plink. smallest ROH = 100kb
range(raw$NSNP)
range(raw$Quality)
raw <- subset(raw, raw$Quality >=20 & raw$NSNP >= 100)
summary(raw$Length)

# mean length
totLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(Length),
            .groups = 'drop')

totSNPs<- raw %>% group_by(Sample, Population) %>% 
  summarise(total=sum(NSNP),
            .groups = 'drop')

mLength <- raw %>% group_by(Sample, Population) %>% 
  summarise(meanBP=mean(Length),
            .groups = 'drop')

totROH<- raw %>% group_by(Sample, Population) %>% tally()

# merge all together
roh.ind.raw <- cbind(totROH, totSNPs$total, totLength$total, mLength$meanBP)
colnames(roh.ind.raw) <- c("Sample", "Population", "NumROH", "NSNPS", "TotLENGTH", "MeanLENGTH")

# add Column for age
roh.ind.raw$Age <- "NA"
contemp<-grep("contemp", x = roh.ind.raw$Population)
hist<-grep("hist", x = roh.ind.raw$Population)
roh.ind.raw$Age[contemp] <- "Contemporary"
roh.ind.raw$Age[hist] <- "Historic"
roh.ind.raw$Age <- as.factor(roh.ind.raw$Age) 

ggplot(roh.ind.raw, aes(x=MeanLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=3, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  ggtitle("Matching PLINK: Quality > 20, NSNP > 100, MinLength 100 Kb")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl


colorlist<-c("#0c9c5f", "#61C3AA", "#1b6ca8", "#7CBBDE", "#e09719", "#f6cd61","#CD4262", "#ff7393","#f37121", "#e88f58")

ggplot(roh.ind.raw, aes(x=TotLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  xlim(c(100,4e05))+
  theme_minimal()+
  ggtitle("Matching PLINK: Quality > 20, NSNP > 100, MinLength 100 Kb")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl

### MATCHING OUR PLINK EXPECTATIONS NOW !!! BECAUSE OF THE LENGTH CUT OFF !!!

#########################################################################################################################
########################################### PLOT PROPORTION OF ROH SEGMENTS FOUND #######################################
#########################################################################################################################
all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
x <- split(all_segments, all_segments$Population)
desired_length <- 10
roh.list <- vector(mode = "list", length = desired_length)
str(roh.list)
names<-as.vector(names(x))
names(roh.list) <- names

for (i in 1:10){
  c.df<-data.frame(x[[i]])
  nseg<-data.frame(nrow(x[[i]]))
  nind <- length(unique(c.df$Sample))
  ratio <- nseg/nind
  roh.list[[i]]<-ratio
}

raw_roh<-ldply(roh.list, rbind) # combine all the lists together
colnames(raw_roh)<-c("Population", "Roh_ratio")

raw_roh$Population <- factor(raw_roh$Population, levels = c("ONFhist", "ONFcontemp", "ABShist", "ABScontemp", "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
raw_roh<- raw_roh[order(raw_roh$Population), ]
#raw_roh_num$Population<-c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017")

raw_roh.p<-ggplot(raw_roh, aes(x=Population, y=Roh_ratio, fill=Population))+
  geom_bar(stat = "identity", position="dodge", alpha=1)+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("") + ylab("ROH segments per individual")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  #ggtitle("ROH lengths per individual in ABS and PLE")+
  scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))
raw_roh.p <- raw_roh.p+ scale_fill_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#f37121", "#e88f58", "#e09719", "#f6cd61", "#CD4262", "#ff7393"))
#scale_fill_manual(values=c("#268FC8", "#268FC8", "#3AB495", "#3AB495", "#CD4262", "#CD4262", "#E6A908", "#E6A908"))
raw_roh.p
#ggsave(raw_roh_count, filename = paste0(OUTPATH, "/all_ROH_counts.png"), height =8, width=11, units = "in", device = "png", dpi=700)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14309   36711   48034   46978   57529   8469 ## raw ROH segment counts


#########################################################################################################################
########################################## SCATTERPLOT LENGTH VS NSNP IN EACH ROH #######################################
#########################################################################################################################
all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
all_segments<- all_segments[order(all_segments$Population), ]

# add Column for age
contemp<-grep("contemp", x = all_segments$Population)
hist<-grep("hist", x = all_segments$Population)
all_segments$Age[contemp] <- "Contemporary"
all_segments$Age[hist] <- "Historic"

all_segments$Age <- as.factor(all_segments$Age) 

segments_plot<-ggplot(all_segments, aes(x=Length/1000, y=NSNP, color=Population))+
  geom_point(aes(shape=Age, color=Population), size=5)+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Length of ROH (kb)") + ylab("Num SNPs in ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) +  scale_color_manual(values=colorlist)
segments_plot
#ggsave(segments_plot, filename = paste0(OUTPATH, "/ROH_KB_NSNP.png"), height =8, width=9, units = "in", device = "png", dpi=700)

summary(all_segments$Length)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#155    14211    25647    76336    53181 26458872 --- mean is 75kb
summary(all_segments$NSNP)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#10.0     94.0    152.0    399.3    291.0 122752.0 


#########################################################################################################################
###################################### SCATTERPLOT MEAN LENGTH VS NSNP IN EACH ROH ######################################
#########################################################################################################################
roh.ind <- read.delim("IndROH_bcftools_2022.txt", header = T, as.is=T, sep="\t")

roh.ind$Population <- factor(roh.ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
roh.ind<- roh.ind[order(roh.ind$Population), ]

segment_plot<-ggplot(roh.ind, aes(x=MeanLENGTH, y=NSNPS, color=Population))+
  geom_point(alpha=0.8, size = 2.5)+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Length of ROH (kb)") + ylab("Num SNPs in ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) +  scale_color_manual(values=colorlist)
segment_plot
#ggsave(segments_plot, filename = paste0(OUTPATH, "/ROH_KB_NSNP.png"), height =8, width=9, units = "in", device = "png", dpi=700)

summary(all_segments$Length)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#155    14211    25647    76336    53181 26458872 


#########################################################################################################################   
################################ SCATTERPLOT AVG ROH LENGTH VS # ROHs PER INDIVIDUAL  ###################################
#########################################################################################################################
roh.ind <- read.delim("IndROH_bcftools_2022.txt", header = T, as.is=T, sep="\t")
roh.ind$Population <- factor(roh.ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
roh.ind<- roh.ind[order(roh.ind$Population), ]

roh.ind$Population <- as.factor(roh.ind$Population) 
roh.ind$NumROH <- as.numeric(roh.ind$NumROH)

### CIRCLES FOR CONTEMP, TRIANGLES FOR HIST
ind_roh_scatter<-ggplot(roh.ind, aes(x=MeanLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=3, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
ggsave(ind_roh_scatter, filename = "PLOTS/scatter_AvgROH.pdf", height=5.5, width=9, units ="in", device ="pdf", dpi=700)

ind_roh_scatter<-ggplot(roh.ind, aes(x=TotLENGTH/1000000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=3, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Total ROH lengths (MB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
ggsave(ind_roh_scatter, filename = "PLOTS/scatter_TotROH.pdf", height=5.5, width=9, units ="in", device ="pdf", dpi=700)

################### MAYBE THE NUMBER OF ROHS PER INDIVIDUAL IS DICTATED A LOT BY SMALLER ROH... #########################
roh.ind <- read.delim("IndROH_bcftools_2022.txt", header = T, as.is=T, sep="\t")
roh.ind$Population <- factor(roh.ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
roh.ind<- roh.ind[order(roh.ind$Population), ]

roh.ind$Population <- as.factor(roh.ind$Population) 
roh.ind$NumROH <- as.numeric(roh.ind$NumROH)

### CIRCLES FOR CONTEMP, TRIANGLES FOR HIST
ind_roh_scatter<-ggplot(roh.ind, aes(x=MeanLENGTH/1000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=3, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH segments")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
ggsave(ind_roh_scatter, filename = "PLOTS/scatter_AvgROH.pdf", height=5.5, width=9, units ="in", device ="pdf", dpi=700)

ind_roh_scatter<-ggplot(roh.ind, aes(x=TotLENGTH/1000000, y=NumROH, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=3, alpha=0.9)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Total ROH lengths (MB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
ggsave(ind_roh_scatter, filename = "PLOTS/scatter_TotROH.pdf", height=5.5, width=9, units ="in", device ="pdf", dpi=700)



#########################################################################################################################
################################## DISTRIBUTION OF ROH LENGTHS (SIMILAR TO PLE PAPER)  ##################################
#########################################################################################################################

########################### SHORTEST SEGMENTS < 1 MB FIRST ########################
all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
all_segments<- all_segments[order(all_segments$Population), ]

# change column names
colnames(all_segments) <- c("Population", "RG", "INDV", "CHR", "START", "END", "LENGTH", "NUM_SNPS", "QUAL")
all_segments$Population <- as.factor(all_segments$Population)

# remove duplicated individuals from both years and only include in historic -- NEED TO DO STILL

# change pop names to years (for ease of the for loops)
all_segments$Population <- gsub("contemp", " 2017", all_segments$Population)
all_segments$Population <- gsub("hist", " 1992", all_segments$Population)
unique(all_segments$Population)

# change the name to match the for loop -- since we are adapting an old script 
allROH <- all_segments

# get a count of segments for each population
count_seg<-data.frame(matrix(0, nrow=11, ncol=4)) # number of pops plus one for rows
rownames(count_seg)<-c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017", "Else")
colnames(count_seg)<-c("0-25Kb","25-50Kb", "50-75Kb","75-1MB")

# count 0-25Kb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]<25000) ==TRUE){
    count_seg[1,1]<-1+count_seg[1,1]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[2,1]<-1+count_seg[2, 2]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]<25000) ==TRUE){
    count_seg[3,1]<-1+count_seg[3,1]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[4,1]<-1+count_seg[4,1]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[5,1]<-1+count_seg[5,1]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[6,1]<-1+count_seg[6,1]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[7,1]<-1+count_seg[7,1]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[8,1]<-1+count_seg[8,1]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[9,1]<-1+count_seg[9,1]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]<25000) == TRUE){
    count_seg[10,1]<-1+count_seg[10,1]
  } else count_seg[11,1]<-1+count_seg[11,1]
}


# count 25-50Kb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) ==TRUE){
    count_seg[1,2]<-1+count_seg[1,2]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[2,2]<-1+count_seg[2,2]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) ==TRUE){
    count_seg[3,2]<-1+count_seg[3,2]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[4,2]<-1+count_seg[4,2]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[5,2]<-1+count_seg[5,2]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[6,2]<-1+count_seg[6,2]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[7,2]<-1+count_seg[7,2]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[8,2]<-1+count_seg[8,2]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[9,2]<-1+count_seg[9,2]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=25000 & allROH$LENGTH[i]<50000) == TRUE){
    count_seg[10,2]<-1+count_seg[10,2]
  } else count_seg[11,2]<-1+count_seg[11,2]
}


# count 50-75kb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) ==TRUE){
    count_seg[1,3]<-1+count_seg[1,3]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[2,3]<-1+count_seg[2,3]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) ==TRUE){
    count_seg[3,3]<-1+count_seg[3,3]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[4,3]<-1+count_seg[4,3]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[5,3]<-1+count_seg[5,3]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[6,3]<-1+count_seg[6,3]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[7,3]<-1+count_seg[7,3]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[8,3]<-1+count_seg[8,3]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[9,3]<-1+count_seg[9,3]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=50000 & allROH$LENGTH[i]<75000) == TRUE){
    count_seg[10,3]<-1+count_seg[10,3]
  } else count_seg[11,3]<-1+count_seg[11,3]
}


# count 75-1MB
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) ==TRUE){
    count_seg[1,4]<-1+count_seg[1,4]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[2,4]<-1+count_seg[2,4]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) ==TRUE){
    count_seg[3,4]<-1+count_seg[3,4]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[4,4]<-1+count_seg[4,4]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[5,4]<-1+count_seg[5,4]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[6,4]<-1+count_seg[6,4]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[7,4]<-1+count_seg[7,4]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[8,4]<-1+count_seg[8,4]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<1000000) == TRUE){
    count_seg[9,4]<-1+count_seg[9,4]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=75000 & allROH$LENGTH[i]<10000000) == TRUE){
    count_seg[10,4]<-1+count_seg[10,4]
  } else count_seg[11,4]<-1+count_seg[11,4]
}

# check -- manually look that it matches your table
nrow(allROH[which(allROH$Population == "PLE 2017" & allROH$LENGTH>=75000 & allROH$LENGTH<1000000),])

### GET PROPORTIONS OF EACH LENGTH CLASS
#divide by number of ROHs in each group
pop.vec <- c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017")

prop_seg_less1MB<-data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:10){
  cpop <- pop.vec[i]
  pr <- as.numeric(count_seg[i,]/(nrow(allROH[which(allROH$Population == cpop),])))
  prop_seg_less1MB<-rbind(prop_seg_less1MB,pr)
}

rownames(prop_seg_less1MB)<-c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017")
colnames(prop_seg_less1MB)<-c("0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.0")
prop_seg_less1MB

# CHECK -- manually compare one example. Look in count_seg + prop_seg_less and compare to this number
cpop="SPSP 1992"
9210/nrow(allROH[which(allROH$Population == cpop),]) #for 25-50kb

### pivot longer
library(tidyverse)
less_tidy <-
  pivot_longer(prop_seg_less1MB, c("0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.0"), names_to = 'LENGTH_bins', values_to = 'Proportions')

pop<-rep(c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017"),each=4)
less_tidy$Pop<-pop

#less_tidy<-read.table("less1MB_proportion_bcftools.txt", header  = T, sep = '\t')

#REORDER
less_tidy$LENGTH_bins <- factor(less_tidy$LENGTH_bins, levels = c("0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.0"))
less_tidy<- less_tidy[order(less_tidy$LENGTH_bins), ]
less_tidy$Pop <- factor(less_tidy$Pop, levels = c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017"))
less_tidy<- less_tidy[order(less_tidy$Pop), ]

#write.table(less_tidy, "less5MB_proportion_bcftools.txt", col.names = T, quote = F, row.names = F, sep = '\t')

library(ggplot2)
p<- ggplot(less_tidy,aes(y = Proportions, x = LENGTH_bins, fill=Pop))+
  geom_bar(alpha=1, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = "")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())
p<-p+ ggtitle("(A) ROH < 1Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
p
p <-p+  scale_fill_manual(values=colorlist)
p
ggsave(p, filename = "PLOTS/prop_short.pdf", height = 8, width = 11, units = "in", dpi=700, device ="pdf")

########################################## LONG SEGMENTS > 1 MB ##########################################

all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
all_segments<- all_segments[order(all_segments$Population), ]

# change column names
colnames(all_segments) <- c("Population", "RG", "INDV", "CHR", "START", "END", "LENGTH", "NUM_SNPS", "QUAL")
all_segments$Population <- as.factor(all_segments$Population)

# remove duplicated individuals from both years and only include in historic -- CHECKED AND THERE ARE NONE OVERLAP JAN2023

# change pop names to years (for ease of the for loops)
all_segments$Population <- gsub("contemp", " 2017", all_segments$Population)
all_segments$Population <- gsub("hist", " 1992", all_segments$Population)
unique(all_segments$Population)

# change the name to match the for loop -- since we are adapting an old script 
allROH <- all_segments

# get a count of segments for each population
count_seg<-data.frame(matrix(0, nrow=11, ncol=5)) # number of pops plus one
rownames(count_seg)<-c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017", "Else")
colnames(count_seg)<-c("1-2MB", "2-3MB", "3-4MB", "4-5MB", "5-30MB")

# count 1-2mb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) ==TRUE){
    count_seg[1,1]<-1+count_seg[1,1]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[2,1]<-1+count_seg[2,1]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) ==TRUE){
    count_seg[3,1]<-1+count_seg[3,1]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[4,1]<-1+count_seg[4,1]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[5,1]<-1+count_seg[5,1]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[6,1]<-1+count_seg[6,1]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[7,1]<-1+count_seg[7,1]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[8,1]<-1+count_seg[8,1]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[9,1]<-1+count_seg[9,1]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=1000000 & allROH$LENGTH[i]<2000000) == TRUE){
    count_seg[10,1]<-1+count_seg[10,1]
  } else count_seg[11,1]<-1+count_seg[11,1]
}

# count 2-3mb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) ==TRUE){
    count_seg[1,2]<-1+count_seg[1,2]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[2,2]<-1+count_seg[2,2]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) ==TRUE){
    count_seg[3,2]<-1+count_seg[3,2]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[4,2]<-1+count_seg[4,2]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[5,2]<-1+count_seg[5,2]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[6,2]<-1+count_seg[6,2]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[7,2]<-1+count_seg[7,2]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[8,2]<-1+count_seg[8,2]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[9,2]<-1+count_seg[9,2]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=2000000 & allROH$LENGTH[i]<3000000) == TRUE){
    count_seg[10,2]<-1+count_seg[10,2]
  } else count_seg[11,2]<-1+count_seg[11,2]
}

# count 3-4mb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) ==TRUE){
    count_seg[1,3]<-1+count_seg[1,3]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[2,3]<-1+count_seg[2,3]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) ==TRUE){
    count_seg[3,3]<-1+count_seg[3,3]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[4,3]<-1+count_seg[4,3]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[5,3]<-1+count_seg[5,3]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[6,3]<-1+count_seg[6,3]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[7,3]<-1+count_seg[7,3]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[8,3]<-1+count_seg[8,3]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[9,3]<-1+count_seg[9,3]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=3000000 & allROH$LENGTH[i]<4000000) == TRUE){
    count_seg[10,3]<-1+count_seg[10,3]
  } else count_seg[11,3]<-1+count_seg[11,3]
}


# count 4-5 -- do this OR the next loop -- comment all of it on or off shift+ctrl+c
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) ==TRUE){
    count_seg[1,4]<-1+count_seg[1,4]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[2,4]<-1+count_seg[2,4]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) ==TRUE){
    count_seg[3,4]<-1+count_seg[3,4]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[4,4]<-1+count_seg[4,4]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[5,4]<-1+count_seg[5,4]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[6,4]<-1+count_seg[6,4]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[7,4]<-1+count_seg[7,4]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[8,4]<-1+count_seg[8,4]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[9,4]<-1+count_seg[9,4]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=4000000 & allROH$LENGTH[i]<5000000) == TRUE){
    count_seg[10,4]<-1+count_seg[10,4]
  } else count_seg[11,4]<-1+count_seg[11,4]
}

# > 4mb
# for (i in 1:nrow(allROH)){
#   if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=4000000) ==TRUE){
#     count_seg[1,4]<-1+count_seg[1,4]
#   }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[2,4]<-1+count_seg[2,4]
#   }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=4000000) ==TRUE){
#     count_seg[3,4]<-1+count_seg[3,4]
#   }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[4,4]<-1+count_seg[4,4]
#   }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[5,4]<-1+count_seg[5,4]
#   }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[6,4]<-1+count_seg[6,4]
#   }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[7,4]<-1+count_seg[7,4]
#   }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[8,4]<-1+count_seg[8,4]
#   }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[9,4]<-1+count_seg[9,4]
#   }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=4000000) == TRUE){
#     count_seg[10,4]<-1+count_seg[10,4]
#   } else count_seg[11,4]<-1+count_seg[11,4]
# }

# 5-30mb
for (i in 1:nrow(allROH)){
  if ((allROH$Population[i] == "ONF 1992" & allROH$LENGTH[i]>=5000000) ==TRUE){
    count_seg[1,5]<-1+count_seg[1,5]
  }else if ((allROH$Population[i] == "ONF 2017" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[2,5]<-1+count_seg[2,5]
  }else if ((allROH$Population[i] == "ABS 1992" & allROH$LENGTH[i]>=5000000) ==TRUE){
    count_seg[3,5]<-1+count_seg[3,5]
  }else if ((allROH$Population[i] == "ABS 2017" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[4,5]<-1+count_seg[4,5]
  }else if ((allROH$Population[i] == "PLE 1992" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[5,5]<-1+count_seg[5,5]
  }else if ((allROH$Population[i] == "PLE 2017" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[6,5]<-1+count_seg[6,5]
  }else if ((allROH$Population[i] == "JDSP 1992" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[7,5]<-1+count_seg[7,5]
  }else if ((allROH$Population[i] == "JDSP 2017" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[8,5]<-1+count_seg[8,5]
  }else if ((allROH$Population[i] == "SPSP 1992" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[9,5]<-1+count_seg[9,5]
  }else if ((allROH$Population[i] == "SPSP 2017" & allROH$LENGTH[i]>=5000000) == TRUE){
    count_seg[10,5]<-1+count_seg[10,5]
  } else count_seg[11,5]<-1+count_seg[11,5]
}

# check -- manually look that it matches your table
nrow(allROH[which(allROH$Population == "PLE 2017" & allROH$LENGTH>=2000000 & allROH$LENGTH<3000000),])

#divide by number of ROHs in each group
pop.vec <- c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017")

prop_seg_less5MB<-data.frame(matrix(ncol = 5, nrow = 0))

for (i in 1:10){
  cpop <- pop.vec[i]
  pr <- as.numeric(count_seg[i,]/(nrow(allROH[which(allROH$Population == cpop),])))
  prop_seg_less5MB<-rbind(prop_seg_less5MB,pr)
}

rownames(prop_seg_less5MB)<-c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017")
colnames(prop_seg_less5MB)<-c("1-2MB", "2-3MB", "3-4MB", "4-5MB", "5-30MB")

prop_seg_less5MB<-data.frame(prop_seg_less5MB)
colnames(prop_seg_less5MB)<-c("1-2", "2-3", "3-4", "4-5", "5-30")

# CHECK -- manually compare one example
cpop="SPSP 1992"
nrow(subset(allROH, Population==cpop & allROH$LENGTH>=2000000 & allROH$LENGTH<3000000))/nrow(allROH[which(allROH$Population == cpop),]) #for 2-3mb #0.001698198
prop_seg_less5MB
cpop="JDSP 2017"
nrow(subset(allROH, Population==cpop & LENGTH >=5000000))/nrow(allROH[which(allROH$Population == cpop),]) # 0.0009397007
prop_seg_less5MB

library(tidyverse)
less_tidy <-
  pivot_longer(prop_seg_less5MB, c("1-2", "2-3", "3-4", "4-5", "5-30"), names_to = 'LENGTH_bins', values_to = 'Proportions')

pop<-rep(c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017"),each=5)
less_tidy$Pop<-pop

#less_tidy<-read.table("less5MB_proportion_bcftools.txt", header  = T, sep = '\t')

#REORDER
less_tidy$LENGTH_bins <- factor(less_tidy$LENGTH_bins, levels = c("1-2", "2-3", "3-4", "4-5", "5-30"))
less_tidy<- less_tidy[order(less_tidy$LENGTH_bins), ]
less_tidy$Pop <- factor(less_tidy$Pop, levels = c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 1992", "SPSP 2017"))
less_tidy<- less_tidy[order(less_tidy$Pop), ]

#write.table(less_tidy, "less5MB_proportion_bcftools.txt", col.names = T, quote = F, row.names = F, sep = '\t')

library(ggplot2)
q<- ggplot(less_tidy,aes(y = Proportions, x = LENGTH_bins, fill=Pop))+
  geom_bar(alpha=1, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = "")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())
q<-q+ ggtitle("(B) ROH > 1Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
q
q <-q+  scale_fill_manual(values=colorlist)
q

ggsave(q, filename = "PLOTS/prop_long.pdf", height = 8, width = 12, units = "in", dpi=700, device ="pdf")

grid.arrange(p, q, nrow = 1)
## MANUALLY EXPORT LANDSCAPE 7 X 19
## combo_bcftools_ROH_lengths_Jan2023
## EDIT IN ILLUSTRATOR



#########################################################################################################################
###################### GETTING FROH -- PROPORTION ROH/TOTAL GENOME FOR EACH INDIVIDUAL (PER POP) ########################
#########################################################################################################################
all_segments <- read.table("all_segments_bcftools_2022.txt", header=T, as.is=T, sep="\t")
all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
all_segments<- all_segments[order(all_segments$Population), ]

roh.ind <- read.delim("IndROH_bcftools_2022.txt", header = T, as.is=T, sep="\t")

roh.ind$Population <- factor(roh.ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
roh.ind<- roh.ind[order(roh.ind$Population), ]  

pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
contig_lengths<-read.delim("~/Box Sync/PhD/Projects/FSJ_genome_paper/FSJ.chrom.sizes.txt", header = F) #Z = ScYP8k3_10 = 75605511bp
sum(contig_lengths$V2)# = 1060969718
# 1060969718-75605511
# contigs minus Z = 985364207 (in KB = 985364.207)

roh.ind <- roh.ind.raw

roh.ind$FROH <- NA
roh.ind$FROH <- as.numeric(roh.ind$TotLENGTH/985364207)

froh.plot<-ggplot(roh.ind, aes(x=Population, y=as.numeric(FROH), fill=Population))+
  geom_boxplot()+
  theme_minimal()+ ylab("Proportion of the genome in ROH (inbreeding)")+
  theme(legend.position = "")+
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_text(size=20))+
  scale_fill_manual(values=c(colorlist))+
  theme(axis.text.x = element_text(angle=22,size=20, vjust=0.5, hjust=0.5), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+ #trbl
  geom_jitter(position=position_jitter(0))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) + theme(plot.margin = margin(12, 25, 12, 20))+
 scale_x_discrete(labels=c("ONF 1992", "ONF 2017", "ABS 1992", "ABS 2017", "PLE 1992", "PLE 2017", "JDSP 1992", "JDSP 2017", "SPSP 2004", "SPSP 2017"))+ggtitle("FROH per pop, HWE filtered")
froh.plot <- froh.plot + geom_jitter(size=1.5, shape=19, position=position_jitter(0))
froh.plot
#scale_fill_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#CD4262", "#fc88a3", "#e09719", "#f6cd61"))
froh.plot
#ggsave(froh.plot, filename =paste0(OUTPATH, "/", cpop, "_froh_allpops_histogram.png"), height=5, width=7, units ="in", device ="png", dpi=700)

#######################################################################

froh$Population <- factor(froh$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp", "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp"))
froh<- froh[order(froh$Population), ]
froh$FROH<-as.numeric(froh$FROH)

comp<- list(c("JDSPcontemp", "ABScontemp"), c("ABShist", "JDSPhist"), c("PLEcontemp", "JDSPcontemp"), c("PLEhist", "JDSPhist"), c("ABScontemp", "ONFcontemp"), c("ABShist", "ONFhist"))

comp<- list(c("JDSPcontemp", "ABScontemp"), c("PLEhist", "JDSPhist"), c("ABShist", "ONFhist"))

froh.plot<-ggplot(froh, aes(x=Population, y=as.numeric(FROH), fill=Population))+
  geom_boxplot()+
  theme_minimal()+ ylab("FROH")+
  theme(legend.position = "")+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  #stat_compare_means(comparisons = comp, method = "t.test", na.rm = T, label = "p.signif", size = 6)+
  ylim(c(0, 0.43))+
  theme(axis.text.x = element_text(angle=40,size=18, vjust=0.5, hjust=0.5), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  geom_jitter(position=position_jitter(0))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank())

#froh.plot <- froh.plot+  scale_fill_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#f37121", "#e88f58", "#e09719", "#f6cd61", "#CD4262", "#ff7393"))
froh.plot <- froh.plot + scale_fill_manual(values = c("#7CBBDE", "#f37121","#e88f58", "#CD4262"))
froh.plot
ggsave(froh.plot, filename =paste0(OUTPATH, "/_froh_allpops_scatter.pdf"), height=8, width=11, units ="in", device ="pdf", dpi=600)


froh.plot<-ggplot(roh.ind, aes(x=as.numeric(FROH), y=..count.., fill=Population))+
  geom_histogram(bins=8, position="dodge", alpha=1)+
  theme_minimal()+
  xlab("Proportion of genome in ROH (FROH)") + ylab("Count")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  theme(legend.position = c(0.8, 0.8))+
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  #ggtitle("ROH lengths per individual in ABS and PLE")+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))
froh.plot <- froh.plot+  scale_fill_manual(values=colorlist)
froh.plot
#ggsave(froh.plot, filename =paste0(OUTPATH, "/", cpop, "_froh_allpops_histogram.png"), height=5, width=7, units ="in", device ="png", dpi=700)