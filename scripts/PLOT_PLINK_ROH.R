## PLOT ROHS ##
#### DECEMBER 2022

library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)

# same tone
colorlist<-c("#3AB495", "#3AB495", "#268FC8", "#268FC8","#E6A908", "#E6A908", "#C06029", "#C06029", "#CD4262", "#CD4262")
# two tone
colorlist<-c("#0c9c5f", "#61C3AA", "#1b6ca8", "#7CBBDE", "#e09719", "#f6cd61","#f37121", "#e88f58", "#CD4262", "#ff7393")

#########################################################################################################################
################################################### LOAD IN ROH FILES ###################################################
#########################################################################################################################

setwd("~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/ROH")
mydir= "~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/ROH/per_pop" #change to respective path
OUTPATH="~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/ROH/PLOTS"

#################################
######## LOAD .HOM FILES ########
#################################
#get all the names of your files + their path
myfiles = list.files(path=mydir, pattern="*.hom$", full.names=TRUE) # get the full path with files names
mytitles<-basename(myfiles) # get just the file's name
names<-sub("ROH[0-9]_*", "", mytitles) # remove everything but the population and remove underscores
names<-sub("ROH_[A-Z]*_[A-Z]*_*", "", names)## this is for MEYER_CEBALLOS
names<-sub("ROH[A-Z]*[0-9]_*", "", names)## for HET3
names<-sub("ROH_[A-Z]*[0-9]_*", "", names)## for HET3
names<-sub("_HWE_noZ_mac1_genomind15*", "", names)## for HET3
names<-sub("noZ_mac1_genomind15*", "", names)## for HET3
names<-sub("_", "", names)
names<-sub("allInds", "", names)
names<-sub(".hom", "", names) 
names
tables <- lapply(myfiles, read.delim, as.is=T, sep="") # load in all the files as tables in a list
names(tables) <- names # assign respective population names to each list element

#combine all of the dataframes together with each df name as a new column.
all_segments <- bind_rows(tables, .id = "Population")
#write.table(all_segments, paste0(OUTPATH, "/segments.txt"), col.names = T, quote = F, row.names = F, sep = "\t")
#######################################
######## LOAD .HOM.INDIV FILES ########
#######################################
myfiles = list.files(path=mydir, pattern="*.indiv$", full.names=TRUE) 
mytitles<-basename(myfiles)
tables <- lapply(myfiles, read.delim, as.is=T, sep="")
names(tables) <- names

#combine all of the dataframes together with each df name as a new column.
ROH_inds <- bind_rows(tables, .id = "Population")
#write.table(ROH_inds, paste0(OUTPATH, "/roh_inds.txt"), col.names = T, quote = F, row.names = F, sep = "\t")

#########################################################################################################################
################################################### PLOT NUMBER ROHs ####################################################
#########################################################################################################################
all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
x<- split(all_segments, all_segments$Population)
desired_length <- 10
roh.list <- vector(mode = "list", length = desired_length)
str(roh.list)
names<-as.vector(names(x))
names(roh.list) <- names

for (i in 1:10){
  c.df<-data.frame(nrow(x[[i]]))
  roh.list[[i]]<-c.df
}

raw_roh_num<-ldply(roh.list, rbind) # combine all the lists together
colnames(raw_roh_num)<-c("Population", "Roh_num")

raw_roh_num$Population <- factor(raw_roh_num$Population, levels = c("ABShist", "ABScontemp", "ONFhist", "ONFcontemp", "JDSPhist", "JDSPcontemp", "PLEhist", "PLEcontemp", "SPSPhist", "SPSPcontemp"))
raw_roh_num<- raw_roh_num[order(raw_roh_num$Population), ]
#raw_roh_num$Population<-c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017")

raw_roh_count<-ggplot(raw_roh_num, aes(x=Population, y=Roh_num, fill=Population))+
  geom_bar(stat = "identity", position="dodge", alpha=1)+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("") + ylab("Total number of ROHs")+
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
raw_roh_count <- raw_roh_count+ scale_fill_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#f37121", "#e88f58", "#e09719", "#f6cd61", "#CD4262", "#ff7393"))
#scale_fill_manual(values=c("#268FC8", "#268FC8", "#3AB495", "#3AB495", "#CD4262", "#CD4262", "#E6A908", "#E6A908"))
raw_roh_count
#ggsave(raw_roh_count, filename = paste0(OUTPATH, "/all_ROH_counts.png"), height =8, width=11, units = "in", device = "png", dpi=700)

summary(raw_roh_num$Roh_num)

# HET3 ROH
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2886    5666    7334    8444   10417   19913

#######################################
######## SUMMARY STATS OF ROHS ########
#######################################
c.df<-data.frame(x[["ABScontemp"]])
  summary(c.df$KB)
  summary(c.df$NSNP)
  
c.df<-data.frame(x[["ABShist"]])
  summary(c.df$KB)
  summary(c.df$NSNP)
  
c.df<-data.frame(x[["ONFcontemp"]])
  summary(c.df$KB)
  summary(c.df$NSNP)
  
c.df<-data.frame(x[["ONFhist"]])
  summary(c.df$KB)
  summary(c.df$NSNP)

c.df<-data.frame(x[["JDSPcontemp"]])
  summary(c.df$KB)
  summary(c.df$NSNP)
  
c.df<-data.frame(x[["JDSPhist"]])
  summary(c.df$KB)
  summary(c.df$NSNP)
  
c.df<-data.frame(x[["PLEcontemp"]])
  summary(c.df$KB)
  summary(c.df$NSNP)

c.df<-data.frame(x[["PLEhist"]])
  summary(c.df$KB)
  summary(c.df$NSNP)

  
  #########################################################################################################################   
  ############################################# ROHS ALONG WHICH CHROMOSOMES  #############################################
  #########################################################################################################################
  all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
  ROHcounts<-data.frame(table(all_segments$CHR))
  
  ROHcounts$Var1<-gsub("ScYP8k3_13\\b", "1", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_10\\b", "Z", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_629\\b", "2", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_866\\b", "3", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_11\\b", "4", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_5\\b", "5", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_9\\b", "6", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_4\\b", "7", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_1\\b", "8", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_3\\b", "9", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_2\\b", "10", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_869\\b", "11", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_870\\b", "12", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_302\\b", "13", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_651\\b", "14", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_864\\b", "15", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_7\\b", "17", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_8\\b", "18", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_6\\b", "19", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_865\\b", "20", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_15\\b", "10S", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_12\\b", "1A", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_14\\b", "4A", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_69\\b", "M", ROHcounts$Var1)
  ROHcounts$Var1<-gsub("ScYP8k3_[0-9]+\\b", "0", ROHcounts$Var1)
  
  chr_roh_count<-ggplot(ROHcounts, aes(x=Var1, y=Freq, fill="Var1"))+
    geom_bar(stat = "identity", position="dodge", alpha=1, fill="steelblue")+
    theme_minimal()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color="white"),
          panel.grid.minor = element_line(color="white"),
          panel.border = element_blank(),
          panel.background = element_blank())+
    xlab("") + ylab("Total number of ROHs")+
    theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
    theme(legend.text = element_text(size=18))+
    theme(plot.title = element_text(hjust=0.5, size=24))+
    theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
    theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
    theme(axis.title.y = element_text(vjust=4))+
    theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
    theme(plot.title=element_text(hjust=0.5, vjust=2.5))
  chr_roh_count
  ggsave(chr_roh_count, filename = paste0(OUTPATH, "/chr_roh_count.png"), height =7, width=12, units = "in", device = "png", dpi=700)

  ######################################################################################################################### 
  ##################################### PROPORTION ROH SIZE ALONG WHICH CHROMOSOMES  ######################################
  #########################################################################################################################
  all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
  ROH_inds <- read.delim(file = paste0(OUTPATH, "/ROH_inds.txt"), header = T,  as.is = T, sep = "\t")
  scaf_lengths<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/scaffold_lengths.txt", as.is = T, header=F)
  colnames(scaf_lengths)<-c("Scaffold", "Length")
  
  x<- split(all_segments, all_segments$CHR)
  desired_length <- 24
  chr.list <- vector(mode = "list", length = desired_length)
  str(chr.list)
  names<-as.vector(names(x))
  names(chr.list) <- names
  inds_names<-unique(all_segments$IID)
  
  ROH_scaffolds<-unique(all_segments$CHR)
  
  for (i in 1:24){
    c.df<-data.frame(x[[i]])
    for (j in 1:218){
      ind<-inds_names[j]
      cur.chr<-subset(c.df, c.df$IID == ind)
      m<-mean(cur.chr$KB)
      all.df[j,]<-data.frame(cbind(chr,m))
    }
    chr.list[[i]]<-all.df
  }
  
  
  for (i in 1:24){
    c.df<-chr.list[[i]][["m"]]
    c.df<-as.numeric(c.df)
    c.df<-data.frame(mean(c.df, na.rm = T))
    chr.list[[i]]<-c.df
}
  

  mean_roh_chr<-ldply(chr.list, rbind) # combine all the lists together
  colnames(mean_roh_chr)<-c("Scaffold", "Mean")
  mean_roh_chr<-merge(mean_roh_chr, scaf_lengths, by="Scaffold")
  
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_13\\b", "1", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_10\\b", "Z", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_629\\b", "2", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_866\\b", "3", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_11\\b", "4", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_5\\b", "5", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_9\\b", "6", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_4\\b", "7", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_1\\b", "8", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_3\\b", "9", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_2\\b", "10", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_869\\b", "11", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_870\\b", "12", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_302\\b", "13", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_651\\b", "14", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_864\\b", "15", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_7\\b", "17", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_8\\b", "18", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_6\\b", "19", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_865\\b", "20", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_15\\b", "10S", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_12\\b", "1A", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_14\\b", "4A", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_69\\b", "M", mean_roh_chr$Scaffold)
  mean_roh_chr$Scaffold<-gsub("ScYP8k3_[0-9]+\\b", "0", mean_roh_chr$Scaffold)
  
  mean_roh_chr<-mean_roh_chr %>% mutate(Prop= (Mean/Length)*100)
  mean_roh_chr<-mean_roh_chr[mean_roh_chr$Scaffold != 0, ]
  
  avg_ROH_chr<-ggplot(mean_roh_chr, aes(x=Scaffold, y=Prop, fill=Scaffold))+
    geom_bar(stat = "identity", position="dodge", alpha=1, fill="steelblue")+
    theme_minimal()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color="white"),
          panel.grid.minor = element_line(color="white"),
          panel.border = element_blank(),
          panel.background = element_blank())+
    xlab("") + ylab("Percent of chromosome in ROH (averaged)")+
    theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
    theme(legend.position = "")+
    theme(legend.text = element_text(size=18))+
    theme(plot.title = element_text(hjust=0.5, size=24))+
    theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
    theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
    theme(axis.title.y = element_text(vjust=4))+
    theme(plot.margin = margin(15, 22, 15, 22))
  avg_ROH_chr
  ggsave(avg_ROH_chr, filename = paste0(OUTPATH, "/perc_roh_chr.png"), height =7, width=11, units = "in", device = "png", dpi=700)
  
  
  
  #########################################################################################################################
  ######################################## SUBSET ALL POPS TO JDSP 1992 SAMPLE SIZE #######################################
  #########################################################################################################################
  
  setwd("~/Box Sync/PhD/Projects/PLE_vs_ABS/Input/No_MAF_filtering/ROH")
  
  ABS_filt1<-read.delim("outputs/ABS_BothYr_NoLDprune_HWE0.001_ROH_filt1.hom.indiv", as.is=T, header = T, sep = "")
  sum(ABS_filt1$NSEG) #372 = Number ROHs across all individuals
  PLE_filt1<-read.delim("outputs/PLE_BothYr_NoLDprune_HWE0.001_ROH_filt1.hom.indiv", as.is=T, header = T, sep = "")
  sum(PLE_filt1$NSEG) #134
  
  
  
  # The number of ROHs went down in ABS with a smaller minimum ROH length (means that some segments in ABS were smaller than 200kb ultimately) all segments in PLE must be at least 500kb then.
  
  set.seed(100)
  ## SUBSAMPLING ABS ROH 1000 TIMES
  ROHlist = list()
  
  for (i in 1:1000) {
    # ... make some data
    dat <- ABS_filt1[sample(nrow(ABS_filt1), size = 62, replace = FALSE),] #subsample 62 individuals to match PLE
    dat<- data.frame(sum(dat$NSEG)) #count how many NSEGS you picked up
    dat$i <- i  # maybe you want to keep track of which iteration produced it?
    ROHlist[[i]] <- dat # add it to your list
  }
  
  allROH = do.call(rbind, ROHlist)
  
  library(ggplot2)
  
  p<-ggplot(allROH, aes(x=allROH$sum.dat.NSEG.))+
    geom_histogram(bins=40, fill="skyblue2")+
    theme_minimal()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color="white"),
          panel.grid.minor = element_line(color="white"),
          panel.border = element_blank(),
          panel.background = element_blank())+
    xlab("Number of ROHs (Filter1)") + ylab("Count")+
    theme(legend.position = "", plot.title = element_text(hjust=0.5, size=24))+
    theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
    theme(axis.text.x = element_text(size=14), axis.text.y=element_text(size = 14))+
    theme(axis.title.y = element_text(vjust=4))+
    theme(plot.margin = margin(15, 40, 15, 22))+
    ggtitle("Subsample of 62 ABS individuals\n across both years (1000x)")+
    theme(plot.title=element_text(hjust=0.5, vjust=2.5))
  p
  
  p<-p+geom_vline(xintercept = 106.708, linetype="solid", colour = "blue", size=0.7 ) #add line for real ABS mean
  p<- p+ annotate("text", x  = 105.708, y = 80, angle=90, label = "mean number ROH (107)") #actual ABS mean
  p
  p<-p+geom_vline(xintercept = 134, linetype="solid", colour = "red", size=0.7 ) #actual PLE number ROH
  p<- p+ annotate("text", x  = 133, y = 75, angle=90, label = "number ROH for PLE (134)")
  p
  
  ggsave(p, file="subsample_ABS_ROH_bothYr_Filt1.png", height = 5, width = 7, units = "in", dpi=600)
  
  
  mean(allROH$sum.dat.NSEG.) #106.708
  summary(allROH$sum.dat.NSEG.)

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
79.0   100.0   107.0   106.7   113.0   141.0 

  
  #########################################################################################################################
  ############################################ SCATTERPLOT KB VS NSNP IN EACH ROH #########################################
  #########################################################################################################################
  #all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
  all_segments$Population <- factor(all_segments$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
  all_segments<- all_segments[order(all_segments$Population), ]
  
  plot(all_segments$KB, all_segments$NSNP)

segments_plot<-ggplot(all_segments, aes(x=KB, y=NSNP, color=Population))+
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
segments_plot
#ggsave(segments_plot, filename = paste0(OUTPATH, "/ROH_KB_NSNP.png"), height =8, width=9, units = "in", device = "png", dpi=700)

summary(all_segments$KB)
# MC
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#100.0   123.9   161.8   214.7   240.2  3366.5 

# HET3
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#100.0   125.4   169.2   243.7   266.8  9145.9 

summary(all_segments$NSNP)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#100     898    1302    1715    1999   40166 

############################################ SCATTERPLOT KB VS NSNP IN EACH ROH (TOGETHER) #########################################
allpop_seg <- read.delim("../ROH_HET3_allPops_breeders.hom", sep="", header=T)

allpop_seg$Population<-NA

for (i in 1:nrow(allpop_seg)) {
  if(allpop_seg$IID[i] %in% ABS_hist$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "ABS1990"
  }else if (allpop_seg$IID[i] %in% ABS_now$Novogene.ID == TRUE) {
    allpop_seg$Population[i] <- "ABS2017"
  } else if (allpop_seg$IID[i] %in% PLE_hist$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "PLE1990"
  } else if (allpop_seg$IID[i] %in% PLE_now$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "PLE2017"
  } else if (allpop_seg$IID[i] %in% JDSP_hist$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "JDSP1990"
  } else if (allpop_seg$IID[i] %in% JDSP_now$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "JDSP2017"
  } else if (allpop_seg$IID[i] %in% ONF_hist$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "ONF1990"
  } else if (allpop_seg$IID[i] %in% ONF_now$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "ONF2017"
  }  else if (allpop_seg$IID[i] %in% SPSP_hist$Novogene.ID == TRUE){
    allpop_seg$Population[i] <- "SPSP2004"
  } else {
    allpop_seg$Population[i] <- "SPSP2017"
  }
}


allpop_seg$Population <- factor(allpop_seg$Population, levels = c("ONF1990", "ONF2017","ABS1990", "ABS2017",  "PLE1990", "PLE2017", "JDSP1990", "JDSP2017", "SPSP2004", "SPSP2017"))
allpop_seg<- allpop_seg[order(allpop_seg$Population), ]

plot(allpop_seg$KB, allpop_seg$NSNP)

segments_plot<-ggplot(allpop_seg, aes(x=KB, y=NSNP, color=Population))+
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
segments_plot



#########################################################################################################################
###################################### PLOT HISTOGRAMS OF ROH LENGTHS BY POPULATION  ####################################
#########################################################################################################################
all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)

#combine the populations sampling periods together
ABS<-subset(all_segments, all_segments$Population == "ABScontemp" | all_segments$Population == "ABShist")
ONF<-subset(all_segments, all_segments$Population == "ONFcontemp" | all_segments$Population == "ONFhist")
JDSP<-subset(all_segments, all_segments$Population == "JDSPcontemp" | all_segments$Population == "JDSPhist")
PLE<-subset(all_segments, all_segments$Population == "PLEcontemp" | all_segments$Population == "PLEhist")
SAV<-subset(all_segments, all_segments$Population == "SPSPcontemp" | all_segments$Population == "SPSPhist")
pop_roh.list<-list(ABS, ONF, JDSP, PLE, SAV)
names <- c("ABS", "ONF", "JDSP", "PLE", "SAV")
names(pop_roh.list)<-names


desired_length <- 5
roh.breakdown <- vector(mode = "list", length = desired_length)
str(roh.breakdown)
names(roh.breakdown) <- names

for (cpop in 1:5){ ### NOTE: IF YOU WANT DIFF COLORS FOR EACH POP - CHANGE CPOP AND EXECUTE EACH LINE MANUALLY OUT OF LOOP
  c.df<-pop_roh.list[[cpop]]
  short<-subset(c.df, c.df$KB <= 500) #500kb
  short$table<-("Short < 0.5 Mb")
  long<-subset(c.df, c.df$KB > 500 & c.df$KB <= 1000) #500kb - 1Mb
  long$table<-("Medium 0.5-1 Mb")
  XL<-subset(c.df, c.df$KB > 1000) #> 1Mb
  XL$table<-("Long > 1 Mb")
  all_lengths<-rbind(short, long, XL)
  
  lengths.plot<-ggplot(all_lengths, aes(x=KB, y=..count.., fill=Population))+
    geom_histogram(binwidth=250, position="dodge", alpha=1)+
    xlab("Lengths of ROH (kb)") + ylab("Count")+
    theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
    theme(legend.position = c(0.8, 0.95))+
    theme(legend.text = element_text(size=18))+
    theme(plot.title = element_text(hjust=0.5, size=24))+
    theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
    theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
    theme(axis.title.y = element_text(vjust=4))+
    theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
    theme(plot.title=element_text(hjust=0.5, vjust=2.5))
  lengths.plot <- lengths.plot+  scale_fill_manual(values=c("#E6A908", "grey60"))
  lengths.plot <- lengths.plot + facet_grid(rows = vars(table), scales = "free") 
  lengths.plot <-lengths.plot + theme(strip.text.y = element_text(size = 14))
  lengths.plot
 # ggsave(lengths.plot, filename =paste0(OUTPATH, "/", cpop, "_lengths_histogram.png"), height=8, width=8, units ="in", device ="png", dpi=700)
  
  c.list<-list(short, long, XL, lengths.plot)
  names(c.list)<-c("short", "long", "XL", "plot")
  roh.breakdown[[cpop]]<-c.list
}

#"#268FC8", "#3AB495", "#CD4262", "#E6A908"

#########################################################################################################################
########################################### PLOT HISTOGRAMS OF ALL ROH LENGTHS  #########################################
#########################################################################################################################



#########################################################################################################################
############################### PLOT **PROPORTION** OF ROH LENGTHS (SIMILAR TO PLE PAPER)  ##############################
#########################################################################################################################
roh_inds<-read.delim(file = paste0(OUTPATH, "/roh_inds.txt"), header = T,  as.is = T, sep = "\t")
all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)

# count how many individuals has zero ROHs
# sum(roh_inds$NSEG ==0)


#######################################
########### SHORT RUNS< 1 MB ##########
#######################################
#split segments by population
pop_segment.list<- split(all_segments, all_segments$Population)

# set up empty list
x<- split(ROH_inds, ROH_inds$Population)
desired_length <- 10
prop.lengths.list <- vector(mode = "list", length = desired_length)
str(prop.lengths.list)
names<-as.vector(names(x))
names(prop.lengths.list) <- names

for (cpop in 1:10){ 
  c.df<-pop_segment.list[[cpop]]
  totalseg<-nrow(c.df)
  A<-data.frame((sum(c.df$KB <= 300))/totalseg) #0-0.3MB
  A$table<-("<0.3 Mb")
  colnames(A)<-c("frac", "table")
  B<-data.frame((sum(c.df$KB  >300 & c.df$KB <= 600))/totalseg) #0.3-0.6MB
  B$table<-("0.3-0.6 Mb")
  colnames(B)<-c("frac", "table")
  C<-data.frame((sum(c.df$KB  >600 & c.df$KB <= 1000.00))/totalseg) #0.6-1MB
  C$table<-("0.6-1.0 Mb")
  colnames(C)<-c("frac", "table")
  prop.lengths.list[[cpop]]<-data.frame(rbind(A, B, C))
}

all.prop.lengths<-ldply(prop.lengths.list, rbind) # combine all the lists together
colnames(all.prop.lengths)<-c("Population", "Frac", "Bins")

all.prop.lengths$Population <- factor(all.prop.lengths$Population, levels = c("ONFhist", "ONFcontemp", "ABShist", "ABScontemp", "JDSPhist", "JDSPcontemp", "PLEhist", "PLEcontemp", "SPSPhist", "SPSPcontemp"))
all.prop.lengths<- all.prop.lengths[order(all.prop.lengths$Population), ]

prop_short_ROH<-ggplot(all.prop.lengths, aes(y = Frac, x = Bins, fill=Population, color=Population))+
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
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c(colorlist)) +
 scale_color_manual(values=c(colorlist))
prop_short_ROH
prop_short_ROH<-prop_short_ROH+ ggtitle("Short ROHs <1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_short_ROH
#ggsave(prop_short_ROH, filename =paste0(OUTPATH, "/prop_short_ROH.pdf"), height =6.5, width=9.5, units = "in", device = "pdf", dpi=700)

prop_short_ROH_JDSP<-all.prop.lengths[which(all.prop.lengths$Population == "JDSPhist" | all.prop.lengths$Population == "JDSPcontemp"),]

prop_short_ROH_JDSP_plot<-ggplot(prop_short_ROH_JDSP, aes(y = Frac, x = Bins, fill=Population, color=Population))+
  geom_bar(alpha=0.95, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  scale_y_continuous(labels = function(x) paste0(x, "00"))+
  #ylim(c(0, 0.025))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c("#CD4262", "#f598b2")) +
  scale_color_manual(values=c("#CD4262", "#f598b2"))
prop_short_ROH_JDSP_plot
prop_short_ROH_JDSP_plot<-prop_short_ROH_JDSP_plot+ ggtitle("Short ROHs <1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_short_ROH_JDSP_plot
#ggsave(prop_short_ROH_JDSP_plot, filename =paste0(OUTPATH, "/prop_short_ROH_JDSP.png"), height =6, width=8, units = "in", device = "png", dpi=700)


prop_short_ROH_ABS<-all.prop.lengths[which(all.prop.lengths$Population == "ABShist" | all.prop.lengths$Population == "ABScontemp"),]

prop_short_ROH_ABS_plot<-ggplot(prop_short_ROH_ABS, aes(y = Frac, x = Bins, fill=Population, color=Population))+
  geom_bar(alpha=0.95, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  scale_y_continuous(labels = function(x) paste0(x, "00"))+
 # ylim(c(0, 0.025))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c("#1b6ca8", "#7CBBDE")) +
  scale_color_manual(values=c("#1b6ca8", "#7CBBDE"))
prop_short_ROH_ABS_plot
prop_short_ROH_ABS_plot<-prop_short_ROH_ABS_plot+ ggtitle("Short ROHs <1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_short_ROH_ABS_plot
#ggsave(prop_short_ROH_ABS_plot, filename =paste0(OUTPATH, "/prop_short_ROH_ABS.png"), height =6, width=8, units = "in", device = "png", dpi=700)




#######################################
########### LONG RUNS > 1 MB ##########
#######################################

desired_length <- 10
prop.lengths.list <- vector(mode = "list", length = desired_length)
str(prop.lengths.list)
names<-as.vector(names(x))
names(prop.lengths.list) <- names

for (cpop in 1:10){ 
  c.df<-pop_segment.list[[cpop]]
  totalseg<-nrow(c.df)
  A<-data.frame((sum(c.df$KB >1000.00 & c.df$KB <= 2000.00))/totalseg) #1-2MB
  A$table<-("1-2 Mb")
  colnames(A)<-c("frac", "table")
  B<-data.frame((sum(c.df$KB >2000.00 & c.df$KB <= 3000.00))/totalseg) #2-3MB
  B$table<-("2-3 Mb")
  colnames(B)<-c("frac", "table")
  C<-data.frame((sum(c.df$KB >3000.00))/totalseg) #>3MB
  C$table<-("3-10 Mb")
  colnames(C)<-c("frac", "table")
  prop.lengths.list[[cpop]]<-data.frame(rbind(A, B, C))
 }

all.prop.lengths<-ldply(prop.lengths.list, rbind) # combine all the lists together
colnames(all.prop.lengths)<-c("Population", "Frac", "Bins")

all.prop.lengths$Population <- factor(all.prop.lengths$Population, levels = c("ONFhist", "ONFcontemp", "ABShist", "ABScontemp", "JDSPhist", "JDSPcontemp", "PLEhist", "PLEcontemp", "SPSPhist", "SPSPcontemp"))
all.prop.lengths<- all.prop.lengths[order(all.prop.lengths$Population), ]

prop_long_ROH<-ggplot(all.prop.lengths, aes(y = Frac, x = Bins, fill=Population, color=Population))+
  geom_bar(alpha=0.95, stat="identity", position = "dodge")+
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
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c(colorlist)) +
  scale_color_manual(values=c(colorlist))
prop_long_ROH
prop_long_ROH<-prop_long_ROH+ ggtitle("Long ROHs >1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_long_ROH
#ggsave(prop_long_ROH, filename =paste0(OUTPATH, "/prop_long_ROH.pdf"), height =6.5, width=9.5, units = "in", device = "pdf", dpi=700)


prop_long_ROH_JDSP<-all.prop.lengths[which(all.prop.lengths$Population == "JDSPhist" | all.prop.lengths$Population == "JDSPcontemp"),]

prop_long_ROH_JDSP_plot<-ggplot(prop_long_ROH_JDSP, aes(y = Frac, x = Bins, fill=Population, color=Population))+
  geom_bar(alpha=0.95, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  ylim(c(0, 0.025))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c("#CD4262", "#f598b2")) +
  scale_color_manual(values=c("#CD4262", "#f598b2"))
prop_long_ROH_JDSP_plot
prop_long_ROH_JDSP_plot<-prop_long_ROH_JDSP_plot+ ggtitle("Long ROHs >1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_long_ROH_JDSP_plot
#ggsave(prop_long_ROH_JDSP_plot, filename =paste0(OUTPATH, "/prop_long_ROH_JDSP.png"), height =6, width=8, units = "in", device = "png", dpi=700)


prop_long_ROH_ABS<-all.prop.lengths[which(all.prop.lengths$Population == "ABShist" | all.prop.lengths$Population == "ABScontemp"),]

prop_long_ROH_ABS_plot<-ggplot(prop_long_ROH_ABS, aes(y = Frac, x = Bins, fill=Population, color=Population))+
  geom_bar(alpha=0.95, stat="identity", position = "dodge")+
  ylab("Proportion of ROH segments per population")+ xlab("ROH lengths (≥ megabases)")+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+theme_bw() +
  ylim(c(0, 0.025))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=20, vjust=-2))+
  theme(axis.text.x = element_text(size=18), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(size=20, vjust=4))+
  theme(plot.margin = margin(12, 6, 20, 22)) + #TRBL
  theme(legend.position = c(0.8, 0.8))+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background=element_blank())+  scale_fill_manual(values=c("#1b6ca8", "#7CBBDE")) +
  scale_color_manual(values=c("#1b6ca8", "#7CBBDE"))
prop_long_ROH_ABS_plot
prop_long_ROH_ABS_plot<-prop_long_ROH_ABS_plot+ ggtitle("Long ROHs >1 Mb") +theme(plot.title = element_text(hjust=0, size=24, face = "bold"))
prop_long_ROH_ABS_plot
#ggsave(prop_long_ROH_ABS_plot, filename =paste0(OUTPATH, "/prop_long_ROH_ABS.png"), height =6, width=8, units = "in", device = "png", dpi=700)



#########################################################################################################################
###################### GETTING FROH -- PROPORTION ROH/TOTAL GENOME FOR EACH INDIVIDUAL (PER POP) ########################
#########################################################################################################################
 all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
 ROH_inds<-read.delim(file = paste0(OUTPATH, "/roh_inds.txt"), header = T,  as.is = T, sep = "\t")
 pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
 contig_lengths<-read.delim("~/Box Sync/PhD/Projects/FSJ_genome_paper/FSJ.chrom.sizes.txt", header = F) #Z = ScYP8k3_10 = 75605511bp
# sum(contig_lengths$V2) = 1060969718
# 1060969718-75605511
# contigs minus Z = 985364207 (in KB = 985364.207)
# 
 x<- split(ROH_inds, ROH_inds$Population)
 desired_length <- 10
 roh.list <- vector(mode = "list", length = desired_length)
 str(roh.list)
 names<-as.vector(names(x))
 names(roh.list) <- names

 x<- split(ROH_inds, ROH_inds$Population)
 froh<-data.frame()
 
 for (i in 1:10){
   for (ind in 1:nrow(x[[i]])){
     sample<-x[[i]][["IID"]][ind]
     Population<-names(x[i])
     FROH<-x[[i]][["KB"]][ind]/985364.207
     combo<-cbind(sample, Population, FROH)
     froh<-rbind(froh, combo)
   }
 }

 #merge FROH with ROH_inds
 colnames(froh)<-c("IID", "population", "FROH")
 froh$population<-NULL
 df<-merge(ROH_inds, froh, by="IID")

#write.table(df, paste0(OUTPATH,"/FULL_ROHind_FROH.txt"), quote = F, col.names = T, sep = "\t", row.names = F)


### USING ONLY LOW MISSINGNESS INDIVIDUALS ###
#froh<-read.table(paste0(OUTPATH,"/FULL_ROHind_FROH.txt"), header = T, sep = "\t", as.is=T)
 froh <- df
low.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/WGS_Read_mapping/scripts/FSJv2/2022-July/breeders_noMiss_noDups.txt", sep="\t", header = F, as.is=T)

froh<-froh[which(froh$IID %in% low.miss.ind$V1),]

# reorder populations
froh$Population <- factor(froh$Population, levels = c("ONFhist", "ONFcontemp", "ABShist", "ABScontemp","PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
froh<- froh[order(froh$Population), ]
froh$FROH<-as.numeric(froh$FROH)

# same tone
colorlist<-c("#3AB495", "#3AB495", "#268FC8", "#268FC8","#E6A908", "#E6A908", "#C06029", "#C06029", "#CD4262", "#CD4262")

froh.plot<-ggplot(froh, aes(x=Population, y=as.numeric(FROH), fill=Population))+
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


############################################ FROH BOXPLOTS (TOGETHER) #########################################

OUTPATH="/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/ROH"
all_segments<-read.delim(file = paste0(OUTPATH, "/ROH_HET3_allPops_breeders.hom"), header = T,  as.is = T, sep = "")
roh_inds<-read.delim(file = paste0(OUTPATH, "/ROH_HET3_allPops_breeders.hom.indiv"), header = T,  as.is = T, sep = "")
pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
all.inds<-read.delim("~/Box Sync/PhD/Projects/Statewide WGS/Novogene/SAMPLE INFO/All_Novogene_individs_WGS20x_August2022.txt", as.is=T, header = T)
contig_lengths<-read.delim("~/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/scaffold_lengths.txt", header = F) #Z = ScYP8k3_10 = 75605511bp
# sum(contig_lengths$V2) = 1060970510
# contigs minus Z = 985364999 (in KB = 985364.999)

FROH<- roh_inds %>% mutate(FROH = KB/985364.999)

colnames(all.inds)[1] <- c("IID")

c.df<-merge(FROH, all.inds)

c.df <- c.df %>% select("IID", "FROH", "Category")


#reorder our populations to plot them 
c.df$Category <- factor(c.df$Category , levels = c("ONF Historic", "ONF Contemporary", "ABS Historic", "ABS Contemporary", "PLE Historic", "PLE Contemporary", "JDSP Historic", "JDSP Contemporary", "SPSP Historic", "SPSP Contemporary"))
c.df<- c.df[order(c.df$Category ), ]

comp <- list(c("ONF Historic", "ONF Contemporary"), c("ABS Historic", "ABS Contemporary"), c("PLE Historic", "PLE Contemporary"), c("JDSP Historic", "JDSP Contemporary"), c("SPSP Historic", "SPSP Contemporary")) # within populations, across years

comp<- list(c("JDSP Contemporary", "ABS Contemporary"), c("ABS Historic", "JDSP Historic"), c("PLE Contemporary", "JDSP Contemporary"), c("PLE Historic", "JDSP Historic"), c("ABS Contemporary", "ONF Contemporary"), c("ABS Historic", "ONF Historic"))

froh.plot<-ggplot(c.df, aes(x=Category, y=as.numeric(FROH), fill=Category))+
  geom_boxplot()+
  theme_minimal()+ ylab("Proportion of the genome in ROH (inbreeding)")+
  theme(legend.position = "")+
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_text(size=20))+
  scale_fill_manual(values=c(colorlist))+
  stat_compare_means(comparisons = comp, method = "wilcox.test" , label = "p.signif", size = 6)+
  theme(axis.text.x = element_text(angle=22,size=20, vjust=0.5, hjust=0.5), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+ #trbl
  geom_jitter(position=position_jitter(0))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) + theme(plot.margin = margin(12, 25, 12, 20))
  #scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP 2017"))+
froh.plot <- froh.plot + geom_jitter(size=1.5, shape=19, position=position_jitter(0))
froh.plot
ggsave(froh.plot, file="PLOTS/froh_noStats.pdf", height = 8, width = 14, units = "in", device="pdf", dpi=800)


c.df$bothYr<-NA
c.df$bothYr[which(c.df$Category == "ABS Historic" | c.df$Category == "ABS Contemporary")]<-"bothABS"
c.df$bothYr[which(c.df$Category == "PLE Historic" | c.df$Category == "PLE Contemporary")]<-"bothPLE"
c.df$bothYr[which(c.df$Category == "JDSP Historic" | c.df$Category == "JDSP Contemporary")]<-"bothJDSP"
c.df$bothYr[which(c.df$Category == "ONF Historic" | c.df$Category == "ONF Contemporary")]<-"bothONF"
c.df$bothYr[which(c.df$Category == "SPSP Historic" | c.df$Category == "SPSP Contemporary")]<-"bothSPSP"

pwc <- c.df %>%
  pairwise_wilcox_test(FROH ~ bothYr, p.adjust.method = "bonferroni")
pwc



x<- split(roh_inds, roh_inds$Population)
desired_length <- 10
roh.list <- vector(mode = "list", length = desired_length)
str(roh.list)
names<-as.vector(names(x))
names(roh.list) <- names


x<- split(roh_inds, roh_inds$Population)
froh<-data.frame()

for (i in 1:10){
  for (ind in 1:nrow(x[[i]])){
  sample<-x[[i]][["IID"]][ind]
  Population<-names(x[i])
  FROH<-x[[i]][["KB"]][ind]/985364.999
  combo<-cbind(sample, Population, FROH)
  froh<-rbind(froh, combo)
  }
}


froh<- froh[order(froh$Population), ]
froh$FROH<-as.numeric(froh$FROH)

write.table(froh, paste0(mydir,"/froh_coeff.txt"), quote = F, col.names = T, sep = "\t", row.names = F)

### USING ONLY LOW MISSINGNESS INDIVIDUALS ###
froh<-read.table(paste0(mydir,"/froh_coeff.txt"), header = T, sep = "\t", as.is=T)
high.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/WGS_Read_mapping/scripts/FSJv2/2022-July/20perc_missingInd_July2022.txt", sep="\t", header = F, as.is=T)

#colorlist<-c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#CD4262", "#f598b2", "#e09719", "#f6cd61")

#froh<-froh[!froh$sample %in% high.miss.ind$V1,] #remove highly missing individuals
#should already be done in the VCF

froh.plot<-ggplot(froh, aes(x=as.numeric(FROH), y=..count.., fill=Population))+
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

#######################################################################

froh$Population <- factor(froh$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp", "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
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
froh.plot <- froh.plot + scale_fill_manual(values = colorlist)
#c("#3AB495", "#3AB495","#268FC8", "#268FC8", "#E6A908", "#E6A908", "#CD4262", "#CD4262")
froh.plot
ggsave(froh.plot, filename =paste0(OUTPATH, "/_froh_allpops_scatter.pdf"), height=8, width=11, units ="in", device ="pdf", dpi=600)

#########################################################################################################################   
############################# SCATTERPLOT AVG ROH LENGTH VS # ROHs FOR EACH INDIVIDUAL  #################################
#########################################################################################################################

# two tone
colorlist<-c("#0c9c5f", "#61C3AA", "#1b6ca8", "#7CBBDE", "#e09719", "#f6cd61","#f37121", "#e88f58", "#CD4262", "#ff7393")

#roh_inds<-read.delim(file = paste0(OUTPATH, "/roh_inds.txt"), header = T,  as.is = T, sep = "\t")
#pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
#low.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/low.miss.ind.txt", sep="\t", header = T, as.is=T)


## MCLUST 
#library(mclust)
#library(factoextra)
#Class <- factor(roh_inds$Population)
#X <- data.matrix(roh_inds[,c(5,7)])
#X  <- X [ , c("KBAVG", "NSEG")]

#fviz_mclust_bic(Mclust(X))
#mod <- Mclust(X)
#summary(mod$BIC)
#table(Class, mod$classification)
#roh_inds$cluster<-mod$classification


### change data to make color=populations, and cluster=shapes
#vec<-paste(roh_inds$Population,roh_inds$cluster)
#roh_inds$classifier<-vec

#allroh<-roh_inds %>% select(IID, NSEG, KBAVG, Population, cluster, classifier)
#allroh<-allroh[which(allroh$IID %in% low.miss.ind$x),]
#allroh<-roh_inds[which(roh_inds$IID %in% low.miss.ind$x),]

#allroh$Population <- factor(allroh$Population, levels = c("ONFhist", "ONFcontemp", "ABShist", "ABScontemp","JDSPhist", "JDSPcontemp", "PLEhist", "PLEcontemp"))
#allroh<- allroh[order(allroh$Population), ]

#colorlist<-c("#077848", "#61C3AA","#1B6CA8", "#7CBBDE", "#B82849", "#FF7393", "#d98c07", "#F6CD61") #JDSP as pink

# add Column for age
contemp<-grep("contemp", x = froh$Population)
hist<-grep("hist", x = froh$Population)
froh$Age[contemp] <- "Contemporary"
froh$Age[hist] <- "Historic"

froh$Age <- as.factor(froh$Age) 

### CIRCLES FOR CONTEMP, TRIANGLES FOR HIST
ind_roh_scatter<-ggplot(froh, aes(x=KBAVG, y=NSEG, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
#ggsave(ind_roh_scatter, filename =paste0(OUTPATH, "/ind_roh_scatter.pdf"), height=5.5, width=9, units ="in", device ="pdf", dpi=700)

ind_roh_scatter<-ggplot(froh, aes(x=KB, y=NSEG, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Sum of ROH lengths (KB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter
#ggsave(ind_roh_scatter, filename =paste0(OUTPATH, "/sum_roh_lengths_scatter.pdf"), height=5.5, width=10, units ="in", device ="pdf", dpi=700)



### SHAPE = MCLUSTERS
ind_roh_scatter<-ggplot(allroh, aes(x=KBAVG, y=NSEG))+
  geom_point(aes(color=Population, shape=(as.factor(allroh$cluster))), size=5)+
  scale_shape_manual(values=c(15,16,17,3,18,1,25))+
  scale_color_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#f37121", "#e88f58", "#e09719", "#f6cd61", "#CD4262", "#ff7393"))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl

ind_roh_scatter


ind_roh_scatter<-ind_roh_scatter + scale_shape_manual(values=c(15,16,17,3,18,1,25))
ind_roh_scatter


ggsave(ind_roh_scatter, filename =paste0(OUTPATH, "/ind_roh_scatter.png"), height=5, width=9, units ="in", device ="png", dpi=700)


############################################ SCATTERPLOT AVGKB VS NSNP PER INDV (TOGETHER) #########################################
allpop_ind <- read.delim("../ROH_HET3_allPops_breeders.hom.indiv", sep="", header=T)

allpop_ind$Population<-NA

for (i in 1:nrow(allpop_ind)) {
  if(allpop_ind$IID[i] %in% ABS_hist$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "ABShist"
  }else if (allpop_ind$IID[i] %in% ABS_now$Novogene.ID == TRUE) {
    allpop_ind$Population[i] <- "ABScontemp"
  } else if (allpop_ind$IID[i] %in% PLE_hist$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "PLEhist"
  } else if (allpop_ind$IID[i] %in% PLE_now$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "PLEcontemp"
  } else if (allpop_ind$IID[i] %in% JDSP_hist$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "JDSPhist"
  } else if (allpop_ind$IID[i] %in% JDSP_now$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "JDSPcontemp"
  } else if (allpop_ind$IID[i] %in% ONF_hist$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "ONFhist"
  } else if (allpop_ind$IID[i] %in% ONF_now$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "ONFcontemp"
  }  else if (allpop_ind$IID[i] %in% SPSP_hist$Novogene.ID == TRUE){
    allpop_ind$Population[i] <- "SPSPhist"
  } else {
    allpop_ind$Population[i] <- "SPSPcontemp"
  }
}

allpop_ind$Population <- factor(allpop_ind$Population, levels = c("ONFhist", "ONFcontemp","ABShist", "ABScontemp",  "PLEhist", "PLEcontemp", "JDSPhist", "JDSPcontemp", "SPSPhist", "SPSPcontemp"))
allpop_ind<- allpop_ind[order(allpop_ind$Population), ]


# add Column for age
contemp<-grep("contemp", x = allpop_ind$Population)
hist<-grep("hist", x = allpop_ind$Population)
allpop_ind$Age[contemp] <- "Contemporary"
allpop_ind$Age[hist] <- "Historic"

allpop_ind$Age <- as.factor(allpop_ind$Age) 

### CIRCLES FOR CONTEMP, TRIANGLES FOR HIST
ind_roh_scatter<-ggplot(allpop_ind, aes(x=KBAVG, y=NSEG, group=Population)) +
  geom_point(aes(shape=Age, color=Population), size=5)+
  scale_color_manual(values=c(colorlist))+
  theme_minimal()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="white"),
        panel.grid.minor = element_line(color="white"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Average ROH lengths (KB)") + ylab("Number of ROH")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22)) #trbl
ind_roh_scatter



#########################################################################################################################
################################################### FROH ACROSS TIME ####################################################
#########################################################################################################################
all_segments<-read.delim(file = paste0(OUTPATH, "/segments.txt"), header = T,  as.is = T, sep = "\t")
x<- split(all_segments, all_segments$Population)

desired_length <- 8
short.list<- vector(mode = "list", length = desired_length)
str(short.list)
names<-as.vector(names(x))
names(short.list) <- names

for (i in 1:8){
  c.segments<-x[[i]]
  totalseg<-nrow(c.segments)
  short<-sum(c.segments$KB <1000.00) # <1MB
  long<-sum(c.segments$KB >=1000.00)
  both<-data.frame(cbind(short,long))
  short.list[[i]]<-both
}

roh_num<-ldply(short.list, rbind) # combine all the lists together
colnames(roh_num)<-c("Population", "Short","Long")
 
write.table(roh_num, paste0(OUTPATH, "/short_long_counts.txt"), col.names = T, quote = F, row.names = F, sep = "\t")



#########################################################################################################################
#################################################### FROH VS FHATS ######################################################
#########################################################################################################################
froh<-read.delim(paste0(mydir,"/froh_coeff.txt"), header = T, sep = "\t", as.is=T)
pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
low.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/low.miss.ind.txt", sep="\t", header = T, as.is=T)


#################################
######## LOAD .HOM FILES ########
#################################
INPATH="~/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/popgen_analysis/IBC"
OUTPATH="~/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/popgen_analysis/IBC/PLOTS"

#get all the names of your files + their path
myfiles = list.files(path=INPATH, pattern="*.ibc$", full.names=TRUE) # get the full path with files names
mytitles<-basename(myfiles) # get just the file's name
names<-sub(".ibc", "", mytitles)  # remove everything but the population and remove underscores
names<-sub("_", "", names)
names
tables <- lapply(myfiles, read.delim, as.is=T, sep="") # load in all the files as tables in a list
names(tables) <- names # assign respective population names to each list element

#combine all of the dataframes together with each df name as a new column.
all_ibc <- bind_rows(tables, .id = "Population")
#write.table(all_ibc, paste0(OUTPATH, "/IBCs.txt"), col.names = T, quote = F, row.names = F, sep = "\t")

## Combine IBC and FROH
all_ibc<-all_ibc %>% 
  rename(sample = IID)
ibc_froh<-data.frame(merge(all_ibc, froh, by=c("sample", "Population")))




###### Plot ######
for (i in pops){
  cur_df<-ibc_froh[which(ibc_froh$Population == i),]
  
  paste0("fhat1.plot.", i)<-ggplot(cur_df, aes(x=FROH, y=Fhat3))+
    geom_point(alpha=0.8, size = 2.5, color=cur_df)+
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
    theme(plot.title=element_text(hjust=0.5, vjust=2.5)) +  scale_color_manual(values=c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#b82849", "#ff7393", "#e09719", "#f6cd61"))

}

x<- split(ibc_froh, ibc_froh$Population)

desired_length <- 8
fhats.list <- vector(mode = "list", length = desired_length)
str(fhats.list)
names<-as.vector(names(x))
names(fhats.list) <- names
colorlist<-c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#CD4262", "#f598b2", "#e09719", "#f6cd61")

for (i in 1:8){
  c.df<-x[[i]] 
  cur.color<-colorlist[i]
  fhat.plot <-ggplot(c.df, aes(x=FROH, y=Fhat3))+
    geom_point(alpha=0.8, size = 2.5, color=cur.color)+
    geom_smooth(method=lm)+
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
    theme(plot.title=element_text(hjust=0.5, vjust=2.5)) 

  fhats.list[[i]]<-fhat.plot
}

library(gridExtra)

#png(filename = "Graphs/non-mfi.png", width = 1280, height = 960, units = "px")
do.call(grid.arrange, c(fhats.list, list(ncol = 3)))
#dev.off()

ggplot(ibc_froh, aes(x=FROH, y=Fhat3, color=Population))+
  geom_point(alpha=0.8, size = 2.5)+
  facet_wrap(~Population, ncol=2)+
  geom_smooth(method=lm)+
  #theme_minimal()+
  #theme(axis.line = element_line(colour = "black"),
      #  panel.grid.major = element_line(color="white"),
      #  panel.grid.minor = element_line(color="white"),
      #  panel.border = element_blank(),
      #  panel.background = element_blank())+
  xlab("FROH") + ylab("FHAT3")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) 




## USING IBC CALC ALL TOGETHER IN PLINK

ibc_unfil<-read.delim("../../IBC/Filter7.ibc", as.is = T, header=T, sep = '')

ibc_unfil$Population<-NA

for (i in 1:nrow(ibc_unfil)) {
  if(ibc_unfil$IID[i] %in% ABS_hist$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "ABS1990"
  }else if (ibc_unfil$IID[i] %in% ABS_now$Novogene.ID == TRUE) {
    ibc_unfil$Population[i] <- "ABS2017"
  } else if (ibc_unfil$IID[i] %in% PLE_hist$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "PLE1990"
  } else if (ibc_unfil$IID[i] %in% PLE_now$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "PLE2017"
  } else if (ibc_unfil$IID[i] %in% JDSP_hist$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "JDSP1990"
  } else if (ibc_unfil$IID[i] %in% JDSP_now$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "JDSP2017"
  } else if (ibc_unfil$IID[i] %in% ONF_hist$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "ONF1990"
  } else if (ibc_unfil$IID[i] %in% ONF_now$Novogene.ID == TRUE){
    ibc_unfil$Population[i] <- "ONF2017"
  } else {
    ibc_unfil$Population[i] <- "SAV2017"
  }
}

ibc_unfil<-ibc_unfil %>% 
  rename(sample = IID)
ibc_froh2<-data.frame(merge(ibc_unfil, froh, by=c("sample")))

ggplot(ibc_froh2, aes(x=FROH, y=Fhat3, color=Population.y))+
  geom_point(alpha=0.8, size = 2.5)+
  facet_wrap(~Population.y, ncol=2)+
  geom_smooth(method=lm)+
  xlab("FROH") + ylab("FHAT3")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) 



ggplot(ibc_froh2, aes(x=FROH, y=Fhat2))+
  geom_point(alpha=0.8, size = 2.5)+
  geom_smooth(method=lm)+
  #theme_minimal()+
  #theme(axis.line = element_line(colour = "black"),
  #  panel.grid.major = element_line(color="white"),
  #  panel.grid.minor = element_line(color="white"),
  #  panel.border = element_blank(),
  #  panel.background = element_blank())+
  xlab("FROH") + ylab("FHAT3")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) 



################################## FROH VS F (vcftools het) ###################################
all.inds<-read.delim("~/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2020/popgen_analysis/All_Novogene_individs_WGS20x.txt", as.is=T, header = T)

ABS_now<-all.inds[which(all.inds$Category == "ABS Contemporary"),] #30 individuals
ABS_hist<-all.inds[which(all.inds$Category == "ABS Historic"),] #30 individuals

JDSP_now<-all.inds[which(all.inds$Category == "JDSP Contemporary"),] #39 individuals
JDSP_hist<-all.inds[which(all.inds$Category == "JDSP Historic"),] #15 individuals

PLE_now<-all.inds[which(all.inds$Category == "PLE Contemporary"),] #26 individuals
PLE_hist<-all.inds[which(all.inds$Category == "PLE Historic"),] #25 individuals

ONF_now<-all.inds[which(all.inds$Category == "ONF Contemporary"),] #29 individuals
ONF_hist<-all.inds[which(all.inds$Category == "ONF Historic"),] #25 individuals

SAV_now<-all.inds[which(all.inds$Category == "Savannas"),] #13 individuals


library(tidyverse)

#load in all het files
het_unfil<-read.delim("../../HET/Filter7.het", as.is=T, header=T, sep = "")
het_unfil <- het_unfil %>% mutate(Obs.Het = 1-(O.HOM./N.NM.))

#subset out populations (1990s and 2017)
#1990s
ABShist.het <-het_unfil[which(het_unfil$IID %in% ABS_hist$Novogene.ID),]
PLEhist.het <-het_unfil[which(het_unfil$IID %in% PLE_hist$Novogene.ID),]
ONFhist.het <-het_unfil[which(het_unfil$IID %in% ONF_hist$Novogene.ID),]
JDSPhist.het <-het_unfil[which(het_unfil$IID %in% JDSP_hist$Novogene.ID),]

hist.het.df<-rbind(ABShist.het, PLEhist.het, ONFhist.het, JDSPhist.het)

#get a vector of population labels to add onto dataframe
ABShist.pop<-c(rep("ABS1990", nrow(ABShist.het)))
PLEhist.pop<-c(rep("PLE1990", nrow(PLEhist.het)))
ONFhist.pop<-c(rep("ONF1990", nrow(ONFhist.het)))
JDSPhist.pop<-c(rep("JDSP1990", nrow(JDSPhist.het)))

hist.het.df$Population<-c(ABShist.pop, PLEhist.pop, ONFhist.pop, JDSPhist.pop)
#write.table(hist.het.df, "Results/HET/AllPops_Hist_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')

#combine the Obs.Hets and Pop vectors together #put all into dataframe
#het.df.unfilt.hist<- data.frame(AvgHet = c(ABShist.het$Obs.Het, ONFhist.het$Obs.Het, JDSPhist.het$Obs.Het, PLEhist.het$Obs.Het), Population=rep(c("ABS1990", "ONF1990", "JDSP1990", "PLE1990"), c(length(ABShist.het$Obs.Het), length(ONFhist.het$Obs.Het), length(JDSPhist.het$Obs.Het), length(PLEhist.het$Obs.Het))))

#hist.het.df<-read.delim("Results/HET/AllPops_Hist_Het.txt", as.is = T, header=T, sep = '\t')

library(dplyr)
het.df.unfilt.hist<- hist.het.df %>% select(Obs.Het, Population)
colnames(het.df.unfilt.hist)<-c("AvgHet", "Population")

#reorder our populations to plot them 
het.df.unfilt.hist$Population <- factor(het.df.unfilt.hist$Population, levels = c("ABS1990", "ONF1990", "JDSP1990", "PLE1990"))
het.df.unfilt.hist<- het.df.unfilt.hist[order(het.df.unfilt.hist$Population), ]

#plot
require(ggplot2)


#### NOW 2017 SAMPLES
#subset out populations (1990s and 2017)
#2017
ABSnow.het <-het_unfil[which(het_unfil$IID %in% ABS_now$Novogene.ID),]
PLEnow.het <-het_unfil[which(het_unfil$IID %in% PLE_now$Novogene.ID),]
ONFnow.het <-het_unfil[which(het_unfil$IID %in% ONF_now$Novogene.ID),]
JDSPnow.het <-het_unfil[which(het_unfil$IID %in% JDSP_now$Novogene.ID),]
SAVnow.het <-het_unfil[which(het_unfil$IID %in% SAV_now$Novogene.ID),]

het.df.unfilt.now<-rbind(ABSnow.het, PLEnow.het, ONFnow.het, JDSPnow.het,SAVnow.het)

#get a vector of population labels to add onto dataframe
ABSnow.pop<-c(rep("ABS2017", nrow(ABSnow.het)))
PLEnow.pop<-c(rep("PLE2017", nrow(PLEnow.het)))
ONFnow.pop<-c(rep("ONF2017", nrow(ONFnow.het)))
JDSPnow.pop<-c(rep("JDSP2017", nrow(JDSPnow.het)))
SAVnow.pop<-c(rep("SAV2017", nrow(SAVnow.het)))

#write.table(het.df.unfilt.now, "Results/HET/AllPops_Contemp_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')
#het.df.unfilt.now<-read.delim("Results/HET/AllPops_Contemp_Het.txt", as.is = T, header=T, sep = '\t')

#combine the Obs.Hets and Pop vectors together #put all into dataframe
het.df.unfilt.now<- data.frame(AvgHet = c(ABSnow.het$Obs.Het, ONFnow.het$Obs.Het, JDSPnow.het$Obs.Het, PLEnow.het$Obs.Het, SAVnow.het$Obs.Het), Population=rep(c("ABS2017", "ONF2017", "JDSP2017", "PLE2017", "SAV2017"), c(length(ABSnow.het$Obs.Het), length(ONFnow.het$Obs.Het), length(JDSPnow.het$Obs.Het), length(PLEnow.het$Obs.Het), length(SAVnow.het$Obs.Het))))

#reorder our populations to plot them 
het.df.unfilt.now$Population <- factor(het.df.unfilt.now$Population, levels = c("ABS2017", "ONF2017", "JDSP2017", "PLE2017", "SAV2017"))
het.df.unfilt.now<- het.df.unfilt.now[order(het.df.unfilt.now$Population), ]

combined_het<-rbind(het.df.unfilt.hist, het.df.unfilt.now)

#reorder our populations to plot them 
combined_het$Population <- factor(combined_het$Population, levels = c("ABS1990", "ABS2017", "ONF1990", "ONF2017", "JDSP1990", "JDSP2017", "PLE1990", "PLE2017", "SAV2017"))
combined_het<- combined_het[order(combined_het$Population), ]

F.df.unfilt.combine<- data.frame(F = c(ABSnow.het$F, ONFnow.het$F, JDSPnow.het$F, PLEnow.het$F, SAVnow.het$F, ABShist.het$F, ONFhist.het$F, JDSPhist.het$F, PLEhist.het$F), Population=rep(c("ABS2017", "ONF2017", "JDSP2017", "PLE2017", "SAV2017", "ABS1990", "ONF1990", "JDSP1990", "PLE1990"), c(length(ABSnow.het$F), length(ONFnow.het$F), length(JDSPnow.het$F), length(PLEnow.het$F), length(SAVnow.het$F), length(ABShist.het$F), length(ONFhist.het$F), length(JDSPhist.het$F), length(PLEhist.het$F))))

#reorder our populations to plot them 
F.df.unfilt.combine$Population <- factor(F.df.unfilt.combine$Population, levels = c( "ABS1990", "ABS2017", "ONF1990", "ONF2017", "JDSP1990", "JDSP2017", "PLE1990", "PLE2017", "SAV2017"))
F.df.unfilt.combine<- F.df.unfilt.combine[order(F.df.unfilt.combine$Population), ]



froh<-read.table(paste0(mydir,"/froh_coeff.txt"), header = T, sep = "\t", as.is=T)
low.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/low.miss.ind.txt", sep="\t", header = T, as.is=T)

colnames(froh)<-c("IID", "Population", "FROH")

combo_F<-merge(het_unfil, froh)


ggplot(combo_F, aes(x=FROH, y=F, color=Population))+
  geom_point(alpha=0.8, size = 2.5)+
  facet_wrap(~Population, ncol=2)+
  geom_smooth(method=lm)+
  xlab("FROH") + ylab("F")+
  theme(legend.title=element_blank(), legend.text = element_text(size=14), legend.background =element_blank()) +
  #theme(legend.position = "")+
  theme(legend.text = element_text(size=18))+
  theme(plot.title = element_text(hjust=0.5, size=24))+
  theme(axis.title.x = element_text(size=18, vjust=-1.5), axis.title.y = element_text(size=18))+
  theme(axis.text.x = element_text(angle=-35, size=18, vjust=1, hjust=0), axis.text.y=element_text(size = 18))+
  theme(axis.title.y = element_text(vjust=4))+
  theme(plot.margin = margin(15, 22, 15, 22))+ #trbl
  theme(plot.title=element_text(hjust=0.5, vjust=2.5)) 



#########################################################################################################################
#################################################### FROH VS PEDIGREE ######################################################
#########################################################################################################################
roh_inds<-read.delim(file = paste0(OUTPATH, "/roh_inds.txt"), header = T,  as.is = T, sep = "\t")
pops<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Scripts/pop_list_analyses.txt", header = F)
low.miss.ind<-read.delim("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/Updated2021/Filter7/Variant_QC/low.miss.ind.txt", sep="\t", header = T, as.is=T)
ped_coeff<-read.delim("../pedF_for_Tram.txt", as.is = T, header = T)
