## PLOT HET + ADD STATS ##
## JAN 2023 ##

library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

#ABS C, ABS H, ONF, JDSP, PLE, SPSP
#colorlist<-c("#1b6ca8", "#7CBBDE", "#438876", "#61C3AA", "#C06029", "#F6B38C", "#B88706", "#E8B120", "#CD4262", "#E18DA0")
colorlist<-c("#3AB495", "#3AB495","#268FC8", "#268FC8", "#E6A908", "#E6A908", "#C06029", "#C06029", "#CD4262", "#CD4262")

setwd("~/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/HET")

###########################################################################################
#################################### PLOT HET TOGETHER #################################### 
###########################################################################################

all_het <- read.delim("breeders_snps_hwe_noZ_genomind15_noLD.het", as.is=T, header=T, sep="")
all_het <- all_het %>% mutate(Obs.Het = 1-(O.HOM./N.NM.)) #get new column of heterozygosity

all.inds<-read.delim("~/Box Sync/PhD/Projects/Statewide WGS/Novogene/SAMPLE INFO/All_Novogene_individs_WGS20x_August2022.txt", as.is=T, header = T) #load in metadata

ABS_now<-all.inds[which(all.inds$Category == "ABS Contemporary"),] #30 individuals
ABS_hist<-all.inds[which(all.inds$Category == "ABS Historic"),] #30 individuals

JDSP_now<-all.inds[which(all.inds$Category == "JDSP Contemporary"),] #54 individuals
JDSP_hist<-all.inds[which(all.inds$Category == "JDSP Historic"),] #15 individuals

PLE_now<-all.inds[which(all.inds$Category == "PLE Contemporary"),] #26 individuals
PLE_hist<-all.inds[which(all.inds$Category == "PLE Historic"),] #25 individuals

ONF_now<-all.inds[which(all.inds$Category == "ONF Contemporary"),] #35 individuals
ONF_hist<-all.inds[which(all.inds$Category == "ONF Historic"),] #25 individuals

SPSP_hist<-all.inds[which(all.inds$Category == "SPSP Historic"),] #35 individuals
SPSP_now<-all.inds[which(all.inds$Category == "SPSP Contemporary"),] #18 individuals

#### PLOT ####
#subset out populations (1990s and 2017)
#1990s
ABShist.het <-all_het[which(all_het$IID %in% ABS_hist$Novogene.ID),]
PLEhist.het <-all_het[which(all_het$IID %in% PLE_hist$Novogene.ID),]
ONFhist.het <-all_het[which(all_het$IID %in% ONF_hist$Novogene.ID),]
JDSPhist.het <-all_het[which(all_het$IID %in% JDSP_hist$Novogene.ID),]
SPSPhist.het <-all_het[which(all_het$IID %in% SPSP_hist$Novogene.ID),]

hist.het.df<-rbind(ABShist.het, PLEhist.het, ONFhist.het, JDSPhist.het, SPSPhist.het)

#get a vector of population labels to add onto dataframe
ABShist.pop<-c(rep("ABS historic", nrow(ABShist.het)))
PLEhist.pop<-c(rep("PLE historic", nrow(PLEhist.het)))
ONFhist.pop<-c(rep("ONF historic", nrow(ONFhist.het)))
JDSPhist.pop<-c(rep("JDSP historic", nrow(JDSPhist.het)))
SPSPhist.pop<-c(rep("SPSP historic", nrow(SPSPhist.het)))

hist.het.df$Population<-c(ABShist.pop, PLEhist.pop, ONFhist.pop, JDSPhist.pop, SPSPhist.pop)
#write.table(hist.het.df, "HET/AllPops_Hist_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')
het.df.unfilt.hist<- hist.het.df %>% select(Obs.Het, Population)
colnames(het.df.unfilt.hist)<-c("AvgHet", "Population")

#reorder our populations to plot them 
het.df.unfilt.hist$Population <- factor(het.df.unfilt.hist$Population, levels = c("ABS historic", "ONF historic", "JDSP historic", "PLE historic", "SPSP historic"))
het.df.unfilt.hist<- het.df.unfilt.hist[order(het.df.unfilt.hist$Population), ]



#2017
ABSnow.het <-all_het[which(all_het$IID %in% ABS_now$Novogene.ID),]
PLEnow.het <-all_het[which(all_het$IID %in% PLE_now$Novogene.ID),]
ONFnow.het <-all_het[which(all_het$IID %in% ONF_now$Novogene.ID),]
JDSPnow.het <-all_het[which(all_het$IID %in% JDSP_now$Novogene.ID),]
SPSPnow.het <-all_het[which(all_het$IID %in% SPSP_now$Novogene.ID),]

#get a vector of population labels to add onto dataframe
ABSnow.pop<-c(rep("ABS contemporary", nrow(ABSnow.het)))
PLEnow.pop<-c(rep("PLE contemporary", nrow(PLEnow.het)))
ONFnow.pop<-c(rep("ONF contemporary", nrow(ONFnow.het)))
JDSPnow.pop<-c(rep("JDSP contemporary", nrow(JDSPnow.het)))
SPSPnow.pop<-c(rep("SPSP contemporary", nrow(SPSPnow.het)))

#combine the Obs.Hets and Pop vectors together #put all into dataframe
het.df.unfilt.now<- data.frame(AvgHet = c(ABSnow.het$Obs.Het, ONFnow.het$Obs.Het, JDSPnow.het$Obs.Het, PLEnow.het$Obs.Het, SPSPnow.het$Obs.Het), Population=rep(c("ABS contemporary", "ONF contemporary", "JDSP contemporary", "PLE contemporary", "SPSP contemporary"), c(length(ABSnow.het$Obs.Het), length(ONFnow.het$Obs.Het), length(JDSPnow.het$Obs.Het), length(PLEnow.het$Obs.Het), length(SPSPnow.het$Obs.Het))))

#reorder our populations to plot them 
het.df.unfilt.now$Population <- factor(het.df.unfilt.now$Population, levels = c("ABS contemporary", "ONF contemporary", "JDSP contemporary", "PLE contemporary", "SPSP contemporary"))
het.df.unfilt.now<- het.df.unfilt.now[order(het.df.unfilt.now$Population), ]

#write.table(het.df.unfilt.now, "Results/HET/AllPops_Contemp_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')
#het.df.unfilt.now<-read.delim("Results/HET/AllPops_Contemp_Het.txt", as.is = T, header=T, sep = '\t')
#Plot Het of all populations for both years together.

#### TEST ABS AND PLE 

wilcox.test(ABShist.het$Obs.Het, PLEhist.het$Obs.Het, alternative = "greater") # in 1990 ABS was signficantly higher than PLE
wilcox.test(ABSnow.het$Obs.Het, PLEnow.het$Obs.Het, alternative = "greater")

################################# COMBINE #####################################
combined_het<-rbind(het.df.unfilt.hist, het.df.unfilt.now)

#reorder our populations to plot them 
combined_het$Population <- factor(combined_het$Population, levels = c("ONF historic", "ONF contemporary", "ABS historic", "ABS contemporary", "PLE historic", "PLE contemporary", "JDSP historic", "JDSP contemporary", "SPSP historic", "SPSP contemporary"))
combined_het<- combined_het[order(combined_het$Population), ]

combined_het.plot<-ggplot(data=combined_het, aes(x=Population,y=AvgHet, fill=Population)) + 
  geom_boxplot(alpha=0.95) +
  xlab("")+ ylab("Site-based heterozygosity") +
  scale_fill_manual(values=colorlist)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=16, angle = 22, vjust = 0.6), axis.text.y=element_text(size = 16))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 25, 12, 20))+
  #scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP 2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))

combined_het.plot

combined_het.plot <- combined_het.plot + geom_jitter(size=1.5, shape=19, position=position_jitter(0))
#ggsave(combined_het.plot, file="PLOTS/Het_noStats.pdf", height = 8, width = 14, units = "in", device="pdf", dpi=800)

##########################################################################
############################### ADD STATS ################################
##########################################################################

# add categories to dataframe
combined_het$bothYr<-NA
combined_het$bothYr[which(combined_het$Population == "ABS historic" | combined_het$Population == "ABS contemporary")]<-"bothABS"
combined_het$bothYr[which(combined_het$Population == "PLE historic" | combined_het$Population == "PLE contemporary")]<-"bothPLE"
combined_het$bothYr[which(combined_het$Population == "JDSP historic" | combined_het$Population == "JDSP contemporary")]<-"bothJDSP"
combined_het$bothYr[which(combined_het$Population == "ONF historic" | combined_het$Population == "ONF contemporary")]<-"bothONF"
combined_het$bothYr[which(combined_het$Population == "SPSP historic" | combined_het$Population == "SPSP contemporary")]<-"bothSPSP"

colorlist<-c("#1b6ca8", "#7CBBDE", "#0c9c5f", "#61C3AA", "#CD4262", "#f598b2", "#e09719", "#f6cd61")

# get summary stats - mean and sd for each population group
summary.het<-combined_het %>%
  group_by(Population) %>%
  get_summary_stats(AvgHet, type = "mean_sd")

# test ANOVA across all groups
res.aov <- combined_het %>% anova_test(AvgHet ~ Population)
res.aov

# pairwise t-test and wilcoxon for each group - with bonferroni corrections
#pwc <- combined_het %>%
#  pairwise_t_test(AvgHet ~ Population, p.adjust.method = "bonferroni")
#pwc

pwc <- combined_het %>%
  pairwise_wilcox_test(AvgHet ~ Population, p.adjust.method = "bonferroni")
pwc

# years combined across populations
pwc <- combined_het %>%
  pairwise_wilcox_test(AvgHet ~ bothYr, p.adjust.method = "bonferroni")
pwc

#write.table(pwc, file="wilcoxon_het_info.txt", quote = F, col.names = T, row.names = F, sep="\t")

#Auto-compute p-value label positions using the function add_xy_position()
pwc <- pwc %>% add_xy_position(x = "Population")

# Show significance level, Hide non-significant tests
ggboxplot(combined_het, x = "Population", y = "AvgHet", fill="Population", palette = c(colorlist))+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )+
  xlab("")+ ylab("Site-based heterozygosity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=14, angle = 30, vjust = 0.6), axis.text.y=element_text(size = 14))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 12, 10, 22))+
  scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP 2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))
  

# get all pairwise comparisons in GGPLOT2 -- compare means
comp <- list(c("ONF historic", "ONF contemporary"), c("ABS historic", "ABS contemporary"), c("PLE historic", "PLE contemporary"), c("JDSP historic", "JDSP contemporary"), c("SPSP historic", "SPSP contemporary")) # within populations, across years

comp <- list(c("bothONF", "bothABS"), c("bothONF", "bothJDSP"), c("bothONF", "bothPLE"), c("bothONF", "bothSPSP"), c("bothABS", "bothPLE"), c("bothABS", "bothJDSP"), c("bothABS", "bothSPSP"), c("bothPLE", "bothJDSP"), c("bothPLE", "bothSPSP"), c("bothJDSP", "bothSPSP")) # across populations - years combined
             
# get all the pairwise comparisons you want from the data
comp <- list(c("PLE historic", "PLE contemporary"), c("ONF contemporary", "ABS contemporary"), c("ONF historic", "ABS historic"), c("ONF historic", "JDSP historic"), c("ONF historic", "PLE historic"), c("ONF contemporary", "PLE contemporary"), c("ONF contemporary", "JDSP contemporary"), c("SPSP historic", "SPSP contemporary"), c("ONF historic", "SPSP historic"), c("ONF contemporary", "SPSP contemporary"), c("ABS historic", "SPSP historic"), c("ABS contemporary", "SPSP contemporary"))

#plot
combined_het.plot<-ggplot(data=combined_het, aes(x=Population,y=AvgHet, fill=Population)) + 
  geom_boxplot(alpha=0.95) +
  stat_compare_means(comparisons = comp, method = "wilcox.test" , label = "p.signif", size = 6)+
  xlab("")+ ylab("Average individual heterozygosity") +
  scale_fill_manual(values=c(colorlist))+
  theme_bw() +
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
  theme(plot.margin = margin(12, 12, 10, 22))+
  #scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP 2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))
combined_het.plot
# label.y = c(11900, 12200, 12500)
#combined_het.plot<- combined_het.plot + ggtitle("Site-based heterozygosity across all populations\n using 10 million SNPs (with 1e-05 HWE filtering)") + 
#theme(plot.title = element_text(size=18, hjust = 0.5))
combined_het.plot

combined_het.plot<-combined_het.plot + geom_jitter(size=1.5, shape=19, position=position_jitter(0))
combined_het.plot
ggsave(combined_het.plot, file="PLOTS/Het1.pdf", height = 8, width = 14, units = "in", device="pdf", dpi=800)


##########################################################################
############################ STATS QC FIRST ##############################
##########################################################################

# visualize first!
boxplot(AvgHet ~ Population, data=combined_het) #how does your occupation predict your wage/salary

#Assumptions:
# 1. Independent observations
# 2. Normality of the residuals/observations (histograms of the residuals to check)
# 3. Homogeneous variance (side by side boxplots, scatterplot of the residuals versus predicted values)
# Test if variance is equal across Occupation (How variable in the wages is about the same across the occupation/categorical variables?)

#THE VARIANCES ARE NOT EVEN/EQUAL ACROSS POPS
bartlett.test(combined_het$AvgHet, combined_het$Population)
#a small p value small means that we'd reject the null hyp that the variances are EVEN/EQUAL across the variance!! We don't want this. Let's just log it.

boxplot(log(combined_het$AvgHet)~Population, data=combined_het)
bartlett.test(log(combined_het$AvgHet), combined_het$Population)

#CHECK NORMALITY -- NOT MET
anv1=lm(log(AvgHet)~Population, data=combined_het)
hist(resid(anv1))
plot(predict(anv1), resid(anv1)) #One way ANOVA, estimating a mean expected. There are only going to be 6 (for the occupations)
#we want to see that across the occupations that the vertical spread is the same across the 6 means

summary(anv1)
#anova -- one overall test to see if any signficant different between all the 6 variables
anova(anv1) #small p means there is one or more Populations that is different than the others
#IF THIS WERE NOT SIGNIF, DONT NEED TO DO PAIRWISE

library(emmeans)
emmeans(anv1, pairwise ~ Population, type="response")




######################### FILTER OUT LL INDIVIDUALS FROM PLE #############################
library("readxl")
notes <- as.data.frame(read_excel("~/Box Sync/PhD/Field Notes/PLE_banding_TN2018.xlsx"))
LL <- notes$USFWS[notes$Region == "LL"]
LL <- c(LL, "1713-13409") # this was a bird caught by Hugh Ellis that needs to be added

all_het <- read.delim("breeders_snps_hwe_noZ_genomind15_noLD.het", as.is=T, header=T, sep="")
all_het <- all_het %>% mutate(Obs.Het = 1-(O.HOM./N.NM.)) #get new column of heterozygosity

all.inds<-read.delim("~/Box Sync/PhD/Projects/Statewide WGS/Novogene/SAMPLE INFO/All_Novogene_individs_WGS20x_August2022.txt", as.is=T, header = T) #load in metadata

## REMOVE LL INDIVIDUALS
rm <- which(all.inds$USFWS %in% LL)
all.inds <- all.inds[-rm,] # N=284

ABS_now<-all.inds[which(all.inds$Category == "ABS Contemporary"),] #30 individuals
ABS_hist<-all.inds[which(all.inds$Category == "ABS Historic"),] #30 individuals

JDSP_now<-all.inds[which(all.inds$Category == "JDSP Contemporary"),] #54 individuals
JDSP_hist<-all.inds[which(all.inds$Category == "JDSP Historic"),] #15 individuals

PLE_now<-all.inds[which(all.inds$Category == "PLE Contemporary"),] #26 individuals -- 17 if you remove LL
PLE_hist<-all.inds[which(all.inds$Category == "PLE Historic"),] #25 individuals

ONF_now<-all.inds[which(all.inds$Category == "ONF Contemporary"),] #35 individuals
ONF_hist<-all.inds[which(all.inds$Category == "ONF Historic"),] #25 individuals

SPSP_hist<-all.inds[which(all.inds$Category == "SPSP Historic"),] #35 individuals
SPSP_now<-all.inds[which(all.inds$Category == "SPSP Contemporary"),] #18 individuals


#### PLOT ####
#subset out populations (1990s and 2017)
#1990s
ABShist.het <-all_het[which(all_het$IID %in% ABS_hist$Novogene.ID),]
PLEhist.het <-all_het[which(all_het$IID %in% PLE_hist$Novogene.ID),]
ONFhist.het <-all_het[which(all_het$IID %in% ONF_hist$Novogene.ID),]
JDSPhist.het <-all_het[which(all_het$IID %in% JDSP_hist$Novogene.ID),]
SPSPhist.het <-all_het[which(all_het$IID %in% SPSP_hist$Novogene.ID),]

hist.het.df<-rbind(ABShist.het, PLEhist.het, ONFhist.het, JDSPhist.het, SPSPhist.het)

#get a vector of population labels to add onto dataframe
ABShist.pop<-c(rep("ABS historic", nrow(ABShist.het)))
PLEhist.pop<-c(rep("PLE historic", nrow(PLEhist.het)))
ONFhist.pop<-c(rep("ONF historic", nrow(ONFhist.het)))
JDSPhist.pop<-c(rep("JDSP historic", nrow(JDSPhist.het)))
SPSPhist.pop<-c(rep("SPSP historic", nrow(SPSPhist.het)))

hist.het.df$Population<-c(ABShist.pop, PLEhist.pop, ONFhist.pop, JDSPhist.pop, SPSPhist.pop)
#write.table(hist.het.df, "HET/AllPops_Hist_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')
het.df.unfilt.hist<- hist.het.df %>% select(Obs.Het, Population)
colnames(het.df.unfilt.hist)<-c("AvgHet", "Population")

#reorder our populations to plot them 
het.df.unfilt.hist$Population <- factor(het.df.unfilt.hist$Population, levels = c("ABS historic", "ONF historic", "JDSP historic", "PLE historic", "SPSP historic"))
het.df.unfilt.hist<- het.df.unfilt.hist[order(het.df.unfilt.hist$Population), ]


#2017
ABSnow.het <-all_het[which(all_het$IID %in% ABS_now$Novogene.ID),]
PLEnow.het <-all_het[which(all_het$IID %in% PLE_now$Novogene.ID),]
ONFnow.het <-all_het[which(all_het$IID %in% ONF_now$Novogene.ID),]
JDSPnow.het <-all_het[which(all_het$IID %in% JDSP_now$Novogene.ID),]
SPSPnow.het <-all_het[which(all_het$IID %in% SPSP_now$Novogene.ID),]

#get a vector of population labels to add onto dataframe
ABSnow.pop<-c(rep("ABS contemporary", nrow(ABSnow.het)))
PLEnow.pop<-c(rep("PLE contemporary", nrow(PLEnow.het)))
ONFnow.pop<-c(rep("ONF contemporary", nrow(ONFnow.het)))
JDSPnow.pop<-c(rep("JDSP contemporary", nrow(JDSPnow.het)))
SPSPnow.pop<-c(rep("SPSP contemporary", nrow(SPSPnow.het)))

#combine the Obs.Hets and Pop vectors together #put all into dataframe
het.df.unfilt.now<- data.frame(AvgHet = c(ABSnow.het$Obs.Het, ONFnow.het$Obs.Het, JDSPnow.het$Obs.Het, PLEnow.het$Obs.Het, SPSPnow.het$Obs.Het), Population=rep(c("ABS contemporary", "ONF contemporary", "JDSP contemporary", "PLE contemporary", "SPSP contemporary"), c(length(ABSnow.het$Obs.Het), length(ONFnow.het$Obs.Het), length(JDSPnow.het$Obs.Het), length(PLEnow.het$Obs.Het), length(SPSPnow.het$Obs.Het))))

#reorder our populations to plot them 
het.df.unfilt.now$Population <- factor(het.df.unfilt.now$Population, levels = c("ABS contemporary", "ONF contemporary", "JDSP contemporary", "PLE contemporary", "SPSP contemporary"))
het.df.unfilt.now<- het.df.unfilt.now[order(het.df.unfilt.now$Population), ]

#write.table(het.df.unfilt.now, "Results/HET/AllPops_Contemp_Het.txt", col.names = T, quote = F, row.names = F, sep = '\t')
#het.df.unfilt.now<-read.delim("Results/HET/AllPops_Contemp_Het.txt", as.is = T, header=T, sep = '\t')
#Plot Het of all populations for both years together.

################################# COMBINE #####################################
combined_het<-rbind(het.df.unfilt.hist, het.df.unfilt.now)

#reorder our populations to plot them 
combined_het$Population <- factor(combined_het$Population, levels = c("ONF historic", "ONF contemporary", "ABS historic", "ABS contemporary", "PLE historic", "PLE contemporary", "JDSP historic", "JDSP contemporary", "SPSP historic", "SPSP contemporary"))
combined_het<- combined_het[order(combined_het$Population), ]

combined_het.plot<-ggplot(data=combined_het, aes(x=Population,y=AvgHet, fill=Population)) + 
  geom_boxplot(alpha=0.95) +
  xlab("")+ ylab("Site-based heterozygosity") +
  scale_fill_manual(values=colorlist)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="gray 93"),
        panel.grid.minor = element_line(color="gray 94"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))+
  theme(legend.position=(""))+
  theme(legend.text = element_text(hjust=0.5, size=14))+
  theme(axis.text.x = element_text(size=16, angle = 22, vjust = 0.6), axis.text.y=element_text(size = 16))+
  theme(axis.title.y = element_text(vjust=5))+
  theme(axis.title.x = element_text(vjust=-0.5))+
  theme(plot.margin = margin(12, 25, 12, 20))+
  #scale_x_discrete(labels=c("ABS 1992", "ABS 2017", "ONF 1992", "ONF 2017", "JDSP 1992", "JDSP 2017", "PLE 1992", "PLE 2017", "SPSP 2004", "SPSP 2017"))+
  theme(plot.title=element_text(hjust=0.5, vjust=2.5))

combined_het.plot


## test whether ABS and PLE are signficantly different
wilcox.test(ABShist.het$Obs.Het, PLEhist.het$Obs.Het, alternative = "greater") # in 1990 ABS was signficantly higher than PLE
wilcox.test(ABSnow.het$Obs.Het, PLEnow.het$Obs.Het, alternative = "greater") # in contemporary times they are not longer significantly different -- in 2008 they were still signifcantly different. How to interpret?


