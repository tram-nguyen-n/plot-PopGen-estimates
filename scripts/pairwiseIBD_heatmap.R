##################### PLOT PAIRWISE IBD FROM PLINK ##################
# JAN 2023

setwd("/Users/tramnguyen/Box Sync/PhD/Projects/FSJ-WGS20x/2022-July/IBD")

library(reshape2)
library("RColorBrewer")
display.brewer.all()
library("pheatmap")
library("corrplot")

# load in breeders plink ibd -- .genome
pairIBD<-read.delim("breeders_snps_hwe_noZ_genomind15_noLD.genome", as.is = T, header = T, sep="", stringsAsFactors = FALSE)
pairIBD$Population<-NA

all.inds<-read.delim("~/Box Sync/PhD/Projects/Statewide WGS/Novogene/SAMPLE INFO/All_Novogene_individs_WGS20x_August2022.txt", as.is=T, header = T)


ABS_now<-all.inds[which(all.inds$Category == "ABS Contemporary"),] #30 individuals
ABS_hist<-all.inds[which(all.inds$Category == "ABS Historic"),] #30 individuals

JDSP_now<-all.inds[which(all.inds$Category == "JDSP Contemporary"),] #54 individuals
JDSP_hist<-all.inds[which(all.inds$Category == "JDSP Historic"),] #15 individuals

PLE_now<-all.inds[which(all.inds$Category == "PLE Contemporary"),] #26 individuals
PLE_hist<-all.inds[which(all.inds$Category == "PLE Historic"),] #25 individuals

ONF_now<-all.inds[which(all.inds$Category == "ONF Contemporary"),] #29 individuals
ONF_hist<-all.inds[which(all.inds$Category == "ONF Historic"),] #25 individuals

SPSP_hist<-all.inds[which(all.inds$Category == "SPSP Historic"),] #35 individuals
SPSP_now<-all.inds[which(all.inds$Category == "SPSP Contemporary"),] #18 individuals

# add population data
for (i in 1:nrow(pairIBD)) {
  if(pairIBD$IID1[i] %in% ABS_hist$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "ABS1990"
  }else if (pairIBD$IID1[i] %in% ABS_now$Novogene.ID == TRUE) {
    pairIBD$Population[i] <- "ABS2017"
  } else if (pairIBD$IID1[i] %in% PLE_hist$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "PLE1990"
  } else if (pairIBD$IID1[i] %in% PLE_now$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "PLE2017"
  } else if (pairIBD$IID1[i] %in% JDSP_hist$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "JDSP1990"
  } else if (pairIBD$IID1[i] %in% JDSP_now$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "JDSP2017"
  } else if (pairIBD$IID1[i] %in% ONF_hist$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "ONF1990"
  } else if (pairIBD$IID1[i] %in% ONF_now$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "ONF2017"
  }  else if (pairIBD$IID1[i] %in% SPSP_hist$Novogene.ID == TRUE){
    pairIBD$Population[i] <- "SPSP2004"
  } else {
    pairIBD$Population[i] <- "SPSP2017"
  }
}

pairIBD$Population <- factor(pairIBD$Population, levels = c("ONF1990", "ONF2017","ABS1990", "ABS2017", "PLE1990", "PLE2017", "JDSP1990", "JDSP2017", "SPSP2004", "SPSP2017"))
pairIBD<- pairIBD[order(pairIBD$Population), ]


#### Make a matrix of pairwise IBD ####
## Informs acast that you have all these different values and it should make it the levels
#sample.list <- with(pairIBD, sort(unique(c(IID1, IID2)))) #if you want to sort alphabetically by sample name
sample.list <- unique(c(pairIBD$IID1, pairIBD$IID2))
pairIBD$IID1 <- with(pairIBD, factor(IID1, levels = sample.list))
pairIBD$IID2 <- with(pairIBD, factor(IID2, levels = sample.list))

# check that there are no NAs
range(pairIBD$PI_HAT)
#0.0000 0.7327

#convert long-to-wide
x <- acast(pairIBD, IID1 ~ IID2, value.var = "PI_HAT", fill=0, drop=F) ## filling = 1 because missing values are the within same sample IBD that is missing
# fill = NA set everything else that is missing to NA
x  = x+t(x) ### if you had this originally as NA, it couldn't add them
diag(x) <- 1 ## set diagonals to 1
x[1:5,1:5]
isSymmetric(x)

# plot a heatmap
colfunc <- colorRampPalette(c("#CBEDD5", "#439A97")) # use this to get a continuous color scale. first color is low values
pheatmap(x, cluster_cols = F, cluster_rows = F, scale = "none", legend = T, color = colfunc(15))

pheatmap(x, cluster_cols = F, cluster_rows = F, scale = "none", legend = T, color = brewer.pal(9,"YlOrRd"))


# plot only the bottom triangle
M <- x
M[lower.tri(M)] <- NA
M

pheatmap(M, cluster_cols = F, cluster_rows = F, scale = "none", legend = T, color = brewer.pal(9,"YlOrRd"))




