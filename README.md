# plot-PopGen-estimates

This repository contains R scripts for visualizing popgen estimates of heterozygosity, inbreeding, and relatedness in a whole-genome dataset consisting of 5 populations across two timepoints (N=10 groups). We calculated population genetic estimates using the program PLINK, options ```--het, --ibd, --homozyg, and --genome```. Please see the [plink documentation](https://www.cog-genomics.org/plink/) for details. Relatedness was measured as the shared proportion that is identical-by-descent (IBD) across pairwise comparisions of our individuals. Inbreeding was calculated as the proportion of the genome in runs-of-homozygosity (FROH) and was calculated in [BCFtools/roh](https://samtools.github.io/bcftools/howtos/roh-calling.html).

## 

# 1. Heterozygosity
The follow is an example of a plot produced with ```PLOT_HET.R```

<img src="https://i.imgur.com/qcINk7I.jpg" width=70% height=70%>

This plot looks at site-based heterozygosity (as opppose to nucleotide diversity) in ten sampling groups.

<br />
<br />

# 2. Inbreeding (FROH)

<br />

<img src="https://i.imgur.com/xDWgDNE.png" width=70% height=70%>

This plot investigates the abundance and lengths of ROHs detected in BCFtools. Here I was comparing between PLINK and BCFtools detection of ROH and so this plot constraints ROH parameters to meet our PLINK parameters. This plot shows that our largest, most genetically diverse population of ONF has the fewest and shortest ROH, while our declining and most inbred population at JDSP (pink) has many more ROH that are also longer. 

<br />









