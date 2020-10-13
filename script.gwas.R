##################################################
########### GWAS and Manhattan plots #############
###########        Paula Silva       #############
########### last update: Sept, 2020  #############
##################################################

### load GAPIT packages and libraries
if (!requireNamespace("BiocManager", quietly = TRUE))
  +     install.packages("BiocManager")
BiocManager::install(c("multtest","gplots", "LDheatmap", "genetics", "ape", "EMMREML", "scatterplot3d"))
aBiocManager::install("Matrix")
library(multtest) 
library(gplots)
library(LDheatmap)
library(genetics)
library(ape)
library(EMMREML)
library(scatterplot3d)
library(compiler)
source("http://zzlab.net/GAPIT/gapit_functions.txt")
source("http://zzlab.net/GAPIT/emma.txt")


### Read phenotypic data
pheno <- read.csv('lr.pheno.lf.csv', header = T, check.names = F)
str(pheno) # check pheno columns are in numeric format
myY <- pheno # rename pheno file
dim(myY) # check dimension of data


### Read genotypic data
geno <- read.delim('LF_GBS.hmp.filt.hmp.txt', header = F) # geno file is hapmap format
myG <- geno # rename geno file
dim(myG) # check dimension of data


### Run naive GWAS first to generate kinship matrix and principal components
myGAPIT <- GAPIT(Y = myY, G = myG, PCA.total = 0)


### Read kinship and principla components (these two files are saved on the your working directory)
myKI <- read.csv('GAPIT.Kin.VanRaden.csv', head = F) # kinship matrix
myCV <- read.csv('GAPIT.PCA.csv', head = T) # principal components


### Run GWAS Q+K model
myGAPIT <- GAPIT(Y = myY, G = myG, KI = myKI, CV = myCV)

##########################################################
##########################################################

#### Re-do Manhattan plots

### load CMplot libraries
library(CMplot)
library(tidyverse)


## Read GWAS results (this file is saved on the your working directory)
## The name of the file has the name of the phenotype (LR_IC2019)
mhtplot <- read.csv('GAPIT.MLM.LR_IC2019.GWAS.Results.csv', header = T, check.names = F)

mhtplot <- mhtplot[,c(1:4)] # keep only the first 4 columns, all the rows

colnames(mhtplot) <-c ("SNP","Chromosome","Position","p_value") ## rename the columns 

# Sort by Chromosome
mhtplot <- mhtplot[order(mhtplot$Chromosome),]

### Rename chromosomes from numeric to chromosomes
level_key <- c(`1` = "1A", `2` = "1B", `3` = "1D", `4` = "2A", `5` = "2B", `6` = "2D",
               `7` = "3A", `8` = "3B", `9` = "3D", `10` = "4A", `11` = "4B", `12` = "4D",
               `13` = "5A", `14` = "5B", `15` = "5D", `16` = "6A", `17` = "6B", `18` = "6D",
               `19` = "7A", `20` = "7B", `21` = "7D")
mhtplot$Chromosome <- recode(mhtplot$Chromosome, !!!level_key)


### Run the function and get the Manhattan plot
### many options to adjust colors and visuals (https://github.com/YinLiLin/CMplot)

bonferroni <- 0.01/nrow(mhtplot) # set your threshold 

CMplot(mhtplot, plot.type = "m", col = c('gray30','gray50','gray80'), 
       cex.axis = 1, cex.lab = 2, pch = 1, LOG10 = T, threshold = bonferroni, 
       threshold.col = "red", signal.col = c("red"), signal.pch = 19, 
       signal.cex = 1.5, chr.den.col = NULL, file = "pdf", memo = "", 
       dpi = 300, file.output = TRUE, verbose = TRUE)

