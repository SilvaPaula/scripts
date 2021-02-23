### Figure 9
### Phylogeny

# load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load('data.table','ape','phyclust','tidytree')

# read hap file numeric
hap01 <- fread("file.txt", header = T, check.names = F, data.table = F) # geno file in numeric format
geno <- t(as.matrix(hap01[, 16:ncol(hap01)])) # keep only genotypes

# read the pheno file
pop <-read.csv(file="file.csv", header = T, as.is = T, check.names = F)


###################################
## Hierarchical Cluster Analysis 
###################################
# compute genetic distance
ifelse(test = file.exists('distMat.RData'),
       yes = load('distMat.RData'), no = distMat <- dist(geno))
# saving distance matrix because it's a computationally intensive step
if (!file.exists('distMat.RData')) save(distMat, file = "distMat.RData")

# cluster accessions and convert to phylo object for ape
hc2 <- as.phylo(hclust(distMat, method = 'average'))

# cluster coloring
edgecols <- cbind('line'=NA, 1:nrow(hc2$edge), color='black') # create data frame
edgecols[,1] <- hc2$tip.label[hc2$edge[,2]] # get labels
edgecols <- as.matrix(merge(edgecols, pop, by = 'line', all.x = T)) # get samples info
edgecols <- edgecols[order(as.numeric(edgecols[,2])), ] # get samples in original order

# coloring R/S accessions with different colors
edgecols[,3][edgecols[,6] == 'S'] = "grey70"
edgecols[,3][edgecols[,6] == 'R'] = "purple"

# tips coloring
tipcols <- as.matrix(merge(hc2$tip.label, edgecols, by = 1, sort = F))

# coloring diff lineages with different colors
tipcols[,3][tipcols[,5] == 'L1'] = "#3E4A89FF"
tipcols[,3][tipcols[,5] == 'L2'] = "#1F9E89FF"
tipcols[,3][tipcols[,5] == 'W'] = "#B4DE2CFF"

# plotting tree
pdf("Figure9.pdf", width = 10, height = 10)
plot.phylo(hc2, type = 'p', show.tip.label = T, label.offset = 0.2, cex = 0.4,
           edge.color = edgecols[,3], edge.width = 2, tip.color = tipcols[,3]) 
legend(0, 120, lty = 1, lwd = 3, cex = 1, box.lwd = 1,
       legend = c('Lineage 1 (L1)','Lineage 2 (L2)','Wheat (W)','WCM Resistant','WCM Susceptible'),
       text.col = c("#3E4A89FF","#1F9E89FF","#B4DE2CFF","black","black"), col = c("black","black","black","purple","grey70"))
axisPhylo(1, cex.axis = 0.8)
dev.off()
