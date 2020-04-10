##This is some code relating to limma and venn diagrams
#venn diagram-graphically rep of levels of overlap btwn data groups
#the code is written as part of the bioconductor project
install.packages(bio)
library('BiocManager')
install('limma')
install('statmod')
a
##LIMMA NOTES(Linear Models for Microarray and RNA-seq Data)
#http://bioinf.wehi.edu.au/limma/ -> website containing limma research criteria
#website shows how to cite both package &research publications
#1.differential expression analysis, using the functions lmFit, eBayes and topTable
#2.RNA-seq analysis :the voom function; linear model analysis tools for RNA-seq read counts.
#                 :backgroundCorrect function
#3.Normalization of two colour micro-array data:read.maimages, normalizeWithinArrays or normalizeBetweenArrays functions
#                 :or duplicateCorrelation function;Use of within-array replicate spots for assessing differential expression in microarray experiments.
#4.Estimating array quality weights: arrayWeights, arrayWeightsSimple or arrayWeightsQuick functions
#                                  ;Empirical array quality weights in the analysis of microarray data
#5.separate channel analysis of two-channel microarrays:lmscFit function
#6.construction of design matrices for a number of standard experimental designs
##LIMMA MENU DRIVEN INTERFACE
#a).limmaGUI=>Two color Array
#b).affylmGUI=>Affymetrix/affimetrix arrays-gene expression profiling(
#cellular fucntion analysis by measurement of thousands of genes activities at once)
#eg distinguish btwn actively dividing or show cell reaction to partic treatment

BiocManager::install('limmaGUI')
a
library('limmaGUI')

BiocManager::install('affylmGUI')
a
library(affylmGUI)


library(tcltk)#affylmGUI uses the R package named "tcltk"
affylmGUI()#running affylmGUI
##venn diagrams
library("limmaGUI")#not necessary for subsequent commands anyway
library('limma')
hsb2 <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsb2-3.csv")
#binary variables indicating membership
attach(hsb2)
View(hsb2)
head(hsb2)
hw <- (write >= 60)
hm <- (math >= 60)
hr <- (read >= 60)
c3 <- cbind(hw, hm, hr)#bind columns based on false or true criteria for the conditions
c3
str(c3)#structure of c3
a <- vennCounts(c3)#vennCounts imposes structure needed for Venn diagrams
a
vennDiagram(a)#plots a vennDiagram
vennDiagram(a, include = "both", 
            names = c("High Writing", "High Math", "High Reading"), 
            cex = 1, counts.col = "purple")
##two groups
g <- cbind(
  g1 = c(rep(0, 6), rep(1, 3)), 
  g2 = c(rep(1, 6), rep(0, 3)))
d <- vennCounts(g)
vennDiagram(d)
