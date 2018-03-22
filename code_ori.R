#####################################################
#####################################################
#####################################################
#####################################################

# Paper: IIGW
# Data: EBTKE hot water Gorontalo (Yuanno Rezky)
# Team leader: Prihadi S.
# Code and analysis: Dasapta Erwin Irawan and Prana Ugi

#####################################################
#####################################################
#####################################################
#####################################################

# Note: remove the '#' symbol only if you haven't done it previously


# Set up working folder

# install packages
## from CRAN server
#install.packages("cluster") 
#install.packages("readxl") 
#install.packages("rpart")
#install.packages("party")
#install.packages("vegan")
#install.packages("mgcv")
#install.packages("gam")
#install.packages("psych")


## from Bioconductor server
#pcaMethods package from Bioconductor server
#source("http://bioconductor.org/biocLite.R") 
#biocLite("pcaMethods")

# Loading library
library(pcaMethods) # for pcaMethods package
library(cluster) # for cluster analysis
#library(readxl) # for opening data directly from xls format
library(rpart)
library(party)
library(vegan)
library(gam)
library(mgcv)
library(psych)

# Load data
df <- read.csv("dataGorontalo2.csv", header=T) # load data 
row.names(df) <- df$ID # setting row names
df <- df[2:21]

#attach(df)
str(df) # observe data structure (str)
head(df) #observe data 
tail(df) # observe data
is.na(df) # checking NA if any

#df <- na.omit(df) # omitting NA if any
row.names(df)
str(df)# viewing data structure

# Exploratory using pairs() function
# Assesing data patterns
cor.tab <- cor(df)
write.csv(cor.tab, "cortab.csv")
univar.tab <- describe(df)
write.csv(univar.tab, "univar.csv")

pairs(df[2:6],
      lower.panel=panel.smooth, 
      upper.panel=NULL, 
      pch=20)
pairs(df[7:21],
      lower.panel=panel.smooth, 
      upper.panel=NULL, 
      pch=20)
pairs(df[11:21],
      lower.panel=panel.smooth, 
      upper.panel=NULL, 
      pch=20)

# Run PCA 
# Run PCA (using pcamethods package)
## svdImpute = standard pca, with imputation, standardised, method univariate (uv)
pca <- pca(df, 
           method = "svdImpute", 
           scale = "uv",
           center = T,
           nPcs = 5,
           evalPcs = 1:5)
summary(pca)


## Evaluating results
plotPcs(pca, Pcs = 1:5)
plot(pca, type="lines")
slplot(pca) # default function in pcamethods but not big enough
loadings(pca) # loadings of each variables
write.csv(loadings(pca), "loadings.csv")
pca
scores(pca) # scores of each samples respectively to each variables

## Plotting results (loadings and scores)
plot.new()
par(mfrow=c(1,2))
plot(loadings(pca), 
     pch = 20,
     main = "Variable loadings",
     sub = "Gorontalo area")
text(loadings(pca), 
     row.names(loadings(pca)),
     cex=0.6, 
     pos=1, 
     col="blue")
abline(v=0, h=0, 
       col = "gray70")

plot(scores(pca), 
     main = "Case scores",
     sub = "Gorontalo area")
abline(v=0, h=0, 
       col = "gray70")
text(scores(pca), row.names(df),
     cex=0.6, 
     pos=2, 
     col="blue", offset=0)
dev.off()

# PCA using'vegan' package
vegan.pca <- rda(df)
vegan.pca
plot(vegan.pca)
biplot(vegan.pca, scaling = -1)
ordilabel(plot(vegan.pca), display="species", font=1, col="gray70") # Add some frame on the label
orditkplot(plot(vegan.pca)) # you see that you can move the labels.

# Cluster analysis (using 'cluster' package)
d <- dist(df, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") # fitting distance to cluster, method Ward
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
summary(fit)

# Cluster anlaysis (using 'vegan' package)
d2 <- vegdist(df)
fit2 <- hclust(d2, "ward.D")
fit3 <- hclust(d2, "complete")
fit4 <- hclust(d2, "average")

par(mfrow=c(1,3))
plot(fit2)
plot(fit3)
plot(fit4)
dev.off()

# Regression tree
tree.fit <- rpart(ec ~ ., data = df, 
                  control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
printcp(tree.fit) # display the results
plotcp(tree.fit) # visualize cross-validation results
summary(tree.fit) # detailed summary of splits


# plot tree
plot(tree.fit, uniform=TRUE,
     main="Tree classification of hot water samples: Gorontalo")
text(tree.fit, use.n=TRUE, all=TRUE, cex=.8)
mod_lm <- gam(ec ~ ., data = df)
summary(mod_lm)


# Ref:
## http://www2.stat.unibo.it/montanari/Didattica/Multivariate/CA_lab.pdf
## http://cc.oulu.fi/~jarioksa/opetus/metodi/sessio3.pdf
## http://www2.stat.unibo.it/montanari/Didattica/Multivariate/PCA_lab1.pdf
## http://bioconductor.wustl.edu/bioc/vignettes/pcaMethods
## https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf
## http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
## https://www3.nd.edu/~mclark19/learn/GAMS.pdf