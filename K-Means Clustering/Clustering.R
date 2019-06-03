
library(readr)
tptclm <- read_csv("clusterassgn.csv",  
                    col_types = cols(TRV.DT = col_date(format = "%Y-%m-%d"),
                                     CLM.SUB.DT = col_date(format = "%Y-%m-%d"),
                                     CLM.PYM.DT = col_date(format = "%Y-%m-%d"),
                                     JOURNEY.STR = col_time(format = "%H:%M:%S")))

# SUB.INT = Time lapse between travel and submission date
SUB.INT <- data.frame(tptclm$CLM.SUB.DT - 
                        tptclm$TRV.DT)
colnames(SUB.INT) <- "SUB.INT"

# PYM.INT = Time lapse between submission and paid date
PYM.INT <- data.frame(tptclm$CLM.PYM.DT - 
                        tptclm$CLM.SUB.DT)
colnames(PYM.INT) <- "PYM.INT"

# Remove date columns from tptclm.raw and replace with intervals
date.col.index <- c(5,6,8)
tptclm <- cbind.data.frame(tptclm[, -date.col.index], SUB.INT, PYM.INT)


# Q2: From the dataset you have loaded in Q1,...
# Remove redundant columns ... 
# commenting also on the reason for removing them ...
# Using a variable selection index e.g. index <- c(1:5),... (1m)
# creating a new dataframe of your selected variables,...
# naming it "tptclm.sel" (1m)
# (Total: 2m)
str(tptclm)
View(tptclm)

# Firstly, I removed the following 4 variables because it is not unique (columns 3,5,7,11)
length(unique(tptclm$CLM.SYS))
length(unique(tptclm$TIMEZONE))
length(unique(tptclm$CLM.PRO.STAT))
length(unique(tptclm$CLM.STAT))
library(dplyr)

#Next, I removed the Employee ID and Reference Number variables (Columns 1 and 4) because they are identification
#variables and do not provide any valuable insight to the clustering. 
 
#Putting it all together, 
tptclm.sel <- tptclm[,-c(1,3,4,5,7,11)]

# Q3: Using the appropriate functions,...
# carry out integer encoding on the categorical variables,... (3m)
# convert the time variables to numeric,... (1m)
# and then creating a dataframe ...
# combining the non-categorical variables ... 
# with the integer-encoded ones ...
# naming your combined dataframe "tptclm.num" (1m)
# (Total: 5m)

library(qdapTools)
DE.PT <- data.frame(unique(tptclm.sel$DEPT),
                    c(1:length(unique(tptclm.sel$DEPT))))

WK.DY <- data.frame(unique(tptclm.sel$DAY),
                    c(1:length(unique(tptclm.sel$DAY))))

PR.CD <- data.frame(unique(tptclm.sel$PURP),
                    c(1:length(unique(tptclm.sel$PURP))))

# using %l% as a lookup function for matching
DE.PT.N <- data.frame(tptclm.sel$DEPT %l% DE.PT)
colnames(DE.PT.N) <- "DE.PT.N"

WK.DY.N <- data.frame(tptclm.sel$DAY %l% WK.DY)
colnames(WK.DY.N) <- "WK.DY.N"

PR.CD.N <- data.frame(tptclm.sel$PURP %l% PR.CD)
colnames(PR.CD.N) <- "PR.CD.N"


tptclm.sel$JOURNEY.STR = as.numeric(tptclm.sel$JOURNEY.STR)

# Remove categorical columns from tptclm.sel and replace with integer encoded columns
cat.col.index <- c(1,3,4)

tptclm.num <- cbind.data.frame(tptclm.sel[, -cat.col.index],
                               DE.PT.N,
                               WK.DY.N,
                               PR.CD.N)


# Q4: Carry out normalisation of "tptclm.num" ...
# naming your normalised dataframe "tptclm.norm" (1m)
# commenting on why normalisation needs to be done (1m)
# (Total: 2m)

tptclm.norm <- sapply(tptclm.num ,scale)
rownames(tptclm.norm) <-rownames(tptclm.num)

#Normalization needs to be done because it controls the variability in a dataset.
#Eucledian distances are highly scale dependent, and different variables might have different scales, eg
#a difference of 1 unit in JOURNEY.STR and 1 unit in CLM.AMT has different significance. 

# Q5: Using an appropriate method and algorithm,...
# determine the optimal number (k) of clusters for "tptclm.num",...
# ensuring that essential plots are shown (4m)
# Justify your chosen k (1m)
# (Total: 5m)

set.seed(1)
k.max <- 15
wss <- (nrow(tptclm.norm)-1)*sum(apply(tptclm.norm,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(tptclm.norm,
                                        centers=i,
                                        iter.max = 15, algorithm = "Hartigan-Wong")$withinss)

plot(1:k.max, wss, type="b", xlab="Number of Clusters K",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot to find Optimal Number of Clusters",
     pch=19, cex=1)

#My chosen k value is 7 because it is where the elbow starts.

abline(v = 7, lty =2)

# Q6: Apply the k-means clustering algorithm to "tptclm.norm"... (1m)
# while also showing essential information about the clustering done (1m)
# (Total: 2m)

C <- 7
km <- kmeans(tptclm.norm,C)

#Essential information of the cluster: 
# Number of records in each cluster
km$size 

# Cluster Centroids
km$centers

#dist between cluster centers
dist(km$centers)

# Q7: Derive your cluster numbers and assign them...
# as a new column called "ClusterNumber",... (2m)
# while also ensuring that your dataframe is readable... (1m)
# i.e. time and dates are in the right format...
# naming this dataframe "tptclm.clustn" (1m)
# (Total: 4m)

ClusterNumber <-data.frame(km$cluster)
tptclm.clustn <-cbind.data.frame(tptclm.sel,
                                 ClusterNumber)

# Q8: Using an appropriate function from a package,...
# Visualise your clusters in a way ... (2m)
# that allows your audience to identify outliers
# commenting also on where your outliers are (1m)
# Export the plot as a .pdf file (1m)
# (Total: 4m)

library(cluster)

pdf("clusterplot.pdf") 
clusplot(tptclm.norm, km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=1)
dev.off() 

#The outliers are in 5th cluster. 


# Q9: Carry out outlier distance scoring,... (1m)
# using an appropriate method to rank your top outliers...
# explaining briefly how the distance scoring is done (2m)
# (Total: 3m)

km.centers <- km$centers[km$cluster, ]
km.dist <- sqrt(rowSums((tptclm.norm - km.centers)^2))

# calculate mean distances by cluster:
mdist.km <- tapply(km.dist, km$cluster, mean)

# divide each distance by the mean for its cluster:
distscore.km <- km.dist/(mdist.km[km$cluster])
distfact.km <- data.frame(distscore.km)
colnames(distfact.km) <- "DIST.FACTOR"

#!!!!!!!! HOW SCORING IS DONE !!!!!!!!#

#Firstly, we find the cluster centroids and we calculate the distance between the individual records 
#from the cluster centroids using eucledian distance. 
#We do this because if remove outliers from just distance all the outliers will be from the same cluster. 
#Therefore, we have to benchmark the outlier from the cluster itself.

#Secondly, we calculate the mean distances by cluster and we score each record using the ratio  
# of actual distance to mean distance.

#That is how distance scoring is done. 

# Distance Score Percentile
scoreperc.km <- data.frame(distscore.km/(max(distscore.km)
                                         -min(distscore.km)))
colnames(scoreperc.km) <- "SCORE.PERC"

# dataframe of dataset with cluster#,distance score,distance score percentile
tptclm.clustdn<-cbind.data.frame(tptclm.clustn,
                                 distfact.km,
                                 scoreperc.km)

# For my scoring, I used the Top N to rank my top outliers. 
N <- 5 #Determine N

# Order datapoints by distance from cluster centers
km.distorder <- order(distscore.km, decreasing=TRUE)
tptclm.clustdn.order <- tptclm.clustdn[km.distorder,]
tptclm.clustdn.order$rank <- rank(-tptclm.clustdn.order$SCORE.PERC)


# Q10: Plot out your distance scoring charts appropriately... (1m)
# exporting your plot as a .pdf file ... (1m)
# while also exporting any .csv files of "top ranking" outliers" (1m)
# (Total: 3m)

pdf("distancecharts.pdf") 
plot(x=tptclm.clustdn.order$SCORE.PERC,
     y=tptclm.clustdn.order$DIST.FACTOR,
     main="Outliers based on Top N",
     xlab="Distance Score Percentile",
     ylab="Distance Score",
     col=ifelse(tptclm.clustdn.order$rank<=N,"orangered","black"),
     pch=14)
dev.off() 

km.topN.outl<-data.frame(tptclm.clustdn.order[tptclm.clustdn.order$rank<=N,])

write.csv(km.topN.outl, file = "TopNOutl.csv",row.names = FALSE)
