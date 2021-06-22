## data pre-processing part ##

#load dataset
library(readxl)
data_g2 <- read_excel("C:/Users/freak/OneDrive/Desktop/DSE/5 Machine learning, statistical learning, deep learning and artificial intelligence/b SL, DL & AI - Salini/3 group assignments/2 unsupervised learning report/data_g2.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric"))
#set country column as rownames
library(dplyr)
rownames(data_g2) <- data_g2$country

#impute mean into missing values
install.packages("imputeTS")
library(imputeTS)
data_g2 <- na_mean(data_g2)

#inspect data
summary(data_g2)

#start standarsising#
#to standardise WBLI
hist(data_g2$WBLI)
summary(data_g2$WBLI)
m <-mean(data_g2$WBLI,na.rm = TRUE)
v <- var(data_g2$WBLI,na.rm = TRUE)
WBLI.s <- (data_g2$WBLI - m)/sqrt(v)
mean(WBLI.s,na.rm = TRUE)  
var(WBLI.s,na.rm = TRUE)
hist(WBLI.s)

#to standardise Wparliaments
hist(data_g2$Wparliaments)
summary(data_g2$Wparliaments)
m <-mean(data_g2$Wparliaments,na.rm = TRUE)
v <- var(data_g2$Wparliaments,na.rm = TRUE)
Wparliaments.s <- (data_g2$Wparliaments - m)/sqrt(v)
mean(Wparliaments.s,na.rm = TRUE)  
var(Wparliaments.s,na.rm = TRUE)
hist(Wparliaments.s)

#to standardise femaleSM
hist(data_g2$femaleSM)
summary(data_g2$femaleSM)
m <-mean(data_g2$femaleSM,na.rm = TRUE)
v <- var(data_g2$femaleSM,na.rm = TRUE)
femaleSM.s <- (data_g2$femaleSM - m)/sqrt(v)
mean(femaleSM.s,na.rm = TRUE)  
var(femaleSM.s,na.rm = TRUE)
hist(femaleSM.s)

#to standardise femaleF
hist(data_g2$femaleF)
summary(data_g2$femaleF)
m <-mean(data_g2$femaleF,na.rm = TRUE)
v <- var(data_g2$femaleF,na.rm = TRUE)
femaleF.s <- (data_g2$femaleF - m)/sqrt(v) # standardization
mean(femaleF.s,na.rm = TRUE)  
var(femaleF.s,na.rm = TRUE)
hist(femaleF.s)

#to standardise femaleWS
hist(data_g2$femaleWS)
summary(data_g2$femaleWS)
m <-mean(data_g2$femaleWS,na.rm = TRUE)
v <- var(data_g2$femaleWS,na.rm = TRUE)
femaleWS.s <- (data_g2$femaleWS - m)/sqrt(v) # standardization
mean(femaleWS.s,na.rm = TRUE)  
var(femaleWS.s,na.rm = TRUE)
hist(femaleWS.s)

#to standardise femaleMO
hist(data_g2$femaleMO)
summary(data_g2$femaleMO)
m <-mean(data_g2$femaleMO,na.rm = TRUE)
v <- var(data_g2$femaleMO,na.rm = TRUE)
femaleMO.s <- (data_g2$femaleMO - m)/sqrt(v) # standardization
mean(femaleMO.s,na.rm = TRUE)  
var(femaleMO.s,na.rm = TRUE)
hist(femaleMO.s)

#to standardise maleWS
hist(data_g2$maleWS)
summary(data_g2$maleWS)
m <-mean(data_g2$maleWS,na.rm = TRUE)
v <- var(data_g2$maleWS,na.rm = TRUE)
maleWS.s <- (data_g2$maleWS - m)/sqrt(v) # standardization
mean(maleWS.s,na.rm = TRUE)  
var(maleWS.s,na.rm = TRUE)
hist(maleWS.s)

#to standardise maleAO
hist(data_g2$maleAO)
summary(data_g2$maleAO)
m <-mean(data_g2$maleAO,na.rm = TRUE)
v <- var(data_g2$maleAO,na.rm = TRUE)
maleAO.s <- (data_g2$maleAO - m)/sqrt(v) # standardization
mean(maleAO.s,na.rm = TRUE)  
var(maleAO.s,na.rm = TRUE)
hist(maleAO.s)

#to standardise maleLE
hist(data_g2$maleLE)
summary(data_g2$maleLE)
m <-mean(data_g2$maleLE,na.rm = TRUE)
v <- var(data_g2$maleLE,na.rm = TRUE)
maleLE.s <- (data_g2$maleLE - m)/sqrt(v) # standardization
mean(maleLE.s,na.rm = TRUE)  
var(maleLE.s,na.rm = TRUE)
hist(maleLE.s)

#to standarise femaleLE
hist(data_g2$femaleLE)
summary(data_g2$femaleLE)
m <-mean(data_g2$femaleLE,na.rm = TRUE)
v <- var(data_g2$femaleLE,na.rm = TRUE)
femaleLE.s <- (data_g2$femaleLE - m)/sqrt(v) # standardization
mean(femaleLE.s,na.rm = TRUE)  
var(femaleLE.s,na.rm = TRUE)
hist(femaleLE.s)

#to standardise married18
hist(data_g2$married18)
summary(data_g2$married18)
m <-mean(data_g2$married18,na.rm = TRUE)
v <- var(data_g2$married18,na.rm = TRUE)
married18.s <- (data_g2$married18 - m)/sqrt(v) # standardization
mean(married18.s,na.rm = TRUE)  
var(married18.s,na.rm = TRUE)
hist(married18.s)


data.stand <- scale(data_g2[-1])  # To standarize the variables
data.stand <- as.data.frame(data.stand)
rownames(data.stand) <- data_g2$country

## finish standardisation ##

## modeling part ##
#k-means clustering part

#plot scatter plot WBLI-Wparliaments ############## MAYBE NOT NECESSARY
library(ggplot2)
ggplot(data.stand, aes(x=Wparliaments.s, y=WBLI.s)) +
  geom_point()

#choose the number of clusters (k)
# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data.stand, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

##checked##

#clustering
# Fitting K-Means to the dataset
set.seed(30)
kmeans = kmeans(x = data.stand, centers = 3)
y_kmeans = kmeans$cluster
kmeans$cluster

# Visualising the k-means clustering
library(cluster)
clusplot(data.stand,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         col.txt = "deepskyblue4",
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Countries')
)

#hierarchical clustering, k=3
library(proxy)
simil(data.stand, method="Gower")
dendro_plot <- hclust(dist(data.stand, method="Gower"), method = "complete")
plot(dendro_plot, cex = 0.5)
groups <- cutree(dendro_plot, k=3)
groups
rect.hclust(dendro_plot, k=3, border= c("red", "green", "blue"))

#pull country names and respective group
as.data.frame(cbind(groups))

write.csv(as.data.frame(cbind(groups)), file = "group_cluster.csv")
