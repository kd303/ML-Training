# There are two type of clustering 
#   1. heirachical clustering - Agglomerative clustering
#   2. k-means clusterings 

library(readr)
csat <- read_csv("D:/z_kaushal/ISIDMBA/DataSets/CSat_Freq_Table.csv", 
                 col_types = cols(Id = col_skip()))
View(csat)

#   Essentially to reduce number of rows - group them into multiple classes
# euclidean - distance 
# can be found between two records by doing square root of( sum of square differences)

## euclidean distance is same between two records then they are grouped together

csatEclu = dist(csat, method = "euclidean")

## step 2 :once the distance is found, and groups are formed based on euclidean distances
# are found, the groups are related using different method they are called linkages
# linkage methods are single, linkages, maximumum distnace by calculating distance
# matrix 

csatClusterModel = hclust(csatEclu, method="ward")

## then look at dendogram

plot(csatClusterModel)

plot(csatClusterModel, ylim=c(0,1))

plot(as.dendrogram(csatClusterModel), ylim = c(0,150))

## finding optimal number of cluster - my guess from dendogram is 7

## How to decide the clusters are optimum
## with-in the groups variance should be minimum
## in-between the groups the variance should be maximum
## cant take or statisfy both measures above as it will will mean large number of cluster
## with one member each scenario will be taken
## with-in the group / total variations
## in-between the group / totalt variations
rect.hclust(csatClusterModel, 5 , border="cyan")

rect.hclust(csatClusterModel, 6 , border="orange")

rect.hclust(csatClusterModel, 7 , border="red")

rect.hclust(csatClusterModel, 8 , border="blue")

rect.hclust(csatClusterModel, 9 , border="brown")

rect.hclust(csatClusterModel, 10 , border="yellow")

rect.hclust(csatClusterModel, 11 , border="purple")


kmodCsat4=kmeans(csat, 4)
kmodeCsat5=kmeans(csat,5)
kmodeCsat6=kmeans(csat,6)
kmodeCsat7=kmeans(csat,7)
kmodeCsat8=kmeans(csat,8)
kmodeCsat9=kmeans(csat,9)
kmodeCsat10=kmeans(csat,10)
kmodeCsat11=kmeans(csat,11)

k = c(4,5,6,7,8,9,10,11)

BGSS = c(kmodCsat4$betweenss,kmodeCsat5$betweenss,kmodeCsat6$betweenss,kmodeCsat7$betweenss,kmodeCsat8$betweenss,kmodeCsat9$betweenss,kmodeCsat10$betweenss,kmodeCsat11$betweenss)
## 7 has the highest variation jump, so for the sake of good behaviour will keep it.

plot(k,BGSS,type="b")

clusterIds=cutree(csatClusterModel,7)

output = cbind(csat, clusterIds)
## aggregating average responses to all 4 questions how has 7 group has responded
aggregate(csat, by=list(clusterIds), mean)
## aggregating variance responses to all 4 questions how has 7 group has responded
## 5,6 & 7 seems same data. whilst 1,2,3 and 4 are ssame.
aggregate(csat, by=list(clusterIds), var)


