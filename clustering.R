library(cluster)
library(factoextra)

setwd("C:/R/BigDataTeamProject data")

### PAM

## data
data <- read.csv('data_scale_final.csv', header = T, row.names = 1)
View(data)
summary(data)

library(dplyr)
data <- data %>% select(-c(crime, murder, robbery, rape, theft))
View(data)

fviz_nbclust(data, pam, method = "wss")
fviz_nbclust(data, pam, method = "gap_stat") # 1
fviz_nbclust(data, pam, method = "silhouette") # 6

pam.res <- pam(data, 4)

# 1. Extract cluster medoids 

pam.res$medoids

# 2. Extract clustering vectors 
pam.res$cluster
clusplot(pam.res, main = "Cluster plot, k = 4", color = TRUE)
fviz_cluster(pam.res)
plot(silhouette(pam.res), col = 2:5) # col = color 
fviz_silhouette(silhouette(pam.res))

aggregate(data=data, .~pam.res$cluster, mean)
data_cluster <- aggregate(data=data, .~pam.res$cluster, mean)
View(data_cluster)
write.csv(data_cluster, file = 'data_cluster.csv')

### k-means

## data
data <- read.csv('data_scale_final.csv', header = T, row.names = 1)
View(data)
summary(data)

library(dplyr)
data <- data %>% select(-c(murder, robbery, rape, theft, assault))
View(data)

raw_data <- read.csv('data_final.csv', header = T, row.names = 1)
View(raw_data)
str(raw_data)

raw_data <- raw_data %>% select(-c(murder, robbery, rape, theft, assault))
View(raw_data)

install.packages('NbClust')
library(NbClust)

set.seed(1234)
nbc <- NbClust(data, method = 'kmeans', max.nc = 10) # k = 6
nbc2 <- NbClust(data, method = 'median', max.nc = 10) # k = 2

library(ggplot2)
result <- table(nbc$Best.nc[1,])
result <- as.data.frame(result)
View(result)
ggplot(result, aes(Var1, Freq)) + geom_bar(stat = 'identity') + labs(x = 'group', y = 'count')

ssw <- (nrow(data)-1)*sum(apply(data, 2, var))

for (i in 2:10){
  set.seed(1234)
  ssw[i] <- kmeans(data, centers=i)$tot.withinss
}

par(mfrow = c(1,1))
plot(1:10, ssw, type='b', xlab="군집의 수", ylab="군집 내 변동 총합: 이질성", main = 'k-means')

install.packages("LICORS")
library(LICORS)

ssw <- (nrow(data)-1)*sum(apply(data, 2, var))

for (i in 2:10){
  set.seed(1234)
  ssw[i] <- kmeanspp(data, k=i)$tot.withinss
}

lines(1:10, ssw, type = 'b', col='red')
legend(x='topright', legend=c('K-means', 'K-means++'), lty=2, col=c('black', 'red'))

# k-means++
set.seed(1234)
kpp <- kmeanspp(data, k=6)
kpp

kpp$iter

set.seed(1234)
km <- kmeans(data, centers = 6)
km$iter

kpp$size
kpp$centers

data_cluster <- aggregate(raw_data, by = list(cluster=kpp$cluster), mean)
View(data_cluster)
write.csv(data_cluster, file = 'data_cluster.csv')

pairs(raw_data, col=kpp$cluster, pch=kpp$cluster)

clusplot(data, kpp$cluster, main = "Cluster plot, k = 6", color = TRUE, shade=TRUE,
         labels=2, lines=0)
fviz_cluster(kpp, data = data)