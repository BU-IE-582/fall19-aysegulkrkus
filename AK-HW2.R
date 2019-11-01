library(data.table)
require(arules)
library(dplyr)
library(tidyverse)
library(ggbiplot)
library(ggplot2)
library(MASS)
library(jpeg)
library(imager)
library(magick)

##################TASK 1#########################

musk=fread("Musk1.csv",stringsAsFactors = FALSE)

########PCA analysis
musk_pca <- prcomp(musk[,c(3:168)], center = TRUE, scale = TRUE)
summary(musk_pca)

plot(musk_pca$x, col=c('red', 'blue')[as.factor(musk$V1)])

#to see variances of the PCA's
std_dev <- musk_pca$sdev
musk_var <- std_dev^2
musk_varex<-musk_var/sum(musk_var)
plot(musk_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")

plot(cumsum(musk_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
plot(musk_pca, type="l", ylim=c(0,40))


ggbiplot(musk_pca, labels=rownames(musk), var.axes=FALSE, ellipse = TRUE)
musk_2<-cbind(musk, musk_pca$x[,1:40])
ggplot(musk_2, col=c('red', 'blue')[as.factor(musk$V1)], aes(PC1, PC2, col=V2, fill=V1))+stat_ellipse(geom="polygon", col="black", alpha=0.3)+geom_point(shape=21, col="black")


######MDS part 
musk_MDS<-musk[,c(3:168)]

par(mfrow=c(3,2))
dis_musk<-dist(musk_MDS, method="manhattan")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Manhattan")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Manhattan")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")

dis_musk<-dist(musk_MDS, method="euclidian")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Euclidian")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Euclidian")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")

dis_musk<-dist(musk_MDS, method="Minkowski")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Minkowski")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Minkowski")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")

#For the second part, the 166 features of 92 bags are averaged and the same procedure is repeated
#################################################
musk_aggregated<- aggregate(musk[,2:168],by=list(musk$V2),FUN=mean)
musk_type<- aggregate(musk[,1],by=list(musk$V2),FUN=max)
new_musk<-cbind(musk_type,musk_aggregated[,3:168])

########PCA analysis
musk_pca <- prcomp(new_musk[,c(3:168)], center = TRUE, scale = TRUE)
musk_pca
summary(musk_pca)

plot(musk_pca$x,  col=c('red', 'blue')[as.factor(new_musk$V1)], main = "Bags Removed")

#to see variances of the PCA's
std_dev <- musk_pca$sdev
musk_var <- std_dev^2
musk_varex<-musk_var/sum(musk_var)
plot(musk_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Bags Removed")

plot(cumsum(musk_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b",
     main = "Bags Removed")

plot(musk_pca, type="l", ylim=c(0,40),main = "Bags Removed")


#Plotting of PCA reducing dimensions
pc.use <- 40 # explains 96% of variance
musk_pca_40pca <- musk_pca$x[,1:pc.use]
plot(musk_pca_40pca)

ggbiplot(musk_pca, labels=rownames(musk), ellipse = TRUE)


musk_2<-cbind(musk, musk_pca$x[,1:40])
ggplot(musk_2, col=c('red', 'blue')[as.factor(musk$V1)], aes(PC1, PC2, col=V2, fill=V1))+stat_ellipse(geom="polygon", col="black", alpha=0.3)+geom_point(shape=21, col="black")


######MDS part 
musk_MDS<-new_musk[,c(3:168)]
dis_musk<-dist(musk_MDS, method="manhattan")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Manhattan")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Manhattan")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")

dis_musk<-dist(musk_MDS, method="euclidian")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Euclidian")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Euclidian")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")

dis_musk<-dist(musk_MDS, method="minkowski")
mds1<-isoMDS(dis_musk,k=40)
plot(mds1$points,col=c('red', 'blue')[as.factor(musk$V1)], main = "Minkowski")
mds_shepard <- Shepard(dis_musk, mds1$points)
plot(mds_shepard, col="blue", pch = ".", main = "Minkowski")
lines(mds_shepard$x, mds_shepard$yf, type = "S", col="red")






################Task2

anime <- readJPEG("anime256.jpg", native = FALSE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(anime,0,0,1,1)


noisy_anime<-jitter(anime,amount = 0.1)
noisy_anime[which(noisy_anime<0)]=0
noisy_anime[which(noisy_anime>1)]=1
par(mfrow=c(1,1))
plot(c(0,1),c(0,1),type="n",main="Anime with Noise",xaxt="n", yaxt="n",ann=FALSE)
rasterImage(noisy_anime,0,0,1,1)

#for a single plot of red, green and blue colors
par(mfrow=c(1,3))
plot(c(0,1),c(0,1),type="n",main="Red",axes=FALSE ,xlab="",ylab="")
rasterImage(noisy_anime[,,1],0,0,1,1)
plot(c(0,1),c(0,1),type="n",main="Green",axes=FALSE,xlab="",ylab="")
rasterImage(noisy_anime[,,2],0,0,1,1)
plot(c(0,1),c(0,1),type="n",main="Blue", axes=FALSE, xlab="",ylab="")
rasterImage(noisy_anime[,,3],0,0,1,1)

#Channelling through image function
par(mfrow=c(1,3))
image(noisy_anime[,,1], main="Red")
image(noisy_anime[,,2], main="Green")
image(noisy_anime[,,3], main="Blue")

#####turning the RGB plot to grayscale using graphic editor and reading 256x256 p
gray_noisy_anime <- readJPEG("gray_noise_256p.jpg", native = FALSE)

###Since this is an gray image, all R=G=B valuesi hence we only gathered 1.
gray_noisy_anime <-gray_noisy_anime[,,1]
par(mfrow=c(1,1))
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(gray_noisy_anime ,0,0,1,1)


#for a 3x3 matrix first patch the first matrices center is 2,2
a <- array(numeric(),c(254*254,9))

for(j in 1:254){
        for(k in 1:254){
                b=gray_noisy_anime[k:(k+2),j:(j+2)]
                for(i in 1:3){
                        a[(254*(j-1))+k,3*i-2]=b[i,1]
                        a[(254*(j-1))+k,3*i-1]=b[i,2]
                        a[(254*(j-1))+k,3*i]=b[i,3]
                        
                }
        }
}


patches_pr <- prcomp(a)
patches_pr
summary(patches_pr)
plot(patches_pr$x)



par(mfrow=c(1,1))
plot(c(0,200),c(0,200),type="n",xlab="",ylab="")
Comp1<-matrix(patches_pr$x[,1],254,254)
image(1:254, 1:254, Comp1)
title(main="Component 1")


par(mfrow=c(1,1))
plot(c(0,200),c(0,200),type="n",xlab="",ylab="")
Comp2<-matrix(patches_pr$x[,2],254,254)
image(1:254, 1:254, Comp1)
title(main="Component 2")

par(mfrow=c(1,1))
plot(c(0,200),c(0,200),type="n",xlab="",ylab="")
Comp3<-matrix(patches_pr$x[,3],254,254)
image(1:254, 1:254, Comp3)
title(main="Component 3")

par(mfrow=c(1,1))
plot(c(0,200),c(0,200),type="n",xlab="",ylab="")
Comp1<-matrix(patches_pr$rotation[,1],3,3)
image(1:3, 1:3, Comp1)
title(main="Component 1")






