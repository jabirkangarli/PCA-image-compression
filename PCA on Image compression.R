# 1. Dataset and needed packages

# install.packages("jpeg")
# install.packages("factoextra")
# install.packages("gridExtra")
# install.packages("ggplot2")
# install.packages("magick")
# install.packages("imgpalr")
library(imgpalr)
library(jpeg)
library(factoextra)
library(gridExtra)
library(ggplot2)
library(magick)

photo<-readJPEG("~\\db\\flower.jpg")
plot(1, type="n") # plotting the rasterImage - colour photo
rasterImage(photo, 0.6, 0.6, 1.4, 1.4)

# 2. Principal Component Analysis

photo.sum<-photo[,,1]+photo[,,2]+photo[,,3] # summing up RGB shades
photo.bw<-photo.sum/max(photo.sum)	        # dividing by max
plot(1, type="n")	                          # plotting the rasterImage - black & white photo
rasterImage(photo.bw, 0.6, 0.6, 1.4, 1.4)
writeJPEG(photo.bw, "photo_bw.jpg")

# PCA for pictures is to run individual PCA on each of shades

r<-photo[,,1]
# individual matrix of R color component

g<-photo[,,2]
# individual matrix of G color component

b<-photo[,,3]
# individual matrix of B color component


# PCA for each color components and merging 

r.pca<-prcomp(r, center=FALSE, scale.=FALSE)
# PCA for R color component
g.pca<-prcomp(g, center=FALSE, scale.=FALSE)
# PCA for G color component
b.pca<-prcomp(b, center=FALSE, scale.=FALSE)
# PCA for B color component

rgb.pca<-list(r.pca, g.pca, b.pca)
# merging all PCA into one object


library(gridExtra)
f1<-fviz_eig(r.pca, main="Red", barfill="red", ncp=5, addlabels=TRUE)
f2<-fviz_eig(g.pca, main="Green", barfill="green", ncp=5, addlabels=TRUE)
f3<-fviz_eig(b.pca, main="Blue", barfill="blue", ncp=5, addlabels=TRUE)
grid.arrange(f1, f2, f3, ncol=3)


vec<-seq.int(3, round(nrow(photo)), length.out=9)
for(i in vec){
  photo.pca<-sapply(rgb.pca, function(j) {
    new.RGB<-j$x[,1:i] %*% t(j$rotation[,1:i])}, simplify="array")
  assign(paste("photo_", round(i,0), sep=""), photo.pca) # saving as object
  writeJPEG(photo.pca, paste("photo_", round(i,0), "_princ_comp.jpg", sep=""))
}

# easy plotting of new photo
plot(image_read(photo_3))

# Number of PC

round(vec,0) 

# Collective plotting
par(mfrow=c(3,3)) 
par(mar=c(1,1,1,1))
plot(image_read(get(paste("photo_", round(vec[1],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[2],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[3],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[4],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[5],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[6],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[7],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[8],0), sep=""))))
plot(image_read(get(paste("photo_", round(vec[9],0), sep=""))))

library(abind)
pp=10 # how many principal components should be included
photo.pca2<-abind(r.pca$x[,1:pp] %*% t(r.pca$rotation[,1:pp]),
                  g.pca$x[,1:pp] %*% t(g.pca$rotation[,1:pp]),
                  b.pca$x[,1:pp] %*% t(b.pca$rotation[,1:pp]),
                  along=3)
plot(image_read(photo.pca2))

# Table of results
#install.packages("Metrics")
library(Metrics)

sizes<-matrix(0, nrow=9, ncol=4)
colnames(sizes)<-c("Number of PC", "Photo size", "Compression ratio", "MSE-Mean Squared Error")
sizes[,1]<-round(vec,0)
for(i in 1:9) {
  path<-paste("photo_", round(vec[i],0), "_princ_comp.jpg", sep="")
  sizes[i,2]<-file.info(path)$size 
  photo_mse<-readJPEG(path)
  sizes[i,4]<-mse(photo, photo_mse) # from Metrics::
}
sizes[,3]<-round(as.numeric(sizes[,2])/as.numeric(sizes[9,2]),3)
sizes

