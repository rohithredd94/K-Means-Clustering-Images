#!/usr/bin/env Rscript

library(ggplot2)
install.packages("jpeg", dependencies = TRUE,repos="http://cran.rstudio.com/")
library(jpeg)
setwd("~/MLAssign5/part3")
args = commandArgs(trailingOnly=TRUE)

genclusteredimg <- function(imgdata,k,n){
	imgdim <- dim(imgdata)

	plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
	rasterImage(imgdata,0,0,1,1)

	RGBData <- data.frame(x = rep(1:imgdim[2], each = imgdim[1]),y = rep(imgdim[1]:1, imgdim[2]),
                      R = as.vector(imgdata[,,1]), G = as.vector(imgdata[,,2]), B = as.vector(imgdata[,,3]))


	model.k <- kmeans(RGBData[,c("R","G","B")], centers = k)
	RGBk <- rgb(model.k$centers[model.k$cluster,])
	#print(testplot)
	testplot <- ggplot(data = RGBData, aes(x = x, y = y))+geom_point(colour = RGBk)+labs(title = paste("Image-",n,";Clustered Image(K=",k,")"))
	ggsave(filename = paste("ClusteredImage-",n,".jpg"), plot = testplot, width = 6.34, height = 3.72, limitsize = FALSE)
	
}

print("Image-1 START")
fd <- download.file("http://www.utdallas.edu/~axn112530/cs6375/unsupervised/images/image1.jpg","image1.jpg",mode = 'wb')
imgdata <- readJPEG("image1.jpg")
k <- 4
n <- 1
genclusteredimg(imgdata,k,n)
print("Image-1 END")

rm(list = setdiff(ls(), "genclusteredimg"))

print("Image-2 START")
fd <- download.file("http://www.utdallas.edu/~axn112530/cs6375/unsupervised/images/image2.jpg","image2.jpg",mode = 'wb')
imgdata <- readJPEG("image2.jpg")
k <- 3
n <- 2
genclusteredimg(imgdata,k,n)
print("Image-2 END")

rm(list = setdiff(ls(), "genclusteredimg"))

print("Image-5 START")
fd <- download.file("http://www.utdallas.edu/~axn112530/cs6375/unsupervised/images/image5.jpg","image5.jpg",mode = 'wb')
imgdata <- readJPEG("image5.jpg")
k <- 5
n <- 5
genclusteredimg(imgdata,k,n)
print("Image-5 END")