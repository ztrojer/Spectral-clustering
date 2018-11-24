library(jpeg)
library(mlbench)
library(fields)
library(stats)

rawimg=readJPEG('roza.jpg')
rawimg=t(rawimg)
rawimg=rawimg[,ncol(rawimg):1]
image(rawimg,col = grey((0:8)/8))

smoothimg=image.smooth(rawimg,theta=2)

olddim=dim(rawimg)
newdim=c(round(olddim/10))
prod(newdim)>2^31
img=matrix(NA,newdim[1],newdim[2])
for (r in 1:newdim[1]) {
  centerx=(r-1)/newdim[1]*olddim[1]+1
  lowerx=max(1,round(centerx-olddim[1]/newdim[1]/2,0))
  upperx=min(olddim[1],round(centerx+olddim[1]/newdim[1]/2,0))
  for (c in 1:newdim[2]) {
    centery=(c-1)/newdim[2]*olddim[2]+1
    lowery=max(1,round(centery-olddim[2]/newdim[2]/2,0))
    uppery=min(olddim[2],round(centery+olddim[2]/newdim[2]/2,0))
    img[r,c]=mean(smoothimg$z[lowerx:upperx,lowery:uppery])
  }
}
image(img,col = grey((0:8)/8))

imgvec=matrix(NA,prod(dim(img)),3)
counter=1
for (r in 1:nrow(img)) {
  for (c in 1:ncol(img)) {
    imgvec[counter,1]=r
    imgvec[counter,2]=c
    imgvec[counter,3]=img[r,c]
    
    counter=counter+1
  }
}

podatki <- imgvec[,1:2]

epsilon = 2
sigma= 0.01 #var(imgvec[,3])**2
simetricna=matrix(0,nrow(imgvec),nrow(imgvec))
for(r in 1:nrow(imgvec)) {
  simetricna[r,]=ifelse(abs(imgvec[r,1]-imgvec[,1])<=epsilon & abs(imgvec[r,2]-imgvec[,2])<=epsilon,exp(-(imgvec[r,3]-imgvec[,3])^2/sigma),0)
}

  
D= diag(rowSums(simetricna))
U = D - simetricna

k   <- 3
lastne_vrednosti <- eigen(U, symmetric = TRUE)
Z   <- lastne_vrednosti$vectors[,(ncol(lastne_vrednosti$vectors)-k+1):ncol(lastne_vrednosti$vectors)]
  
km <- kmeans(Z, centers=k)
plot(podatki, col=km$cluster)

