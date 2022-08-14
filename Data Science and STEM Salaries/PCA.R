install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)
install.packages('ggfortify', dependencies = TRUE)
library(ggfortify)



dd <- read.table("TreatedOutliers.csv",header=T, sep=";");

dd$company <- factor(dd$company)
dd$level <- factor(dd$level)
dd$title <- factor(dd$title)
dd$location <- factor(dd$location)
dd$tag <- factor(dd$tag)
dd$gender <- factor(dd$gender)
dd$Race  <- factor(dd$Race)
dd$Education  <- factor(dd$Education)
dd$bonus = as.numeric(dd$bonus)


numeriques<-which(sapply(dd,is.numeric))

dcon<-dd[,numeriques]

pc1 <- prcomp(dcon, scale=TRUE)

autoplot(pc1, data = dd, colour = 'company')
autoplot(pc1, data = dd, colour = 'Education')
autoplot(pc1, data = dd, colour = 'level')

autoplot(pc1, data = dd, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



inerProj<- pc1$sdev^2 
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner
barplot(pinerEix)

barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]


nd = 3


Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]


iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq))


eje1<-1
eje2<-2

Phi = cor(dcon,Psi)



dcat1<-c(1,3,8)
dcat3<-c(5,12,13,14)


colors<-c("blue","red","darkgreen", "purple")

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-2.5,3), ylim=c(-1.5,1),  xlab = "PC1", ylab = "PC2")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

c<-1
for(k in dcat1){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat1],pch=1,col=colors, cex=0.6)

fdic1 = tapply(Psi[,eje1],dd[,2],mean)
fdic2 = tapply(Psi[,eje2],dd[,2],mean) 

lines(fdic1,fdic2,pch=16,col="pink")
text(fdic1,fdic2,labels=levels(dd[,2]),col="pink", cex=0.6)
legend("topleft",names(dd)[2],pch=1,col="pink", cex=0.6)



plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,1.5), ylim=c(-1,0.8), xlab = "PC1", ylab = "PC2")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")


c<-1
for(k in dcat3){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],dd[,k],mean)
  fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(dd[,k]),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(dd)[dcat3],pch=1,col=colors, cex=0.6)



