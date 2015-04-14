library(NeatMap)

make.heatmap1(mtcars,row.method="PCA",column.method="average.linkage")
     
# is equivalent to
     
mtcars.PCA<-prcomp(mtcars)
mtcars.PCA.order<-order(apply(mtcars.PCA$x[,1:2],1,function(x){atan2(x[1],x[2])}))
mtcars.column.cluster<-hclust(as.dist(1-cor(mtcars)),method="average")
mtcars.row.cluster<-hclust(as.dist(1-cor(t(mtcars))),method="average")
heatmap1(mtcars,row.order=mtcars.PCA.order,column.order=mtcars.column.cluster$order, row.cluster=mtcars.row.cluster,column.cluster=mtcars.column.cluster)
     
# Changing Color Scheme
make.heatmap1(mtcars,row.method="PCA",column.method="average.linkage")+
scale_fill_gradient2(low="yellow",high="blue",mid="black",midpoint=200)
     
#Adding labels (the scale function ensures that labels are not clipped)
make.heatmap1(mtcars,row.method="PCA",column.method="average.linkage",
row.labels=rownames(mtcars),column.labels=colnames(mtcars))+
scale_x_continuous(lim=c(-1,15))

# ---------------------------

library(microbiome)
data(peerj32)

mydata <- t(peerj32$microbes)

# Perform nMDS embedding
nmds <- nMDS(mydata)$x

lineplot(nmds,mydata,normalize=T); #1c

make.heatmap1(mydata,row.normalize=T); #1d

make.circularmap(mydata); #1e

# -------------------------

atlas.group.colors <- rep(1:3, length = ncol(mydata))

phylogeny.info <- GetPhylogeny("HITChip", "filtered")
label.colors <- levelmap(rownames(mydata), "L2", "L1", phylogeny.info = phylogeny.info)
mtcars.nMDS<-nMDS(mydata,metric="euclidean")
cluster<-hclust(dist(mydata),method="complete")
draw.dendrogram3d(cluster,mtcars.nMDS$x,labels=rownames(mydata),label.size=0.5, label.colors = label.colors)

make.profileplot3d(mydata,column.method="nMDS", labels=colnames(mydata),label.colors=atlas.group.colors);

label.colors2 <- peerj32$meta$group
make.stereo.profileplot3d(mydata,column.method="nMDS", labels=rownames(mydata),label.colors=as.numeric(label.colors));

