#Networks

library(igraph)
library(WGCNA)
library(cluster)

g <-read.graph("dolphins.paj", format="pajek");
#g <- read.graph("karate.paj", format="pajek");
#g <- graph( c(1,2,2,3,3,4,5,6), directed=FALSE )
#g <- graph.lattice(c(2,1,3))
#g <- graph.full(5, loops=FALSE)

###################################################################################################
## MATRIZ DE SIMILARIDAD// DENDRO 
###################################################################################################

nodes <- vcount(g)
nedges <- ecount(g)
edges <- get.edges(g,E(g))
 
s<- matrix(data=0,nrow=nedges,ncol=nedges);

for (i in 1:nedges){ #Llamo a los links por el número que le pone get.edges
 for(l in (i+1):nedges){
   if(l>nedges) next
   comun<-intersect(edges[i,],edges[l,])
   if(length(comun)==1){
   
   ## No queda general!!
   j=edges[i, !edges[i,]%in%comun ];
   k=edges[l, !edges[l,]%in%comun ];
  
   a <- neighborhood(g,order=1, nodes=j)[[1]]
   b <- neighborhood(g,order=1, nodes=k)[[1]]
   s[i,l]= s[l,i] =length(intersect(a, b))/length(union(a,b));
  
  }else{
    if(length(comun)==0){
     s[i,l]=s[l,i]=0;
    }
  }
 }
}
diag(s)<-1


dendro <- hclust(as.dist(1-s));
#dendro <- hclust(as.dist(s),method="average");
###################################################################################################
## CLUSTERING
###################################################################################################

## Dynamic Tree Cut
cutHeight<-NULL
method<-"hybrid"
dynamic <- cutreeDynamic(dendro,distM = 1-s,minClusterSize=4,pamStage=FALSE,deepSplit=0)

dynamic = cutreeDynamic(
      dendro, cutHeight = cutHeight, minClusterSize = 4,

      # Basic tree cut options
      method = method,
      distM =1-s,
      deepSplit = (ifelse(method=="hybrid", 1, FALSE)),

      # Advanced options
      maxCoreScatter = NULL, minGap = NULL,
      maxAbsCoreScatter = NULL, minAbsGap = NULL,

      minSplitHeight = NULL, minAbsSplitHeight = NULL,

      # External (user-supplied) measure of branch split
      externalBranchSplitFnc = NULL, minExternalSplit = NULL,
      externalSplitOptions = list(),
      externalSplitFncNeedsDistance = NULL,
      assumeSimpleExternalSpecification = TRUE,

      # PAM stage options
      pamStage = TRUE, pamRespectsDendro = TRUE,
      useMedoids = FALSE, maxDistToLabel = NULL,
      maxPamDist = cutHeight,
      respectSmallClusters = TRUE,

      # Various options
      verbose = 2, indent = 0)

E(g)$color <- dynamic+1
plot(g)    

if(FALSE){
clust=cutree(dendro,h=0.02)
E(g)$color <- clust
plot(g)
}

## Densidad de Partición






###################################################################################################
## Validación
###################################################################################################

valid <- silhouette(dynamic,s)