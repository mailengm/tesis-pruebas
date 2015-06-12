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

for (i in 1:(nedges-1)){ #Llamo a los links por el número que le pone get.edges
 for(l in 1:(nedges-1)){
keystone <-intersect(edges[i,],edges[l,])/1

 if(length(keystone)){
 ## No queda general!!
  j=edges[i,2];
  k=edges[l,2];
  
  a <- neighborhood(g,order=1, nodes=j)[[1]]
  b <- neighborhood(g,order=1, nodes=k)[[1]]
  s[i,l]= length(intersect(a, b))/length(union(a,b));
  
  } else {
         s[i,l]=0;
  }
  s[i,i]=1;
  }
}

dendro <- hclust(as.dist(s));
#dendro <- hclust(as.dist(s),method="average");
###################################################################################################
## CLUSTERING
###################################################################################################

## Dynamic Tree Cut

dynamic = cutreeDynamic(
      dendro, cutHeight = NULL, minClusterSize = 4,

      # Basic tree cut options
      method = "hybrid"
      distM =s,
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
E(g)$color <- dynamic
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
