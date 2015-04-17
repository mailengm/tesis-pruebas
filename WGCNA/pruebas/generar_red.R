#configuraciones
dim_red = 5;
puntos_por_cluster = 200;

#genero la red equiespaciada
a=c(1:dim_red);
red=matrix(data=a, nrow=dim_red^2, ncol=2);
red[, 1] = rep(a, each=dim_red);

#genero los puntos de datos alrededor de la red
total_de_puntos = dim_red^2*puntos_por_cluster;
puntos=matrix(data=runif(total_de_puntos*2, -0.25, 0.25), nrow=total_de_puntos, ncol=2) + rep(red, each=puntos_por_cluster);

#graficó los puntos y la red
plot(puntos);
points(red, col="red");

#calculo los kmeans y el silhouette y lo ploteo
km<-kmeans(puntos, 10);
d<-dist(puntos);
s<-silhouette(km$cluster, d);
dev.new()
plot(s);