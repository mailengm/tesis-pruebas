#TO DO
#probar otros mÃ©todos en clvalid

library(fields);

#configuraciones
dim_red = 2; #los puntos en la red no son reales, son solo los lugares alrededor de los cuales se van a armar los clusters
puntos_por_cluster = 5;
parametro_de_red = 1;
ancho_del_cluster = 0.25; #lo que mide el cluster en x
alto_del_cluster = 0.25; #lo que mide el cluster en y
iteraciones_de_k = 50; #cuantas veces vamos a probar ajustar con cada k el mismo set de datos
maximas_iteraciones_de_k_means = 50; #Maximo de iteraciones que hace el kmeans si no converge
nivel_de_ruido = 0.5; #entre 0 y 1, es el porcentaje de ruido en funciÃ³n de la cantidad de puntos que haya
saltos_de_ruido = 10;
saltos_de_dispersion = 10;
cantidad_de_k_a_probar = 20; #Vamos a probar los k entre dim_red^2-cantidad_de_k_a_probar/2 y dim_red^2+cantidad_de_k_a_probar/2
guardar_datos_de_corrida = FALSE;


#limpiamos los graficos actuales
graphics.off();

#creo un directorio para guardar ahÃ­ todos los datos de la corrida
if(guardar_datos_de_corrida){
	cat("Se van a guardar los datos de la corrida\n")
	flush.console();
	script.directorio_actual <- dirname(sys.frame(1)$ofile); # trae el directorio donde corre actualmente este archivo
	script.directorio_de_archivos <- paste(script.directorio_actual, "/corridas/", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), sep="");
	dir.create(script.directorio_de_archivos);
}

#Traemos la librerÃ­a de cluster para usar silhouette y otras cosas
library(cluster);

#genero la red equiespaciada
a <- seq(1, dim_red*parametro_de_red, parametro_de_red);
red <- matrix(data=a, nrow=dim_red^2, ncol=2);
red[, 1] <- rep(a, each=dim_red);

#genero los puntos de datos alrededor de la red
puntos_en_la_red <- dim_red^2;
total_de_puntos <- puntos_en_la_red * puntos_por_cluster;

#Genero los puntos de ruido que son la misma cantidad que los puntos de la red pero no clusterizados. A medida que aumento el nivel de ruido los voy agregando
#puntos_de_ruido <- matrix(runif(total_de_puntos*nivel_de_ruido*2, parametro_de_red-ancho_del_cluster, parametro_de_red*dim_red+ancho_del_cluster), nrow=total_de_puntos*nivel_de_ruido, ncol=2);

#graficÃ³ los puntos y la red
#plot(puntos, main="Red a clusterizar", xlab="x",ylab="y");
#points(red, col="yellow");
#points(puntos_de_ruido, col="red");
#legend((dim_red*parametro_de_red-4), (dim_red*parametro_de_red-1), c("Puntos", "Centros de cluster", "Ruido"), pch=c(1,1, 1), col=c("black", "yellow", "red"))
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/red.png", sep=""), "png");#guardo el plot en un archivo ademÃ¡s de mostrarlo en pantalla




#Esto dice cuanto me tengo que correr de dim_red^2 para adelante y para atras
semi_cantidad_de_k_a_probar = cantidad_de_k_a_probar/2; 

#Probamos kmeans para varios k, desde puntos_en_la_red-10 hasta puntos_en_la_red+10, 
#nos quedamos con el de mejor promedio, iteramos varias veces y graficamos 
#un histograma para ver que k ajustÃ³ mejor la mayor cantidad de veces
mejor_k <- matrix(0, nrow=10, ncol=10); #Vamos a guardar el mejor k y el promedio que dio
mejor_p <- matrix(0, nrow=10, ncol=10); #Vamos a guardar el mejor k y el promedio que dio
promedios <- matrix(0, nrow=cantidad_de_k_a_probar, ncol=2);
media <- matrix(0,nrow=10, ncol=10);
#Vamos a calcular un tiempo estimado de corrida del programa viendo cuanto tardan 10 iteraciones y multiplicando ese tiempo por iteraciones_de_k/10
tiempo_de_inicio <- proc.time();


#Comenzamos la corrida
#cat("Comenzando corrida\nDesde ", (puntos_en_la_red - semi_cantidad_de_k_a_probar), " hasta ", (puntos_en_la_red + semi_cantidad_de_k_a_probar), "\n", sep="");
print("Comenzando corrida");
flush.console();

#Agrego una lista para guardar los valores de silhouette de cada iteración en k
lsil<-list();
data<-list();
ldispersion<-list();
#Bandera que me indica si ya mostrÃ³ el estimado de tiempo que va a demorar la corrida
ya_mostro_tiempo_estimado = FALSE


for(j in saltos_de_dispersion:1){
  lruido<-list();
  ancho_del_cluster=j/(2*saltos_de_dispersion);
  alto_del_cluster=j/(2*saltos_de_dispersion);
  #Genero los puntos de los clusters
  puntos <- matrix(0, nrow=total_de_puntos, ncol=2);
 # puntos[, 1] <- runif(total_de_puntos, -ancho_del_cluster, ancho_del_cluster) + rep(red[, 1], each=puntos_por_cluster);
  puntos[, 1] <- rnorm(total_de_puntos, sd = ancho_del_cluster) + rep(red[, 1], each=puntos_por_cluster);
  #puntos[, 2] <- runif(total_de_puntos, -alto_del_cluster, alto_del_cluster) + rep(red[, 2], each=puntos_por_cluster);
  puntos[, 2] <- rnorm(total_de_puntos, sd = alto_del_cluster) + rep(red[, 2], each=puntos_por_cluster);
  
  for(i in 1:saltos_de_ruido){
    
   
    
    
     d<-dist(puntos)
    
  #  for(k in 2:(cantidad_de_k_a_probar+1)){
      #calculo los kmeans con k puntos aleatorios, el silhouette
      #y saco un promedio de los promedios de cada cluster
      #en el silhouette como medida de cuan buena fue la clusterizaciÃ³n para ese k
      #km<-kmeans(puntos, k, iter.max = maximas_iteraciones_de_k_means, nstart = iteraciones_de_k);
      tree=hclust(d, method="average");
      clus=cutree(tree,k);
      
      if(FALSE){
      #Dynamic Tree Cut. 
      #Primero corto por lm
      lm=mean(tree$height)
      #Despues por ld
      ld=(lm+min(tree$height))*0.5
      #finalmente por lu
      lu=(lm+max(tree$height))*0.5
      
      ############################
      #quiero el plot de las alturas.fig 1 like.
      permutaciones=tree$order;
      alturas=tree$height[permutaciones] #esto no me ordena todo bien :(
      corte= lm; #?
      plot(alturas,typ="h")
      abline(h=corte,col="red")
      
      ############################
      #Para plotear los puntos
      
       plot(puntos[,1],puntos[,2])
       points(puntos[,1],puntos[,2],col=dynamic+1,pch=20) #cambiar dynamic por lo correspondiente

      }
      
      
      #data[[as.character(j,i)]]<-puntos
      #s<-dunn(d, km$cluster);
      #s<-silhouette(km$cluster,d);
  #    lsil[[as.character(k)]]<-s<-silhouette(clus,d);
  #    si<-summary(s);
  #    promedios[k-1, 1] <- si$si.summary["Mean"]; #Promedio pesado de los promedios de los silhouettes
  #    promedios[k-1, 2] <- k;  
  #  }
    
   dynamic=cutreeDynamic(dendro=tree,distM=as.matrix(d),method="hybrid",pamStage=FALSE,
                         minClusterSize=4,maxCoreScatter=0.04)#los clusters dependen mucho de estos parámetros. :(

   if(FALSE){
    plot(puntos[,1],puntos[,2],typ="n")
    #points(puntos[,1],puntos[,2],col=dynamic+1,pch=20)
    text(puntos[,1],puntos[,2],1:nrow(puntos),col=dynamic+1)
   }
   
   s<-silhouette(clus,d);
   si<-summary(s);
  
    #Meto los puntos de ruido entre los puntos de los clusters
    puntos <- rbind(puntos, matrix(runif(total_de_puntos*2*nivel_de_ruido, parametro_de_red-ancho_del_cluster, parametro_de_red*dim_red+ancho_del_cluster), ncol=2));
    
    # Usando distancias ultramétricas:
    
    
    #lruido[[i]]<-s<-silhouette(clus,d);
    #si<-summary(s);
    #media[i,j]=si$si.summary["Mean"];
    #El que dio el promedio mÃ¡s alto es el mejor k segÃºn este criterio
    #mejor_k[j,i] <- promedios[which.max(promedios[,1]), 2];
    #mejor_p[j,i] <- promedios[which.max(promedios[,1]), 1];
    #Muestro cuanto tardÃ³ en correr uno y tiro un estimado de cuanto va a demorar todo
    mejor_p[j,i]<-si$si.summary["Mean"]
 mejor_k[j,i] <- mejor_k<-nrow(table(cutreeDynamic(tree)));
  }
  
  ldispersion[[j]]<-lruido

 #El que dio el promedio mÃ¡s alto es el mejor k segÃºn este criterio
 #mejor_k[j,i] <- promedios[which.max(promedios[,1]), 2];
 #mejor_p[j,i] <- promedios[which.max(promedios[,1]), 1];
 
 #Plot de mejor promedio
 ruido<-1:saltos_de_ruido
 dispersion<-1:saltos_de_dispersion
 image.plot(ruido, dispersion,t(mejor_p))
 
 grid(nx=10, ny=10, lty=1)
 title(main = "Mejor p en funcion de ruido y dispersion", font.main = 4)
 box()
 text(expand.grid(x=ruido, y=dispersion), labels=t(mejor_k))
 
 #plot(mejor_k[, 2]);
 
 if(FALSE){
   res<-unlist(lapply(lsil,function(x){
     return(summary(x)$si.summary["Mean"])
   }))
   plot(as.numeric(names(lsil)),res)
   
   k1<-kmeans(puntos,5,16)
   
   ccolors<-rainbow(21)[k1$cluster]
   plot(puntos[,1],puntos[,2])
   points(puntos[,1],puntos[,2],col=ccolors,pch=20)
 }
 
 
 
  if(!ya_mostro_tiempo_estimado){
    tiempo_para_1_iteracion <- proc.time() - tiempo_de_inicio;
   #cat("Tiempo estimado de corrida: ", round(cantidad_de_k_a_probar * tiempo_para_1_iteracion["elapsed"]), " segundos\n", sep="")
    ya_mostro_tiempo_estimado = TRUE;
  }
  #Imprimo la iteraciÃ³n actual para saber por donde va y cuanto falta
  print(j);
  flush.console();
  
}




#Mostramos un histograma para ver cual es el k que mejor ajustÃ³
#dev.new();
#hist(mejor_k[,2], (puntos_en_la_red-10):(puntos_en_la_red+10), main="Histograma de mejores k", xlab="k",ylab="Ocurrencias")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/histograma.png", sep=""), "png");#guardo el plot en un archivo ademÃ¡s de mostrarlo en pantalla

#Mostramos los promedios para los mejores k para ver entre que valores se mueven
#dev.new();
#plot(mejor_k[,1], main="Valores de los mejores k en cada iteracion", xlab="NÂ° de iteracion", ylab="Promedio")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/promedios.png", sep=""), "png");#guardo el plot en un archivo ademÃ¡s de mostrarlo en pantalla

#Guardo los datos de la corrida en un archivo de R
if(guardar_datos_de_corrida) save(red, puntos, mejor_k, dim_red, puntos_por_cluster, parametro_de_red, ancho_del_cluster, alto_del_cluster, iteraciones_de_k, nivel_de_ruido, file = paste(script.directorio_de_archivos, "/kmeans.RData", sep=""));

#Muestro cuanto tardo la corrida y donde se guardaron los archivos de la misma
cat("La corrida demoro un estimado de:", (proc.time()["elapsed"] - tiempo_de_inicio["elapsed"]), "segundos\n");
if(guardar_datos_de_corrida) cat("Archivos de la corrida guardados en:", script.directorio_de_archivos, "\n");
