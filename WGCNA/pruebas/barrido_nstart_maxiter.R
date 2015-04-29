#TO DO
#probar otros métodos en clvalid

#configuraciones
dim_red = 4; #los puntos en la red no son reales, son solo los lugares alrededor de los cuales se van a armar los clusters
puntos_por_cluster = 50;
parametro_de_red = 1;
ancho_del_cluster = 0.25; #lo que mide el cluster en x
alto_del_cluster = 0.25; #lo que mide el cluster en y
iteraciones_de_k = 50; #cuantas veces vamos a probar ajustar con cada k el mismo set de datos
maximas_iteraciones_de_k_means = 50; #Maximo de iteraciones que hace el kmeans si no converge
nivel_de_ruido = 0.2; #entre 0 y 1, es el porcentaje de ruido en función de la cantidad de puntos que haya
saltos_de_ruido = 0.1;
cantidad_de_k_a_probar = 10; #Vamos a probar los k entre dim_red^2-cantidad_de_k_a_probar/2 y dim_red^2+cantidad_de_k_a_probar/2
guardar_datos_de_corrida = FALSE;

#limpiamos los graficos actuales
graphics.off();

#creo un directorio para guardar ahí todos los datos de la corrida
if(guardar_datos_de_corrida){
	cat("Se van a guardar los datos de la corrida\n")
	flush.console();
	script.directorio_actual <- dirname(sys.frame(1)$ofile); # trae el directorio donde corre actualmente este archivo
	script.directorio_de_archivos <- paste(script.directorio_actual, "/corridas/", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), sep="");
	dir.create(script.directorio_de_archivos);
}

#Traemos la librería de cluster para usar silhouette y otras cosas
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

#graficó los puntos y la red
#plot(puntos, main="Red a clusterizar", xlab="x",ylab="y");
points(red, col="yellow");
#points(puntos_de_ruido, col="red");
#legend((dim_red*parametro_de_red-4), (dim_red*parametro_de_red-1), c("Puntos", "Centros de cluster", "Ruido"), pch=c(1,1, 1), col=c("black", "yellow", "red"))
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/red.png", sep=""), "png");#guardo el plot en un archivo además de mostrarlo en pantalla




#Esto dice cuanto me tengo que correr de dim_red^2 para adelante y para atras
semi_cantidad_de_k_a_probar = cantidad_de_k_a_probar/2; 

#Probamos kmeans para varios k, desde puntos_en_la_red-10 hasta puntos_en_la_red+10, 
#nos quedamos con el de mejor promedio, iteramos varias veces y graficamos 
#un histograma para ver que k ajustó mejor la mayor cantidad de veces
mejor_k <- matrix(0, nrow=10, ncol=10); #Vamos a guardar el mejor k y el promedio que dio
mejor_p <- matrix(0, nrow=10, ncol=10); #Vamos a guardar el mejor k y el promedio que dio
promedios <- matrix(0, nrow=(cantidad_de_k_a_probar + 1), ncol=2);

#Vamos a calcular un tiempo estimado de corrida del programa viendo cuanto tardan 10 iteraciones y multiplicando ese tiempo por iteraciones_de_k/10
tiempo_de_inicio <- proc.time();


#Comenzamos la corrida
#cat("Comenzando corrida\nDesde ", (puntos_en_la_red - semi_cantidad_de_k_a_probar), " hasta ", (puntos_en_la_red + semi_cantidad_de_k_a_probar), "\n", sep="");
print("Comenzando corrida");
flush.console();

#Bandera que me indica si ya mostró el estimado de tiempo que va a demorar la corrida
ya_mostro_tiempo_estimado = FALSE
for(j in 1:10){
  ancho_del_cluster=j/10;
  alto_del_cluster=j/10;
  #Genero los puntos de los clusters
  puntos <- matrix(0, nrow=total_de_puntos, ncol=2);
  puntos[, 1] <- runif(total_de_puntos, -ancho_del_cluster, ancho_del_cluster) + rep(red[, 1], each=puntos_por_cluster);
  puntos[, 2] <- runif(total_de_puntos, -alto_del_cluster, alto_del_cluster) + rep(red[, 2], each=puntos_por_cluster);
  for(i in seq(from = 1, to = 10, by = 1)){
    
    
    for(k in (puntos_en_la_red - semi_cantidad_de_k_a_probar):(puntos_en_la_red + semi_cantidad_de_k_a_probar)){
      #calculo los kmeans con k puntos aleatorios, el silhouette
      #y saco un promedio de los promedios de cada cluster
      #en el silhouette como medida de cuan buena fue la clusterización para ese k
      km<-kmeans(puntos, k, iter.max = maximas_iteraciones_de_k_means, nstart = iteraciones_de_k);
      d<-dist(puntos);
      #s<-dunn(d, km$cluster);
      s<-silhouette(km$cluster,d);
      si<-summary(s);
      promedios[k-(puntos_en_la_red - semi_cantidad_de_k_a_probar - 1), 1] <- si$si.summary["Mean"]; #Promedio pesado de los promedios de los silhouettes
      promedios[k-(puntos_en_la_red - semi_cantidad_de_k_a_probar - 1), 2] <- k;	
    }
    #El que dio el promedio más alto es el mejor k según este criterio
    mejor_k[j,i] <- promedios[which.max(promedios[,1]), 2];
    mejor_p[j,i] <- promedios[which.max(promedios[,1]), 1];
    #Muestro cuanto tardó en correr uno y tiro un estimado de cuanto va a demorar todo

    #Meto los puntos de ruido entre los puntos de los clusters
    puntos <- rbind(puntos, matrix(runif(80*2, parametro_de_red-ancho_del_cluster, parametro_de_red*dim_red+ancho_del_cluster), nrow=80, ncol=2));

  }
  
  if(!ya_mostro_tiempo_estimado){
    tiempo_para_1_iteracion <- proc.time() - tiempo_de_inicio;
    cat("Tiempo estimado de corrida: ", round(cantidad_de_k_a_probar * tiempo_para_1_iteracion["elapsed"]), " segundos\n", sep="")
    ya_mostro_tiempo_estimado = TRUE;
  }
  #Imprimo la iteración actual para saber por donde va y cuanto falta
  print(j);
  flush.console();
  
}

#Plot de mejor promedio
ruido<-1:10
dispersion<-1:10
image.plot(ruido, dispersion,t(mejor_p))
grid(nx=10, ny=10, lty=1)
title(main = "Mejor p en funcion de ruido y dispersion", font.main = 4)
box()
text(expand.grid(x=ruido, y=dispersion), labels=t(mejor_k))

#plot(mejor_k[, 2]);

#Mostramos un histograma para ver cual es el k que mejor ajustó
#dev.new();
#hist(mejor_k[,2], (puntos_en_la_red-10):(puntos_en_la_red+10), main="Histograma de mejores k", xlab="k",ylab="Ocurrencias")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/histograma.png", sep=""), "png");#guardo el plot en un archivo además de mostrarlo en pantalla

#Mostramos los promedios para los mejores k para ver entre que valores se mueven
#dev.new();
#plot(mejor_k[,1], main="Valores de los mejores k en cada iteracion", xlab="N° de iteracion", ylab="Promedio")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/promedios.png", sep=""), "png");#guardo el plot en un archivo además de mostrarlo en pantalla

#Guardo los datos de la corrida en un archivo de R
if(guardar_datos_de_corrida) save(red, puntos, mejor_k, dim_red, puntos_por_cluster, parametro_de_red, ancho_del_cluster, alto_del_cluster, iteraciones_de_k, nivel_de_ruido, file = paste(script.directorio_de_archivos, "/kmeans.RData", sep=""));

#Muestro cuanto tardo la corrida y donde se guardaron los archivos de la misma
cat("La corrida demoro un estimado de:", (proc.time()["elapsed"] - tiempo_de_inicio["elapsed"]), "segundos\n");
if(guardar_datos_de_corrida) cat("Archivos de la corrida guardados en:", script.directorio_de_archivos, "\n");