#TO DO
#probar otros métodos en clvalid

#configuraciones
dim_red = 5; #los puntos en la red no son reales, son solo los lugares alrededor de los cuales se van a armar los clusters
puntos_por_cluster = 50;
parametro_de_red = 2;
ancho_del_cluster = 0.1; #lo que mide el cluster en x
alto_del_cluster = 0.1; #lo que mide el cluster en y
iteraciones_de_k = 500; #cuantas veces vamos a probar ajustar con cada k el mismo set de datos
nivel_de_ruido = 0; #entre 0 y 1, es el porcentaje de ruido en función de la cantidad de puntos que haya
guardar_datos_de_corrida = TRUE;

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

#genero la red equiespaciada
a <- seq(1, dim_red*parametro_de_red, parametro_de_red);
red <- matrix(data=a, nrow=dim_red^2, ncol=2);
red[, 1] <- rep(a, each=dim_red);

#genero los puntos de datos alrededor de la red
puntos_en_la_red <- dim_red^2;
total_de_puntos <- puntos_en_la_red * puntos_por_cluster;

#Los puntos de ruido que vamos a tener
cantidad_de_puntos_de_ruido <- total_de_puntos * nivel_de_ruido;

#Genero los puntos de los clusters
puntos <- matrix(0, nrow=total_de_puntos, ncol=2);
puntos[, 1] <- runif(total_de_puntos, -ancho_del_cluster, ancho_del_cluster) + rep(red[, 1], each=puntos_por_cluster);
puntos[, 2] <- runif(total_de_puntos, -alto_del_cluster, alto_del_cluster) + rep(red[, 2], each=puntos_por_cluster);

#Genero los puntos de ruido
puntos_de_ruido <- matrix(runif(cantidad_de_puntos_de_ruido*2, 0, parametro_de_red*dim_red), nrow=cantidad_de_puntos_de_ruido, ncol=2);

#Meto los puntos de ruido entre los puntos de los clusters
puntos <- rbind(puntos, puntos_de_ruido);

#graficó los puntos y la red
plot(puntos, main="Red a clusterizar", xlab="x",ylab="y");
points(red, col="yellow");
points(puntos_de_ruido, col="red");
legend((dim_red*parametro_de_red-4), (dim_red*parametro_de_red-1), c("Puntos", "Centros de cluster", "Ruido"), pch=c(1,1, 1), col=c("black", "yellow", "red"))
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/red.pdf", sep=""), "pdf");#guardo el plot en un archivo además de mostrarlo en pantalla

#Probamos kmeans para varios k, desde puntos_en_la_red-10 hasta puntos_en_la_red+10, 
#nos quedamos con el de mejor promedio, iteramos varias veces y graficamos 
#un histograma para ver que k ajustó mejor la mayor cantidad de veces
mejor_k <- matrix(0, nrow=iteraciones_de_k, ncol=2); #Vamos a guardar el mejor k y el promedio que dio
promedios <- matrix(0, nrow=21, ncol=2);

#Vamos a calcular un tiempo estimado de corrida del programa viendo cuanto tardan 10 iteraciones y multiplicando ese tiempo por iteraciones_de_k/10
tiempo_de_inicio <- proc.time();

#Comenzamos la corrida
cat("Comenzando corrida\n");
flush.console();
for(i in 1:iteraciones_de_k){
	for(k in (puntos_en_la_red-10):(puntos_en_la_red+10)){
		#calculo los kmeans con k puntos aleatorios, el silhouette
		#y saco un promedio de los promedios de cada cluster
		#en el silhouette como medida de cuan buena fue la clusterización para ese k
		library(cluster);
		km<-kmeans(puntos, k);
		d<-dist(puntos);
		s<-silhouette(km$cluster, d);
		si<-summary(s);
		promedios[k-(puntos_en_la_red-11), 1] <- mean(si$clus.avg.width);
		promedios[k-(puntos_en_la_red-11), 2] <- k;	
		#dev.new();
		#plot(s);
	}
	
	#El que dio el promedio más alto es el mejor k según este criterio
	mejor_k[i,] <- promedios[which.max(promedios[,1]),];
	
	#a la 10 iteración me fijo cuanto tiempo hace que está corriendo y tiro un estimado
	if(i==10){
		tiempo_para_10_iteraciones <- proc.time() - tiempo_de_inicio;
		cat("Tiempo estimado de corrida: ", round((iteraciones_de_k * tiempo_para_10_iteraciones["elapsed"] / 10 )), " segundos\n", sep="")
	}
	#Imprimo la iteración actual para saber por donde va y cuanto falta
	print(i);
	flush.console();
}

#Mostramos un histograma para ver cual es el k que mejor ajustó
dev.new();
hist(mejor_k[,2], (puntos_en_la_red-10):(puntos_en_la_red+10), main="Histograma de mejores k", xlab="k",ylab="Ocurrencias")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/histograma.pdf", sep=""), "pdf");#guardo el plot en un archivo además de mostrarlo en pantalla

#Mostramos los promedios para los mejores k para ver entre que valores se mueven
dev.new();
plot(mejor_k[,1], main="Valores de los mejores k en cada iteracion", xlab="N° de iteracion", ylab="Promedio")
if(guardar_datos_de_corrida) savePlot(filename = paste(script.directorio_de_archivos, "/promedios.pdf", sep=""), "pdf");#guardo el plot en un archivo además de mostrarlo en pantalla

#Guardo los datos de la corrida en un archivo de R
if(guardar_datos_de_corrida) save(red, puntos, mejor_k, dim_red, puntos_por_cluster, parametro_de_red, radio_del_cluster, iteraciones_de_k, nivel_de_ruido, file = paste(script.directorio_de_archivos, "/kmeans.RData", sep=""));

#Muestro cuanto tardo la corrida y donde se guardaron los archivos de la misma
cat("La corrida demoro un estimado de:", (proc.time()["elapsed"] - tiempo_de_inicio["elapsed"]), "segundos\n");
if(guardar_datos_de_corrida) cat("Archivos de la corrida guardados en:", script.directorio_de_archivos, "\n");