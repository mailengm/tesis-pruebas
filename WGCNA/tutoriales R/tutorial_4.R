# Load the WGCNA package
library(WGCNA);
# The following setting is important, do not omit.
options(stringsAsFactors = FALSE);

ruta="Google Drive/Tesis/tesis-pruebas/WGCNA/tutoriales R/";

#Read in the female liver data set
#enableWGCNAThreads()
lnames = load(file = paste(ruta, "FemaleLiver-Data/FemaleLiver-01-dataInput.RData", sep=""));

# Load network data saved in the second part.
lnames = load(file = paste(ruta, "FemaleLiver-Data/FemaleLiver-02-networkConstruction-auto.RData", sep=""));

annot = read.csv(file = paste(ruta, "FemaleLiver-Data/GeneAnnotation.csv", sep=""));

probes = names(datExpr)
probes2annot = match(probes, annot$substanceBXH)
