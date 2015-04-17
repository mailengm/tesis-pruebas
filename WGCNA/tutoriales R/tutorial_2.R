# Display the current working directory
initial.dir<-getwd();
# If necessary, change the path below to the directory where the data files are stored.
# "." means current directory. On Windows use a forward slash / instead of the usual \.
workingDir = "FemaleLiver-Data/";
setwd(workingDir);
# Load the WGCNA package
library(WGCNA);
# The following setting is important, do not omit.
options(stringsAsFactors = FALSE);
#Read in the female liver data set
enableWGCNAThreads()
lnames = load(file = "FemaleLiver-01-dataInput.RData");




setwd(initial.dir);
