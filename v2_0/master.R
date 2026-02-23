# Set the working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Clear console
cat("\014")

source("database_v2.R")
source("replication.R")