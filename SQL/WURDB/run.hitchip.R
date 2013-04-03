# DB installation scripts:
#~/scripts/SQL/WURDB$ less install.dump.sh
#fs <- list.files("~/Rpackages/HITChipDB/R/", full.names = T); for (f in fs) {source(f)}
#fs <- list.files("~/Rpackages/microbiome/R/", full.names = T); for (f in fs) {source(f)}

# LOCAL

# ChickChip OK 2.4.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "pit", dbpwd = "passu", dbname = "chickchipdb")

# MITChip OK 28.3.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "mit", dbpwd = "passu", dbname = "phyloarray_mit")

# PITChip OK 28.3.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "pit", dbpwd = "passu", dbname = "pitchipdb")

# PITChip2 OK 28.3.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "pit", dbpwd = "passu", dbname = "phyloarray_pit")

# OK 28.3.2013 
# FTP SERVER
# ssh -L 3307:128.214.222.203:3306 wageningen@128.214.222.203 # + salas.
library(HITChipDB)
params <- run.profiling.script(dbuser = "root", dbpwd = "fidipro", dbname = "phyloarray", host = '127.0.0.1', port = 3307)







