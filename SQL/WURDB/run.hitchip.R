library(HITChipDB)

# FTP SERVER
params <- run.profiling.script(dbuser = "root", dbpwd = "fidipro", dbname = "phyloarray", host = '127.0.0.1', port = 3307)


# LOCAL

# MITChip OK 14.3.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "mit", dbpwd = "passu", dbname = "phyloarray_mit")

# PITChip OK 14.3.2013
library(HITChipDB); params <- run.profiling.script(dbuser = "pit", dbpwd = "passu", dbname = "pitchipdb")



