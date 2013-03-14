library(HITChipDB)

# FTP SERVER
params <- run.profiling.script(dbuser = "root", dbpwd = "fidipro", dbname = "phyloarray", host = '127.0.0.1', port = 3307)



# LOCAL
library(HITChipDB); params <- run.profiling.script(dbuser = "mit", dbpwd = "passu", dbname = "phyloarray_mit")
#params <- run.profiling.script(dbuser = "lmlahti", dbpwd = "passu", dbname = "Phyloarray")


