library(HITChipDB)

# FTP SERVER
# jsp0812
# ssh -L 3307:128.214.222.203:3306 wageningen@128.214.222.203
params <- run.profiling.script(dbuser = "root", dbpwd = "fidipro", dbname = "phyloarray", host = '127.0.0.1', port = 3307)


# LOCAL
#params <- run.profiling.script(dbuser = "lmlahti", dbpwd = "passu", dbname = "Phyloarray")


