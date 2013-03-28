#!/bin/sh

# 24.3.2013
mysql -umit  -ppassu Phyloarray_MIT   < /home/leo/data/MITChip/DumpMIT_19032013.sql
mysql -upit  -ppassu Phyloarray_PIT   < /home/leo/data/PIT1/DumpPIT1_19032013.sql
mysql -upit2 -ppassu Phyloarray_PIT2  < /home/leo/data/PIT2/DumpChickenandPIT2_19032013.sql

