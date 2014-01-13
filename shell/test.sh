#!/bin/sh

niter=5
nrand=20
correlationmethod="sparcc" # spearman / pearson

datafile="example/fake_data.txt"
outdir="sparccOutput"
corrdir="sparccOutput/basis_corr"
pdir="sparccOutput/pvals"
testtype='one_sided'

# Create dirs
mkdir $outdir
mkdir $corrdir
mkdir $pdir

# --------------------------------

# SparCC correlations
# i: number of iterations, see "python SparCC.py -h"
python SparCC.py $datafile -i $nrand --cor_file=$(printf "%s" "$corrdircor_sparcc.out -a $correlationmethod")

# ---------------------------------
# Pseudo p-value Calculation:
# ---------------------------------
# Calculating pseudo p-values is done via a bootstrap procedure.
# First make N shuffled (w. replacement) datasets:
#python MakeBootstraps.py $datafile -n (($niter)) -o sparccOutput/pvals/boot
foo=$(printf "%s" "$pdir/boot")
echo $foo
python MakeBootstraps.py $datafile -n $niter -o $foo 

# Next, run SparCC on each of the shuffled data sets.
# Make sure to use the exact same parameters which you used when
# running SparCC on the real data, name all the output files
# consistently, numbered sequentially, and with a '.txt' extension.
#more efficient and convenient to write a small script that
#automates this, and submits these runs as separate jobs to a
#cluster (if one is available to you. Otherwise, this may take a
#while to run on a local machine...).

for indx in $(seq 0 $(($niter-1)))
do
  fstart="$pdir/boot_"
  foo=$(printf "%s" "$fstart$indx.txt -i $nrand --cor_file=$pdir/sim_cor_$indx.txt")
  echo $foo
  python SparCC.py $foo
done

#python SparCC.py example/pvals/boot_0.txt -i 5 --cor_file=example/pvals/sim_cor_0.txt

#---------------------

# Now we have all the correlations computed from the shuffled
# datasets, we're ready to get the pseudo p-values.  Remember to
# make sure all the correlation files are in the same folder, are
# numbered sequentially, and have a '.txt' extension.  The following
# will compute both one and two sided p-values.

python PseudoPvals.py $(printf "%s" "$corrdir/cor_sparcc.out $pdir/sim_cor $niter -o $pdir/pvals_$testtype.txt -t $testtype")

#python PseudoPvals.py sparccOutput/basis_corr/cor_sparcc.out sparccOutput/pvals/sim_cor 100 -o sparccOutput/pvals/pvals_two_sided.txt -t 'two_sided'



