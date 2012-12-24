# This example shows how to use alternative/custom CDF environments
# for Affy chips (c) Leo Lahti 2009-2010

# Instructions for creating your own custom CDFs are available at
# http://masker.nci.nih.gov/ev/instructions.html

# Ready-made custom CDFs are available at
# http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/CDF_download.asp
# This BrainArray data is the BioConductor standard and installed in
# the default version. Other customCDFs by various groups available in
# the web.

# These instructions show how to use BrainArray CDFs with your data:

#Loading required package: 
require(affy)
require(affydata)
data(Dilution) # 'Dilution' affybatch

# Option 1:
# Change the name of CDF environment directly to the affybatch object
# This assumes that the CDF environment with that name is available.
# The "HGU95Av2_Hs_ENTREZG" is readily available in BioConductor, as
# are all other BrainArray customCDFs for various chips.
# See http://brainarray.mbni.med.umich.edu/Brainarray/Database/CustomCDF/CDF_download.asp
Dilution@cdfName <- "HGU95Av2_Hs_ENTREZG" 
eset <- rma(Dilution)

# Option 2 (not run):
# Load data (CEL files) directly from folder
# and specify CDF name here:
# eset <- justRMA(celfile.path = "put/your/path/here",  cdfname = "HGU95Av2_Hs_ENTREZG")

