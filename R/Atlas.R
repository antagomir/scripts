# Load Dryad tools
library("rdryad") # Use the install.packages("rdryad") if package not available

# Define the data URL
url <- download_url('10255/dryad.64665')

# Download the data
data <- read.table(url, sep = "\t", row.names = 1, header = TRUE)

# Fix some broken names from the original release..
# ie. replace 'Clostridium..sensu.stricto.les' with 'Clostridiales'
colnames(data) <- gsub("\\.", " ", colnames(data))
#colnames(data) <- gsub("at rel", "et rel", colnames(data))
colnames(data) <- gsub("rel $", "rel.", colnames(data))
colnames(data) <- gsub("Clostridium  sensu stricto ", "Clostridiales", colnames(data))
colnames(data) <- gsub("Clostridialesles", "Clostridiales", colnames(data))

# Convert to matrix 
data <- as.matrix(data)

# ---------------

url <- download_url('10255/dryad.64666')
meta <- read.table(url, sep = "\t", row.names = 1, header = TRUE)

# Add SampleIDs as a separate column from rownames
meta$SampleID <- rownames(meta)

# Order BMI groups in correct order
# (see README at http://datadryad.org/resource/doi:10.5061/dryad.pk75d for details)
meta$BMI_group <- factor(meta$BMI_group, levels = c("underweight", "lean", "overweight", "obese", "severeobese", "morbidobese"))
meta$SubjectID <- factor(meta$SubjectID)

