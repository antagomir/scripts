# https://www.r-bloggers.com/sparql-with-r-in-less-than-5-minutes/


library(SPARQL) # SPARQL querying package
library(ggplot2)
 
# Step 1 - Set up preliminaries and define query
# Define the data.gov endpoint
endpoint <- "http://services.data.gov/sparql"
 
# create query statement
query <-
"PREFIX  dgp1187: <http://data-gov.tw.rpi.edu/vocab/p/1187/>
SELECT ?ye ?fi ?ac
WHERE {
?s dgp1187:year ?ye .
?s dgp1187:fires ?fi .
?s dgp1187:acres ?ac .
}"
 
# Step 2 - Use SPARQL package to submit query and save results to a data frame
qd <- SPARQL(endpoint,query)

# Pick the data
df <- qd$results
 
# Step 3 - Prep for graphing
 
# Numbers are usually returned as characters, so convert to numeric and create a
# variable for "average acres burned per fire"
str(df)
df <- as.data.frame(apply(df, 2, as.numeric))
str(df)
 
df$avgperfire <- df$ac/df$fi
 
# Step 4 - Plot some data
ggplot(df, aes(x=ye, y=avgperfire, group=1)) +
geom_point() +
stat_smooth() +
scale_x_continuous(breaks=seq(1960, 2008, 5)) +
xlab("Year") +
ylab("Average acres burned per fire")
 
ggplot(df, aes(x=ye, y=fi, group=1)) +
geom_point() +
stat_smooth() +
scale_x_continuous(breaks=seq(1960, 2008, 5)) +
xlab("Year") +
ylab("Number of fires")
 
ggplot(df, aes(x=ye, y=ac, group=1)) +
geom_point() +
stat_smooth() +
scale_x_continuous(breaks=seq(1960, 2008, 5)) +
xlab("Year") +
ylab("Acres burned")
 