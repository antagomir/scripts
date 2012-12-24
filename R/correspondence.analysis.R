#http://www.statmethods.net/advstats/ca.html
# Correspondence Analysis


# In the following example, A and B are categorical factors.
# also multiple ca is available.

library(ca)
mytable <- with(mydata, table(A,B)) # create a 2 way table
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
print(fit) # basic results 
summary(fit) # extended results 

plot(fit) # symmetric map

plot(fit, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

