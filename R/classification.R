# Elastic net
#library(glmnet)
#fit <- glmnet(t(emat.rpa), as.numeric(as.factor(classInfo)), family = "multinomial")

###############################

# Nearest shrunken centroids
#library(pamr)
#results <- pamr.train(list(x = emat.rpa, y = factor(classInfo)))

###############################

# Random forest
library(randomForest)
df <- data.frame(cbind(t(emat.rpa), classInfo = factor(classInfo)))
set.seed(32453)
dfr <- df[sample(nrow(df), 500),] # random subset
rf <- randomForest(classInfo ~ ., data = dfr, importance = FALSE, proximity = FALSE)
print(rf)

