require(glmnet)
source("funcs.R")
#source("loadData.R")
#emat1 <- su2h$emat
#emat2 <- su2m$emat
##y1 <- factor(as.vector(sapply(colnames(emat1), function(x){tolower(substr(x, 1,4))})))
#y1 <- factor(sapply(su2h$labs, function(x){tolower(substr(x, 1,4))}))
#y2 <- factor(sapply(su2m$labs, function(x){tolower(substr(x, 1,4))}))

#save.image("/share/work/lmlahti/tmp/tmp1.RData")
load("/share/work/lmlahti/tmp/tmp1.RData")

tmp <- homologous.expression.matrices(emat1, emat2, human2mouse)
  x1 <- tmp$x1[sample(seq(nrow(tmp$x1))),]
  x2 <- tmp$x2[sample(seq(nrow(tmp$x2))),]
  
  # Take common conditions that have enough (>2) replicates
  # in test data
  conditions <- intersect(y1, y2)
  conditions <- intersect(names(which(table(y2)>1)), conditions)

  inds1 <- which(y1 %in% conditions)
  inds2 <- which(y2 %in% conditions)

  # Take the subset of samples corresponding to the conditions
  x1 <- x1[,inds1]
  x2 <- x2[,inds2]

  # Labels
  y1 <- factor(y1[inds1])
  y2 <- factor(y2[inds2])

  # Fit model
  fit <- glmnet(t(x1), y1, family = "multinomial")

  # Check sparse (gene-specific) coefficients with a given lambda value
  # this is nrow(x1) + 1; the first element is the intercept term
  my.lambda <- 0.01
  betas <- coef(fit,s = my.lambda)

  # CV
  #set.seed(10101)
  #cvfit <- cv.glmnet(t(x1), y1, family = "multinomial", nfolds = min(10, floor(length(y1)/2)))
  #cvfit <- cv.glmnet(t(x1), y1, family = "multinomial", nfolds = 3)
  #plot(cvfit)
  #title("Multinomial fit", line = 2.5)

  # Test Prediction (this is not yet real example)
  pred <- predict(fit, newx = t(x2), type="response", s = 0.01)
  y2.pred <- apply(pred[,,1],1, function(x){names(x)[which.max(x)]})
  mean(y2.pred == y2)

