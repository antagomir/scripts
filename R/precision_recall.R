true <- df$HealthStatus == "Healthy"
predict <- df$varname > quantile(df$varname, 1 - 0.1) # Top % diversity

retrieved <- sum(predict)
precision <- sum(predict & true) / retrieved
recall <- sum(predict & true) / sum(true)
F <- 2 * precision * recall / (precision + recall)

#-------------------------------------------------------------

par(mfrow = c(2, 3))
for (k in 1:ncol(g)) {
  varname <- names(g)[[k]];
  df <- meta(pseq$all);
  df$varname <- df[, varname];
  df <- subset(df, !is.na(varname));
  res <- pROC::roc(df$HealthStatus == "Healthy", df$varname, direction = "<");
  plot(res, main = paste(varname, "AUC", round(auc(res), 3)))
}

# ----------------------------------------------------------------------------------
# --------------------------------------------------------

k <- k+1
varname <- names(g)[[2]];
df <- meta(pseq$all);
df$varname <- df[, varname];
df <- subset(df, !is.na(varname));
library(PRROC)
pr<-pr.curve(scores.class0 = df$varname[true], scores.class1 = df$varname[!true], curve = TRUE)
plot(pr, main = varname)

