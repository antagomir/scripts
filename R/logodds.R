logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
# Odds ratio & Significance
logodds <- coef(glm(status ~ Bb, family = binomial(link = "logit"), data = df))[[2]]
odds <- exp(logodds)
# change_in_probability <- odds - 1
prob <- odds / (1 + odds)
# prob2 <- logit2prob(logodds)

