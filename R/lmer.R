library(lme4)
M1 <- lmer (signaali ~ (1|levy), data=df)
M2 <- lmer (signaali ~ diagnoosi + (1|levy), data=df) # levy as random effect
comp <- anova(M1, M2) # Compare models
pv <- comp[["Pr(>Chisq)"]][[2]] # Pick p-value for diagnosis
# coef(M2)$levy$diagnoosi # effect size also available