#http://stats.stackexchange.com/questions/8428/comparing-an-lme-to-a-lm

# This is to add to @ocram's answer because it is too long to post as a
# comment. I would treat A ~ B + C as your null model so you can assess
# the statistical significance of a D-level random intercept in a
# nested model setup. As ocram pointed out, regularity conditions are
# violated when H0:σ2=0, and the likelihood ratio test statistic (LRT)
# will not necessarily be asymptotically distributed χ2. The solution
# was I taught was to bootstrap the LRT (whose bootstrap distribution
# will likely not be χ2) parametrically and compute a bootstrap p-value
# like this:

library(lme4)
my_modelB <- lm(formula = A ~ B + C)
lme_model <- lmer(y ~ B + C + (1|D), data=my_data, REML=F)
lrt.observed <- as.numeric(2*(logLik(lme_model) - logLik(my_modelB)))
nsim <- 999
lrt.sim <- numeric(nsim)
for (i in 1:nsim) {
    y <- unlist(simulate(mymodlB))
    nullmod <- lm(y ~ B + C)
    altmod <- lmer(y ~ B + C + (1|D), data=my_data, REML=F)
    lrt.sim[i] <- as.numeric(2*(logLik(altmod) - logLik(nullmod)))
}
mean(lrt.sim > lrt.observed) #pvalue

