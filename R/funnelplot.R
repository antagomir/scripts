#http://stats.stackexchange.com/questions/5195/how-to-draw-funnel-plot-using-ggplot2-in-r

library(ggplot2)

set.seed(1)
p <- runif(100)
number <- sample(1:1000, 100, replace = TRUE)
p.se <- sqrt((p*(1-p)) / (number))
df <- data.frame(p, number, p.se)

## common effect (fixed effect model)
p.fem <- weighted.mean(p, 1/p.se^2)

## lower and upper limits for 95% and 99.9% CI, based on FEM estimator
number.seq <- seq(0.001, max(number), 0.1)
number.ll95 <- p.fem - 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul95 <- p.fem + 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ll999 <- p.fem - 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul999 <- p.fem + 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
dfCI <- data.frame(number.ll95, number.ul95, number.ll999, number.ul999, number.seq, p.fem)

## draw plot
fp <- ggplot(aes(x = number, y = p), data = df) +
    geom_point(shape = 1) +
    geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) +
    geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) +
    geom_line(aes(x = number.seq, y = number.ll999), linetype = "dashed", data = dfCI) +
    geom_line(aes(x = number.seq, y = number.ul999), linetype = "dashed", data = dfCI) +
    geom_hline(aes(yintercept = p.fem), data = dfCI) +
    scale_y_continuous(limits = c(0,1.1)) +
  xlab("number") + ylab("p") + theme_bw() 
fp

##################

library(ggplot2)

# Specify data
p <- dw$Nettomuutto/1000 # convert promilles to numeric percentages btw. 0...1
number <- dw$Asukasluku

# Std
p.se <- sqrt((p*(1-p)) / (number))

# Form dataframe
df <- data.frame(p, number, p.se)

## common effect (fixed effect model)
p.fem <- weighted.mean(p, 1/p.se^2)

## lower and upper limits for 95% and 99.9% CI, based on FEM estimator
number.seq <- seq(min(number), max(number), length = 100)
number.ll95 <- p.fem - 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul95 <- p.fem + 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ll999 <- p.fem - 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul999 <- p.fem + 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
dfCI <- data.frame(number.ll95, number.ul95, number.ll999, number.ul999, number.seq, p.fem)

## draw plot
fp <- ggplot(aes(x = number, y = p), data = df) + geom_point(shape = 1) 
fp <- fp + geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) 
fp <- fp + geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) 
fp <- fp + geom_line(aes(x = number.seq, y = number.ll999), linetype = "dashed", data = dfCI) 
fp <- fp + geom_line(aes(x = number.seq, y = number.ul999), linetype = "dashed", data = dfCI) 
fp <- fp + geom_hline(aes(yintercept = p.fem), data = dfCI) 
#fp <- fp + scale_y_continuous(limits = c(0,1.1)) 
fp <- fp + xlab("number") + ylab("p") + theme_bw() 
fp <- fp + scale_x_log10()
fp
