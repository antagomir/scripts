# Back of the envelope rule for reads vs. coverage:
# http://en.wikipedia.org/wiki/DNA_sequencing_theory

EC <- function (R) {1 - exp(-R)}

v <- seq(1, 30, 1)

pdf("coverage.pdf")

par(mfrow = c(1,2))

plot(v, EC(v), type = "b", ylab = "Expected coverage", xlab = "Amount of sequence vs. target sequence length", main = "Coverage")

plot(v, log10(1-EC(v)), type = "b", ylab = "Missed proportion of the target sequence (log10)", xlab = "Amount of sequence vs. target sequence length", main = "Depth")

dev.off()