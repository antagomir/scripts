drops <- c(4944, 4961, 4979, 4997, 5017, 5035, 5054, 5071, 5094, 5114, 5131)

# Original
origin <- c(5597, 5421)
bottle <- c(1430, 1730)
bottle.total <- 4300   # 4200-4400
bottle.content <- 2500 # 2400-2600

fill  <- max(drops) - min(drops)
out <- diff(origin)
error <- c((abs(out) - fill)/fill, (abs(out) - fill)/abs(out))
print(paste("Fill:", fill, "/ Out:", out, "Error:", error))

hist(diff(drops), main=paste("Mean: ", round(mean(diff(drops)), 1)), 25)
print(range(diff(drops)))

# Alc: 820 -> 5947
# New d: 20

5947 - max(drops)