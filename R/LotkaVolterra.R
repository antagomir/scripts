# http://www.r-bloggers.com/lotka-volterra-model%C2%A0%C2%A0intro/

library(deSolve)
 
LotVmod <- function (Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dx = x*(alpha - beta*y)
        dy = -y*(gamma - delta*x)
        return(list(c(dx, dy)))
    })
}
 
Pars <- c(alpha = .9, beta = 1, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 1000, by = 1)
 
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
 
#matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
#legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)

library(gridExtra)
library(ggplot2)
p1 <- ggplot(out[, -1], aes(x = y)) + geom_density()
p2 <- ggplot(out[, -1], aes(x = x)) + geom_density()
grid.arrange(p1, p2, nrow = 1)


# --------------------

library(GillespieSSA)

parms <- c(b=2, d=1, g=2.5, K=2000, c=5, a=5e-04 )
 
x0   <- c(N=1000, P=1000)
nu   <- matrix(c(+1, -1, 0,  0,
                  0,  0, 1, -1),nrow=2,byrow=T)
a    <- c("b*N", "d*N+(b-d)/K*N*N + a*P*N","c*a*P*N", "g*P")
tmax <- 100

out <- ssa(x0,a,nu,parms,tmax)

