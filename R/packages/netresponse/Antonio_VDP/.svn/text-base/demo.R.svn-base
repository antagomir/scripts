Ns <- 300
Nf <- 2
d1 <- t(sapply(1:(Ns/2),function(x){rnorm(Nf,mean=rep(c(0,0),Nf))}))
d2 <- t(sapply(1:(Ns/2),function(x){rnorm(Nf,mean=rep(c(-5,-5),Nf))}))
d3 <- t(sapply(1:(Ns/2),function(x){rnorm(Nf,mean=rep(c(5,-8),Nf))}))
d4 <- t(sapply(1:(Ns/2),function(x){rnorm(Nf,mean=rep(0,Nf))}))
d4[,1] = -4 + d4[,1]*2; # 2 = standard deviation
d4[,2] = 4 + d4[,2]*0.5;
d5 <- t(sapply(1:(Ns/2),function(x){rnorm(Nf,mean=rep(0,Nf))}))
d5[,1] = 6 + d5[,1]*0.5;
d5[,2] = 2 + d5[,2]*2;

D <- rbind(d1,d2,d3,d4,d5)
out <- vdp_mixt(D)

print('--- Mu_bar ---');
print(out$hp_posterior$Mu_bar);
print('--- Mu_tilde ---');
print(out$hp_posterior$Mu_tilde);
print('--- Ksi_beta ---');
print(out$hp_posterior$Ksi_beta);
print('--- Ksi_alpha ---');
print(out$hp_posterior$Ksi_alpha);
print('--- Variances ---');
varsprint <- out$hp_posterior$Ksi_beta/out$hp_posterior$Ksi_alpha;  # Cluster variances
print(varsprint);
print('--- True Variances ---');
print(c(var(d1[,1]),var(d1[,2])));
print(c(var(d2[,1]),var(d2[,2])));
print(c(var(d3[,1]),var(d3[,2])));
print(c(var(d4[,1]),var(d4[,2])));
print(c(var(d5[,1]),var(d5[,2])));
print('--- gamma ---');
print(out$hp_posterior$gamma);
print('--- Nc ---');
print(out$hp_posterior$Nc);
print('--- post gamma ---');
auxSum <- sum(out$hp_posterior$Nc);
print(auxSum)


cluster_assignments=apply(out$hp_posterior$q_of_z, 1, which.max);

K = dim(out$hp_posterior$q_of_z)[2] -1;

plot(D,xlim=c(min(D[,1]),max(D[,1])),ylim=c(min(D[,2]),max(D[,2])), pch = 20);

#I = which(cluster_assignments == 1);
#plot(D[I,],xlim=c(min(D[,1]),max(D[,1])),ylim=c(min(D[,2]),max(D[,2])));

#if(K>1){
#  for ( i in 2:K){
#    I = which(cluster_assignments == i);
#    points(D[I,],pch = i);
#  }
#}




aux = d1;
lines(ellipse::ellipse(matrix(c(var(aux[,1]),0,0,var(aux[,2])),2),centre = colMeans(aux)), lty = 2);
aux = d2;
lines(ellipse::ellipse(matrix(c(var(aux[,1]),0,0,var(aux[,2])),2),centre = colMeans(aux)), lty = 2);
aux = d3;
lines(ellipse::ellipse(matrix(c(var(aux[,1]),0,0,var(aux[,2])),2),centre = colMeans(aux)), lty = 2);
aux = d4;
lines(ellipse::ellipse(matrix(c(var(aux[,1]),0,0,var(aux[,2])),2),centre = colMeans(aux)), lty = 2);
aux = d5;
lines(ellipse::ellipse(matrix(c(var(aux[,1]),0,0,var(aux[,2])),2),centre = colMeans(aux)), lty = 2);

# variances are assumed Gamma distributed and here beta/alpha gives the expectation)
vars <- out$hp_posterior$Ksi_beta/out$hp_posterior$Ksi_alpha  # Cluster variances
for( i in 1:K){
  lines(ellipse::ellipse(matrix(c(vars[i,1],0,0,vars[i,2]),2),centre = out$hp_posterior$Mu_bar[i,]));
}
