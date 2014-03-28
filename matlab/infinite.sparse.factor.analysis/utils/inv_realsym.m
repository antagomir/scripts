function Ainv=inv_realsym(A)
[e,lam]=eig(A);
Ainv=e*diag(1./diag(lam))*e';