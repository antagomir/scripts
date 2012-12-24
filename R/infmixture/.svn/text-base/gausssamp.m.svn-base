function g = gausssamp(mu,sig,N)
if ~exist('N')
    N = 1;
end
Ch = chol(sig)';
u = randn(N,length(mu));
g = u*Ch' + repmat(mu,N,1);