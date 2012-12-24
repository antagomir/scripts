function Z = ibpgen(N, alpha);
infinity = 1000; 
Z = zeros(N,infinity);
m = zeros(infinity); 
Kplus=1;
for i=1:N
    for k=1:infinity
        if (m(k)>0)
            if (rand(1)<(m(k)/i))
                Z(i,k) = 1;
                m(k) = m(k) + 1; 
            end
        else
            break
        end
    end
    new = poissrnd(alpha/i);
    if (new>0)
        Z(i,k:k+new-1) = ones(1,new);
        m(k:k+new-1) = m(k:k+new-1) + ones(1,new);
        Kplus = k+new-1; 
    end
end
Z = Z(:,1:Kplus);
%colormap('gray');
%image(255*(1-Z(1:N,1:coltodraw))); 