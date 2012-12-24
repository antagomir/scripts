function sample=initisFA(settings)
% Make an initial sample for the MCMC sampler. 

    sample.alpha=1;
    if ~settings.sampleAlpha
        sample.alpha=settings.alpha;
    end
    sample.Beta=1;

    if settings.np
        if settings.K
            sample.Z=ones(settings.D,settings.K);
        else
            % Initialise sample.Z to a random sample from the IBP
            sample.Z = ibpgen(settings.D,sample.alpha);
        end
        % K is the number of non-zero rows in sample.Z
        [D K] = size(sample.Z);
    else
        K=settings.K;
        D=settings.D;
        sample.Z=ones(settings.D,K);
    end
    
    sample.b=settings.b;
    % Initialise alpha,sigmae,sigmag, and sample.Beta
    sample.lambdae=ones(1,D)*gamrnd(settings.a,sample.b);
    
    sample.T=ones(D,1);

    % Randomly initialise X and G from their priors
    if settings.lapFlag
        sample.X = laprnd(0,1,K,settings.N);
    else
        sample.X = normrnd(0,1,[K settings.N]);
    end
    
    if settings.fokoue
        sample.lambdag=ones(D,K);
    else
        sample.lambdag=ones(1,K)*gamrnd(settings.c,settings.d);
    end
    
    sample.d=settings.d;
    
    sample.G = normrnd(0,1,[settings.D K]);
    sample.Gold = sample.G;

    sample.G = sample.G.*sample.Z;
end