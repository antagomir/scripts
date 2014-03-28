function [samp,results]=isFA(Y,mvmask,samp,results,settings)
% Infinite Sparse Factor Analysis
% Inputs:
%   Y - Dimensions x observations data matrix
%   mvmask - 1 for elements of Y considered to be observed
%            0 for elements that should be considered to be missing
%   samp   - Initial parameters. These can be set randomly using initisAF.m
%   results - If restarting the simulation this should be the old results.
%            Otherwise it should be []
%   settings - Model/algorithm settings. Default settings can be found in
%            defaultsettings.m
%
% Outputs:
%   samp - last sample from the sampler
%   results - table of results, each row is an iteration
%
%  The columns of results are:
% 1. Cumulative CPU time
% 2. log joint
% 3. Alpha
% 4. Beta
% 5. K (number of latent factors)
% 6. Mean square predictive error on missing values
% 7. Predictive log likelihood of missing values
% 8. Sparsity of Z
% 9. Sparsity of T

[D N]=size(Y);
[D K] = size(samp.G);

any_mv=any(any(mvmask==0));
if any_mv
    Yoriginal=Y;
    Ymv=Y(mvmask==0);
end
predError=0;
logpredlikelihood=0;

% Used to monitor acceptance ratio for MH steps
proposals=0;
acceptances=0;

totalObservations=sum(sum(mvmask));

if ~isempty(results)
    basetime=results(end,1);
else
    basetime=0;
end

if settings.store_samples
    samples=cell(settings.store_samples,1);
end

tic

old_lp=-1e100;
lp_threshold=1e4;

mkdir(settings.output);

M=[];
XX=[];

% samp.Gibbs sampling loop
for r=1:settings.iterations
    % Prediction
    Ypred=(samp.Z.*samp.G)*samp.X;

    % Calculate log predictive likelihood over all missing values
    if any_mv
        calculatePredictivePerformance
    end

    if any_mv
        if settings.addNoiseToMV
            e=mvnrnd(zeros(N,D),diag(samp.lambdae.^-1))';
            for i=1:N
                Ypred(:,i)=Ypred(:,i)+e(:,i);
            end
        end
        Y(mvmask==0)=Ypred(mvmask==0);
    end

    % Calculate error, likelihood, and posterior prob terms
    [E,loglikelihood,logposterior]=calculatePosterior();

    old_lp=logposterior;
    Z_sparsity=mean(mean(samp.Z));
    T_sparsity=mean(samp.T);
    % Record some statistics about this iteration...
    results=[results;basetime+toc,logposterior,samp.alpha,samp.Beta,K,predError, ...
        logpredlikelihood,mean(mean(samp.Z)),mean(samp.T)];

    % ... and output them to the screen
    if settings.verbose
        if proposals>0
            acceptance_ratio=acceptances/proposals;
        else
            acceptance_ratio=0;
        end
        fprintf(1,'%d/%d: ll:%-3.3f lp:%-3.3f b=%-3.3f alpha=%-3.3f K=%d a/r=%-3.3f lpl=%-3.3f pe=%-3.3f Zs=%-3.3f Ts=%-3.3f\n' ...
            ,r,settings.iterations,loglikelihood,logposterior,samp.b,samp.alpha,K,acceptance_ratio, ...
            logpredlikelihood,predError,Z_sparsity,T_sparsity);
    else
        fprintf(1,'%d ',r);
        if mod(r,20)==0
            fprintf(1,'\n');
        end
    end

    probZ=[];

    if K>0
        % Sample Z and G
        if settings.sparse && ~settings.fokoue
            sampleZG;
        else
            sampleG;
        end
        sampleX;
    end

    if settings.sampleNoiseVariance
        sampleNoiseHyperparameters();
    end

    sampleMixingMatrixHyperparameters();

    sampleIBPHyperparameters();

    if settings.variableSelection
        sampleT();
    end

    if settings.store_samples>0
        for s=1:(settings.store_samples-1)
            samples{s}=samples{s+1};
        end
        samples{settings.store_samples}=samp;
    else
        samples=samp;
    end

    save(settings.output,'results','samples','settings');
end

fprintf(1,'isFA completed.\n');

    function calculatePredictivePerformance
        predError=mean((Ypred(mvmask==0)-Ymv).^2);
        logpredlikelihood=0;
        for d=1:D
            Yo=Yoriginal(d,:);
            Yp=Ypred(d,:);
            dmv=length(Yp(mvmask(d,:)==0));
             logpredlikelihood=logpredlikelihood+ ...
             logmvnpdf(Yo(mvmask(d,:)==0)',Yp(mvmask(d,:)==0)',samp.lambdae(d)*eye(dmv));
        end
    end

    function sampleT
        sumT=sum(samp.T);
        for d=1:D
            if samp.T(d)
                e=Y(d,:)-samp.G(d,:)*samp.X;
            else
                e=Y(d,:)-samp.Gold(d,:)*samp.X;
            end
            r=((2/samp.b+Y(d,:)*Y(d,:)')/(2/samp.b+e*e'))^((2*settings.a+N)/2)* ...
                (sumT-samp.T(d)+settings.arho)/(settings.brho-(sumT-samp.T(d))+D-1);
            pTis1=r/(r+1);
            if rand<pTis1
                if ~samp.T(d)
                    % turning feature on
                    samp.G(d,:)=samp.Gold(d,:); %or should we sample this?
                    samp.T(d)=1;
                end
            else
                samp.T(d)=0;
            end
        end
        samp.G=bsxfun(@times,samp.G,samp.T);
    end

    function sampleZG
        XX=sum(samp.X.^2,2);
        if settings.variableSelection
            sumT=sum(samp.T);
        end

        if settings.np
            priorExp=samp.alpha*samp.Beta/(samp.Beta+D-1);
            poissdraws=poissrnd(priorExp*settings.accelerateFeatures,D,1);
            if settings.addOneFeature>0
                poissdraws(rand(D,1)<settings.addOneFeature)=1;
            end
        end

        for d = 1:D
            zmask = ones(1,K);
            for k = 1:K
                m = sum(samp.Z(:,k))-samp.Z(d,k);
                if m>0
                    sampleOneElement(d,k);
                else
                    zmask(k) = 0;
                end
            end
            samp.Z(d,:) = samp.Z(d,:).*zmask; % zero marked zs
            samp.G(d,:) = samp.G(d,:).*samp.Z(d,:); % values of samp.X for z=0 should also be zero
            E(d,:)=Y(d,:)-samp.G(d,:)*samp.X;
            if settings.variableSelection
                samp.Gold(d,:) = samp.Gold(d,:).*samp.Z(d,:);
            end
            if settings.np
                deleteRedundantFeatures;
                if ~settings.variableSelection || samp.T(d)
                    sampleK(d,poissdraws(d),priorExp);
                end
            end
        end

        if settings.plotFlag
            figure(1);
            image((1-probZ)*255);
            colormap('gray');
            title('Inferred Z');
            ylabel('Dimension d');
            xlabel('Feature k');
            drawnow;
        end

        % Sample z(k,t)2
        function sampleOneElement(d,k)
            if samp.Z(d,k) && samp.T(d)
                ek=E(d,:)+samp.G(d,k)*samp.X(k,:);
            else
                ek=E(d,:);
            end
            lambda=samp.lambdae(d)*XX(k)+samp.lambdag(k);
            mu=samp.lambdae(d)*samp.X(k,:)*ek'/lambda;
            if settings.richGetRicher
                logrp=log(m/(samp.Beta+D-1-m));
            else
                logrp=log(sum(sum(samp.Z))-samp.Z(d,k)+samp.alpha/K) ... 
                    -log(D*K-1+sum(sum(samp.Z))-samp.Z(d,k)+samp.Beta);
            end
            logrl=0.5*(lambda*mu^2-log(lambda)+log(samp.lambdag(k)));
            if settings.variableSelection
                pTis1=sumT-samp.T(d)+settings.arho;
                pTis0=settings.brho-(sumT-samp.T(d))+D-1;
                GX=samp.Gold(d,:)*samp.X;
                logpTis1weight=-.5*samp.lambdae(d)*(-2*Y(d,:)*GX'+GX*GX');
                A=pTis0+exp(logpTis1weight)*pTis1;
                B=pTis0+exp(logrl+logpTis1weight)*pTis1;
                if ~isinf(A) && ~isinf(B)
                    logrl=log(B)-log(A);
                end
            end
            logrprop=logrp+logrl;
            probzis1 = 1/(1+exp(-logrprop));
            if ~isnan(probzis1)
                samp.Z(d,k) = (rand<probzis1);
            else
                warning('probzis1 is nan');
                if settings.verbose
                    keyboard
                end
            end
            if samp.Z(d,k)
                if settings.variableSelection
                    if rand<(pTis1/(pTis0+pTis1))
                        samp.Gold(d,k)=mu+randn()*lambda^-0.5;
                    else
                        samp.Gold(d,k)=randn()*samp.lambdag(k)^-0.5;
                    end
                    samp.G(d,k)=samp.Gold(d,k)*samp.T(d);
                else
                    samp.G(d,k)=mu+randn()*lambda^-0.5;
                end
            else
                samp.G(d,k)=0;
                samp.Gold(d,k)=0;
            end
            E(d,:)=ek-samp.G(d,k)*samp.X(k,:);
        end
    end

    function sampleK(d,Knew,gamma)
        if (Knew==0)
            return
        end
        proposals=proposals+1;
        if settings.perfactor|(K==0)
            lambdag=gamrnd(settings.c,samp.d,[1 Knew]);
        else
            lambdag=ones(1,Knew)*samp.lambdag(1);
        end
        g = randn(Knew,1).*lambdag';
        % Calculate Lambda and mu
        prec=samp.lambdae(d)*g*g'+eye(Knew);
        Variance=inv(prec);
        M=samp.lambdae(d)*Variance*g*E(d,:);
        try
            logrprop= N/2*(Knew*log(2*pi)+log(det(Variance)))+.5*trace(M'*prec*M);
            if settings.addOneFeature>0 || settings.accelerateFeatures~=1
                propProb=(1-settings.addOneFeature) ...
                    *poisspdf(Knew,gamma*settings.accelerateFeatures)+settings.addOneFeature*(Knew==1);
                logrprop = logrprop + log(poisspdf(Knew,gamma) / propProb);
            end
        catch
            warning('rproposal failed');
        end
        if rand()<exp(logrprop)
            acceptances=acceptances+1;
            % accept proposal
            samp.Z = [samp.Z zeros(D,Knew)];
            % Put ones in the column t of the new rows
            samp.Z(d,K+1:K+Knew)=ones(1,Knew);
            try
                Xprime=M+mvnrnd(zeros(Knew,1),Variance,N)';
            catch
                warning('Bad covariance matrix');
                Xprime=reshape(M,Knew,N);
            end
            samp.X = [samp.X;Xprime];
            XX=[XX;sum(Xprime.^2,2)];
            samp.G = [samp.G zeros(D,Knew)];
            samp.Gold = [samp.Gold zeros(D,Knew)];
            samp.G(d,K+1:K+Knew)=g';
            samp.Gold(d,K+1:K+Knew)=g';
            samp.lambdag=[samp.lambdag lambdag];
            K = K+Knew;
            E(d,:)=E(d,:)-g'*Xprime;
            if settings.sampleNewG
                for k=(K-Knew+1):K
                    ek=E(d,:)+samp.G(d,k)*samp.X(k,:);
                    XdotH=samp.X(k,:);
                    lambda=samp.lambdae(d)*XX(k)+samp.lambdag(k);
                    mu=samp.lambdae(d)*XdotH*ek'/lambda;
                    samp.G(d,k)=normrnd(mu,lambda^(-0.5));
                end
            end
        end
    end

    function deleteRedundantFeatures
        % Delete features which are not active at any data point
        singleton_set = find(sum( samp.Z , 1 ) == 0);
        % Delete feature k
        samp.Z(:,singleton_set)=[];
        samp.X(singleton_set,:)=[];
        XX(singleton_set)=[];
        samp.G(:,singleton_set)=[];
        samp.Gold(:,singleton_set)=[];
        samp.lambdag(singleton_set)=[];
        K=size(samp.Z,2);
    end

% Sample X columnwise. Can do whole matrix but comes down to the same
% thing
    function sampleX
        lambdaG=bsxfun(@times,samp.G,samp.lambdae');
        prec=samp.G'*lambdaG+eye(K);
        try
            Variance=inv_realsym(prec);
        catch
            warning('Singular matrix');
        end
        mus=Variance*lambdaG'*Y;
        firstTime=1;
        for n=1:N
            % could make this faster by doing X=mus+mvnrnd?
            try
                samp.X(:,n)=mvnrnd(mus(:,n),Variance);
            catch
                if firstTime
                    warning('Bad covariance matrix');
                    if settings.verbose
                        keyboard
                    end
                    firstTime=0;
                end
                samp.X(:,n)=mus(:,n);
            end
        end
    end

    function sampleG
        firstTime=1;
        XX=samp.X*samp.X';
        for d=1:D
            if settings.fokoue
                prec=samp.lambdae(d)*XX+diag(samp.lambdag(d,:));
            else
                prec=samp.lambdae(d)*XX+diag(samp.lambdag);
            end
            Variance=inv(prec);
            mu=Variance*samp.lambdae(d)*samp.X*Y(d,:)';
            try
                samp.G(d,:)=mvnrnd(mu,Variance)';
            catch
                if firstTime
                    warning('Bad covariance matrix');
                    firstTime=0;
                end
                samp.G(d,:)=mu';
            end
        end
    end

    function sampleNoiseHyperparameters
        E = Y - samp.G*samp.X;
        E = E .* mvmask;
        % Sample noise level
        if settings.isotropic
            samp.lambdae=ones(1,D)*gamrndi(settings.a+totalObservations/2,samp.b+.5*trace(E'*E));
        else
            for d=1:D
                samp.lambdae(d)=gamrndi(settings.a+sum(mvmask(d,:))/2,samp.b+.5*E(d,:)*E(d,:)');
            end
            if settings.shareNoise
                samp.b=gamrndi(settings.a0+D*settings.a,settings.b0+sum(samp.lambdae));
            end
        end
    end

    function sampleMixingMatrixHyperparameters
        % Sample mixing matrix scale
        if settings.fokoue
            for d=1:D
                if samp.T(d)
                    for k=1:K
                        samp.lambdag(d,k)=gamrndi(settings.c+1/2,settings.d+1/2*samp.G(d,k)^2);
                    end
                end
            end
        else
            ZT=bsxfun(@times,samp.Z,samp.T);
            if settings.perfactor
                for k=1:K
                    samp.lambdag(k)=gamrndi(settings.c+sum(ZT(:,k))/2,samp.d+.5*samp.G(:,k)'*samp.G(:,k));
                end
                if settings.learnscale
                    samp.d=gamrndi(settings.c0+settings.c*K,settings.d0+sum(samp.lambdag));
                end
            else
                samp.lambdag=ones(1,K)*gamrndi(settings.c+sum(sum(ZT))/2,samp.d+.5*trace(samp.G'*samp.G));
            end
        end
    end

    function sampleIBPHyperparameters
        % Sample alpha
        if settings.np && settings.sampleAlpha
            samp.alpha=gamrndi(settings.e+K,settings.f+HD(samp.Beta));
        end
        % Sample beta if using two parameter model
        if settings.sampleBeta
            mk=sum(samp.Z');
            samp.BetaPrime=gamrnd(2,1);
            propa=prod(beta(mk(1:K),N-mk(1:K)+samp.BetaPrime)./beta(mk(1:K),N-mk(1:K)+samp.Beta));
            propb=(samp.BetaPrime/samp.Beta)^K*exp(-samp.alpha*(HD(samp.BetaPrime)-HD(samp.Beta)));
            prop=propa*propb;
            if (rand<prop)
                samp.Beta=samp.BetaPrime;
            end
        end
    end

    function answer=HD(x)
        answer=sum(x./(x+(1:D)-1));
    end

    function [E,loglikelihood,logposterior]=calculatePosterior
        E = Y - samp.G*samp.X;
        E = E.*mvmask;

        try
            term(1)=1/2*sum(sum(mvmask,2)'.*log(.5*samp.lambdae/pi))-sum(samp.lambdae'.*sum(E.^2,2))/2;
            %P(lambdae|a,b)
            if settings.isotropic
                term(2)=log(gampdf(samp.lambdae(1),settings.a,samp.b^-1));
            else
                term(2)=sum(log(gampdf(samp.lambdae,settings.a,samp.b^-1)));
            end
            %P(X)
            term(3)=-K*N/2*log(2*pi)-sum(sum(samp.X.^2))/2;
            term(4)=0;
            if settings.fokoue
                for d=1:D
                    if samp.T(d)
                         term(4)=term(4)+sum(log(gampdf(samp.lambdag(d,:),settings.c,settings.d^-1)));
                    end
                end
            else
                if settings.perfactor
                    % This should still work if K==0
                    term(4)=sum(log(gampdf(samp.lambdag,settings.c,samp.d^-1)));%P(sigma_g|c,d)
                    if settings.learnscale
                        term(4)=term(4)+log(gampdf(samp.d,settings.c0,settings.d0^-1));
                    end
                else
                    if K==0
                        term(4)=0;
                    else
                        term(4)=log(gampdf(samp.lambdag(1),settings.c,samp.d^-1));%P(sigma_g|c,d)
                    end
                end
            end
            term(5)=0;
            for d=1:D
                if settings.fokoue
                    term(5)=term(5)+log(mvnpdf(samp.G(d,:),0,diag(samp.lambdag(d,:).^-1)));
                else
                    term(5)=term(5)+log(mvnpdf(samp.G(d,:),0,diag(samp.lambdag.^-1)));
                end
            end
            mk=sum(samp.Z);
            if settings.np
                term(6)=K*log(samp.alpha*samp.Beta)-samp.alpha*HD(samp.Beta)+sum(betaln(mk(1:K),D-mk(1:K)+samp.Beta));%TODO missing one term
            elseif settings.sparse
                term(6)=sum(betaln(mk+samp.alpha/K,D-mk+1)-betaln(samp.alpha/K,1));
            else
                term(6)=0;
            end
            term(7)=log(gampdf(samp.alpha,settings.e,settings.f^-1));
            term(8)=log(gampdf(samp.Beta,1,2));
            loglikelihood=term(1);
            logposterior=sum(term);
        catch
            warning('Problem calculating logP(x,t)');
            loglikelihood=NaN;
            logposterior=NaN;
            if settings.verbose
                keyboard
            end
        end
    end

end
