function out = gibbs_dep_infall_marg_tables(X,Y,varargin)
nI = 3;
nK = 2; % X marginals
nJ = 3; % Y marginals
[N,mX] = size(X);
[N,mY] = size(Y);
no_its = 10;
kap0K = 1;v0K = mX+1;mu0K = repmat(0,1,mX);Lam0K = eye(mX);
kap0J = 1;v0J = mY+1;mu0J = repmat(0,1,mY);Lam0J = eye(mY);
bprior = 'mv';
burn = 0;
verbose = 2;
alp0 = 1;gam0K = 1;bet0J=1;gam0J=1;

infmix = [1 1 1];
GeGo = [];
updatehyp = 0;hp = [1 1];
comp_type = {'gauss','gauss'}; % Gaussian (gauss) or multinomial (mult)
mult_hyp_pars = {repmat(1,1,mX),repmat(1,1,mY)};
indiv_beta = 0;
storeID = 0;storepoints = [];
for i = 1:2:length(varargin)-1
    switch lower(varargin{i})
        case 'k' % Initial number of X-clusters
            nK = varargin{i+1};
        case 'j' % Initial number of Y-clusters
            nJ = varargin{i+1};
        case 'i' % Initial number of top-level clusters
            nI = varargin{i+1};
        case 'no_its' % Number of posterior samples
            no_its = varargin{i+1};
        case 'infmix'
            infmix = varargin{i+1};
        case 'verbose' % verbosity: 1=plot some textual summaries,
                       % 2=draw images
            verbose = varargin{i+1};
        case 'alpha' % Value for alpha (might not work due to
                     % implementation of hyperparameter sampling)
            alp0 = varargin{i+1};
        case 'beta' % Values for beta, 2-dimensional vector (see alpha)
            bet0 = varargin{i+1};
        case 'gamma' % Values for gamma, 2-dimensional vector (see alpha)
            gam0 = varargin{i+1};
        case 'v0k'
            v0K = varargin{i+1};
        case 'v0j'
            v0J = varargin{i+1};
        case 'lam0j'
            Lam0J = varargin{i+1};
        case 'lam0k'
            Lam0K = varargin{i+1};
        case 'thetainit' % Initial value for theta
            Theta = varargin{i+1};
        case 'phiinit' % See theta
            Phi = varargin{i+1};
        case 'lamfac'
            Lam0K = Lam0K * varargin{i+1};
            Lam0J = Lam0J * varargin{i+1};
        case 'kap0K'
            kap0K = varargin{i+1};
        case 'kap0J'
            kap0J = varargin{i+1};
        case 'prior'
            bprior = varargin{i+1};
        case 'gego'
            GeGo = varargin{i+1};
            out.zp = zeros(N,size(GeGo,2));
            out.kp = out.zp;
            out.jp = out.zp;
        case 'burn' % Number of samples to be dropped as burn-in period
            burn = varargin{i+1};
        case 'updatehyp' % Will the hyperparameters be samples or
                         % ketp fixed
            updatehyp = varargin{i+1};
        case 'hypprior' % Struct containing parameters for hyperpriors
            hypprior = varargin{i+1};
        case 'comp_type' % Component type; 'gauss' or 'mult'
            comp_type = varargin{i+1};
        case 'indiv_beta' % Does each top-level cluster has its own beta?
            indiv_beta = varargin{i+1};
        case 'storeid' % Will we store indicator variables?
            storeID = varargin{i+1};
        case 'storepoints' % For which samples the indicator
                           % variables are stored
            storepoints = varargin{i+1};
            out.storepoints = storepoints;
    end
end




if ~exist('Theta')
    % Initialise assignments
    % EVeryone in one group at one table
    nK = 1;nJ = 1;nI = 1;nTK = 1;nTJ = 1;
    iTablesK(1,1) = 1; % Which tables are in this restaurant TK x I
    iTablesJ(1,1) = 1; % TJ x I
    ktp = repmat(1,N,1); % Which people sit at this table N x T
    jtp = repmat(1,N,1); % N x T
    Ktables(1,1) = 1; % Which component does this table use TK x K
    Jtables(1,1) = 1; % TJ x J

    % Better initialisation
    if(0)
    nK = 3;
    nJ = 4;
    Theta = rand(N,nK);
    Theta = (Theta' == repmat(max(Theta'),nK,1))';
    Phi = rand(N,nJ);
    Phi = (Phi' == repmat(max(Phi'),nJ,1))';
    Ktables = eye(nK);
    Jtables = eye(nJ);
    iTablesK = eye(nK);
    iTablesJ = [1 0 0; eye(nK)];
    ktp = Theta;jtp = Phi;
    nI = nK;
    nTK = nK;
    nTJ = nJ;
    end;
else
%     nK = size(Theta,2);
%     nJ = size(Phi,2);
%     nI = nK;
%     iTablesK = eye(nK);
%     iTablesJ = eye(nJ);
%     ktp = Theta;
%     jtp = Phi;
%     Ktables = eye(nK);
%     Jtables = eye(nJ);
%     nTK = nK;
%     nTJ = nJ;
    nK = size(Theta,2);
    nJ = size(Phi,2);
    nI = 1;
    iTablesK = ones(nK,1);
    iTablesJ = ones(nJ,1);
    ktp = Theta;
    jtp = Phi;
    Ktables = eye(nK);
    Jtables = eye(nJ);
    nTK = nK;
    nTJ = nJ;
end
% Construct marginal assignents
Z = ktp * iTablesK;
Theta = ktp * Ktables;
Phi = jtp * Jtables;

bet0K = repmat(1,nI,1);bet0J = repmat(1,nI,1);

IID = [1:nI]';
KID = [1:nK]';
JID = [1:nJ]';
nextIID = nI+1;
nextKID = nK + 1;
nextJID = nJ + 1;

out.Zcross = zeros(N);
out.Kcross = zeros(N);
out.Jcross = zeros(N);

out.nKall = zeros(no_its-burn,1);
out.nJall = out.nKall;out.nKall2 = out.nKall;out.nKall5 = out.nKall;
out.nIall = out.nKall;out.nJall2 = out.nJall;out.nJall5 = out.nJall;
out.nIall2 = out.nIall;out.nIall5 = out.nIall;

% Construct statistics necessary for the marginals
ClustSumK = sum(Theta);
for k = 1:nK
    DataSumK(k,:) = sum(X(find(Theta(:,k)),:),1);
    ClustCrossK(:,:,k) = X(find(Theta(:,k)),:)'*X(find(Theta(:,k)),:);
    switch comp_type{1}
        case 'gauss'
            TPDFK(:,k) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(k,:),ClustCrossK(:,:,k),ClustSumK(k),bprior);
        case 'mult'
            TPDFK(:,k) = comp_pred_dirichlet(X,DataSumK(k,:),mult_hyp_pars{1});
    end
end
switch comp_type{1}
    case 'gauss'
        switch bprior
            case 'mv'
                EMPTYK = compmvtpdf(X,mu0K,(Lam0K*(1+kap0K))./(kap0K*(v0K-mX+1)),v0K-mX+1);
            case 'uv'
                EMPTYK = compuvtpdf(X,mu0K,(repmat(Lam0K*v0K,1,mX).*(kap0K + 1)./(kap0K*(v0K))),v0K);
        end
    case 'mult'
        EMPTYK = comp_pred_dirichlet(X,repmat(0,1,mX),mult_hyp_pars{1});
end


ClustSumJ = sum(Phi);
for j = 1:nJ
    DataSumJ(j,:) = sum(Y(find(Phi(:,j)),:),1);
    ClustCrossJ(:,:,j) = Y(find(Phi(:,j)),:)'*Y(find(Phi(:,j)),:);
    switch comp_type{2}
        case 'gauss'
            TPDFJ(:,j) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(j,:),ClustCrossJ(:,:,j),ClustSumJ(j),bprior);
        case 'mult'
            TPDFJ(:,j) = comp_pred_dirichlet(Y,DataSumJ(j,:),mult_hyp_pars{2});
    end
end
switch comp_type{2}
    case 'gauss'
        switch bprior
            case 'mv'
                EMPTYJ = compmvtpdf(Y,mu0J,(Lam0J*(1+kap0J))./(kap0J*(v0J-mY+1)),v0J-mY+1);
            case 'uv'
                EMPTYJ = compuvtpdf(Y,mu0J,(repmat(Lam0J*v0J,1,mY).*(kap0J)./(kap0J*(v0J+1))),v0J);
        end
    case 'mult'
        EMPTYJ = comp_pred_dirichlet(Y,repmat(0,1,mY),mult_hyp_pars{2});
        
end


tablesumk = sum(ktp,1);
tablesumj = sum(jtp,1);
Ktablesum = sum(Ktables,1);
Jtablesum = sum(Jtables,1);

out.hypall = zeros(no_its,5);

if storeID
    NP = length(storepoints);
    out.allIID = cell(NP,1);
    out.allJID = cell(NP,1);
    out.allKID = cell(NP,1);
    out.allIIDsizes = cell(NP,1);
    out.allJIDsizes = cell(NP,1);
    out.allKIDsizes = cell(NP,1);
    out.allZ = cell(NP,1);
    out.allTheta = cell(NP,1);
    out.allPhi = cell(NP,1);
end

for it = 1:no_its
    if verbose
        fprintf('\nIt: %g, nI: %g, nK: %g, nJ: %g, nTK: %g, nTJ: %g',...
            it,nI,nK,nJ,nTK,nTJ);
    end
    order = randperm(N);
    % Resample Z
    % New version - update z by marginalising over j and k and then update
    % j and k
    
    for n = 1:N
        oldIID = -1;oldKID = -1;oldJID = -1;

        tn = order(n);
        
        ti = find(Z(tn,:));
        tk = find(Theta(tn,:));
        tj = find(Phi(tn,:));
        ttk = find(ktp(tn,:)); % Current k table
        ttj = find(jtp(tn,:)); % Current j table
        
        % Remove this person
        % From the table
        ktp(tn,:) = 0;
        jtp(tn,:) = 0;        
        % From the marginals
        Theta(tn,:) = 0;
        Phi(tn,:) = 0;
        Z(tn,:) = 0;
        % From the counts
        tablesumk(ttk) = tablesumk(ttk) - 1;
        tablesumj(ttj) = tablesumj(ttj) - 1;
        ClustSumK(tk) = ClustSumK(tk) - 1;
        ClustSumJ(tj) = ClustSumJ(tj) - 1;
        
               
        % Does this leave anything empty?
        % Tables
        if tablesumk(ttk) == 0
            nTK = nTK - 1;
            tablesumk(ttk) = [];
            iTablesK(ttk,:) = [];
            ktp(:,ttk) = [];
            Ktables(ttk,:) = [];
            Ktablesum(tk) = Ktablesum(tk) - 1;
        end
        % is this k empty now?
        if Ktablesum(tk) == 0
            nK = nK - 1;
            Theta(:,tk) = [];
            ClustSumK(tk) = [];
            DataSumK(tk,:) = [];
            ClustCrossK(:,:,tk) = [];
            TPDFK(:,tk) = [];
            Ktables(:,tk) = [];
            Ktablesum(tk) = [];
            oldKID = KID(tk);
            KID(tk) = [];
            tk = -1;
        else
            % Update the k marginal stats
            DataSumK(tk,:) = DataSumK(tk,:) - X(tn,:);
            ClustCrossK(:,:,tk) = ClustCrossK(:,:,tk) - X(tn,:)'*X(tn,:);
            switch comp_type{1}
                case 'gauss'
                    TPDFK(:,tk) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(tk,:),ClustCrossK(:,:,tk),ClustSumK(tk),bprior);
                case 'mult'
                    TPDFK(:,tk) = comp_pred_dirichlet(X,DataSumK(tk,:),mult_hyp_pars{1});
            end
        end

        if tablesumj(ttj) == 0
            nTJ = nTJ - 1;
            tablesumj(ttj) = [];
            iTablesJ(ttj,:) = [];
            jtp(:,ttj) = [];
            Jtables(ttj,:) = [];
            Jtablesum(tj) = Jtablesum(tj) - 1;
        end
        % is this j empty now?
        if Jtablesum(tj) == 0
            nJ = nJ - 1;
            Phi(:,tj) = [];
            ClustSumJ(tj) = [];
            DataSumJ(tj,:) = [];
            ClustCrossJ(:,:,tj) = [];
            TPDFJ(:,tj) = [];
            Jtables(:,tj) = [];
            Jtablesum(tj) = [];
            oldJID = JID(tj);
            JID(tj) = [];
        else
            % Update the j marginal stats
            DataSumJ(tj,:) = DataSumJ(tj,:) - Y(tn,:);
            ClustCrossJ(:,:,tj) = ClustCrossJ(:,:,tj) - Y(tn,:)'*Y(tn,:);
            switch comp_type{2}
                case 'gauss'
                    TPDFJ(:,tj) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(tj,:),ClustCrossJ(:,:,tj),ClustSumJ(tj),bprior);
                case 'mult'
                    TPDFJ(:,tj) = comp_pred_dirichlet(Y,DataSumJ(tj,:),mult_hyp_pars{2});
            end
        end

        % Does this leave an empty restaurant?
        if sum(iTablesK(:,ti)) == 0
            nI = nI - 1;
            iTablesK(:,ti) = [];
            iTablesJ(:,ti) = [];
            Z(:,ti) = [];
            bet0K(ti) = [];
            bet0J(ti) = [];
            oldIID = IID(ti);
            IID(ti) = [];
        end
        
        % Choose a new restuarant for this person (tn)
        % Marginalise over the tables in each restaurant
        % Compute the number of people sitting at each table
        kcount = iTablesK .* repmat(tablesumk',1,nI);
        temppdfk = zeros(1,nTK);
        for k = 1:nTK
            temppdfk(k) = TPDFK(tn,find(Ktables(k,:)));
        end
        knew = [sum(Ktables,1).*exp(TPDFK(tn,:)) gam0K*exp(EMPTYK(tn))];
        knew = knew./(sum(sum(Ktables)) + gam0K);
        knews = sum(knew);
        
        margk = sum(kcount.*repmat(exp(temppdfk)',1,nI),1) + bet0K'*knews;
        margk = margk./(sum(kcount,1)+bet0K');
        
        jcount = iTablesJ .* repmat(tablesumj',1,nI);
        jnew = [sum(Jtables,1).*exp(TPDFJ(tn,:)) gam0J*exp(EMPTYJ(tn))];
        jnew = jnew./(sum(sum(iTablesJ)) + gam0J);
        jnews = sum(jnew);
        
        temppdfj = zeros(1,nTJ);
        for j = 1:nTJ
            temppdfj(j) = TPDFJ(tn,find(Jtables(j,:)));
        end
        
        margj = sum(jcount.*repmat(exp(temppdfj)',1,nI),1) + bet0J'*jnews;
        margj = margj./(sum(jcount,1)+bet0J');
        prior = [sum(Z,1) alp0];
        post = log([margk knews]) + log([margj jnews]) + log(prior);
        
        post = exp(post - max(post));
        post = post./sum(post);
        ti = find(multsamp(post));
        
        if ti <= nI
            % This is a current restaurant
            Z(tn,ti) = 1;
        else
            nI = nI + 1;
            if oldIID>0
                IID(nI,1) = oldIID;
            else
                IID(nI,1) = nextIID;
                nextIID = nextIID + 1;
            end
            Z(:,nI) = zeros(N,1);
            Z(tn,ti) = 1;
            iTablesJ(:,nI) = zeros(nTJ,1);
            iTablesK(:,nI) = zeros(nTK,1);
            kcount(:,ti) = zeros(nTK,1);
            jcount(:,ti) = zeros(nTJ,1);
            if indiv_beta
                bet0K(ti,1) = MHup(1,N,1,1,10,hypprior(4));
                bet0J(ti,1) = MHup(1,N,1,1,10,hypprior(5));
            else
                bet0K(ti,1) = bet0K(1);
                bet0J(ti,1) = bet0J(1);
            end
        end
        
        % Now assign the people to a table in this restaurant
        % They can sit at a current one or create a new one.
        kc = kcount(:,ti); % This is the number of people at tables in here
        prior = [kc' bet0K(ti)];
        post = [temppdfk log(knews)];
        post = post + log(prior);
        post = exp(post - max(post));
        post = post./sum(post);
        newttk = find(multsamp(post));
        if newttk <= nTK
            % Current table
            % iTablesK(ttk,ti) = 1 by defn
            tk = find(Ktables(newttk,:));
            Theta(tn,tk) = 1;
            ktp(tn,newttk) = 1;
            % Update the sums
            ClustSumK(tk) = ClustSumK(tk) + 1;
            tablesumk(newttk) = tablesumk(newttk) + 1;
            DataSumK(tk,:) = DataSumK(tk,:) + X(tn,:);
            ClustCrossK(:,:,tk) = ClustCrossK(:,:,tk) + X(tn,:)'*X(tn,:);
            switch comp_type{1}
                case 'gauss'
                    TPDFK(:,tk) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(tk,:),ClustCrossK(:,:,tk),ClustSumK(tk),bprior);
                case 'mult'
                    TPDFK(:,tk) = comp_pred_dirichlet(X,DataSumK(tk,:),mult_hyp_pars{1});
            end
        else
            % This is a new table
            post = knew./sum(knew);
            newttk = find(multsamp(post));
            if newttk <= nTK
                % It is a new one with parameter currently used in a different restaurant
                nTK = nTK + 1;
                ktp(:,nTK) = zeros(N,1);
                ktp(tn,nTK) = 1;
                tk = find(Ktables(newttk,:));
                Theta(tn,tk) = 1;
                iTablesK(nTK,:) = zeros(1,nI);
                iTablesK(nTK,ti) = 1; % Add this table to this restaurant
                Ktables(nTK,:) = zeros(1,nK);
                Ktables(nTK,tk) = 1;
                Ktablesum(tk) = Ktablesum(tk) + 1;
                ClustSumK(tk) = ClustSumK(tk) + 1;
                tablesumk(nTK) = 1;
                DataSumK(tk,:) = DataSumK(tk,:) + X(tn,:);
                ClustCrossK(:,:,tk) = ClustCrossK(:,:,tk) + X(tn,:)'*X(tn,:);
                switch comp_type{1}
                    case 'gauss'
                        TPDFK(:,tk) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(tk,:),ClustCrossK(:,:,tk),ClustSumK(tk),bprior);
                    case 'mult'
                        TPDFK(:,tk) = comp_pred_dirichlet(X,DataSumK(tk,:),mult_hyp_pars{1});
                end
            else
                % A completely new table
                nTK = nTK + 1;
                nK = nK + 1;
                if oldKID > 0
                    KID(nK,1) = oldKID;
                else
                    KID(nK,1) = nextKID;
                    nextKID = nextKID + 1;
                end
                Theta(:,nK) = zeros(N,1);
                Theta(tn,nK) = 1;
                ktp(:,nTK) = zeros(N,1);
                ktp(tn,nTK) = 1;
                iTablesK(nTK,:) = zeros(1,nI);
                iTablesK(nTK,ti) = 1;
                Ktables(nTK,:) = zeros(1,nK-1);
                Ktables(:,nK) = zeros(nTK,1);
                Ktables(nTK,nK) = 1;
                Ktablesum(1,nK) = 1;
                tablesumk(1,nTK) = 1;
                ClustSumK(nK) = 1;
                DataSumK(nK,:) = X(tn,:);
                ClustCrossK(:,:,nK) = X(tn,:)'*X(tn,:);
                switch comp_type{1}
                    case 'gauss'
                        TPDFK(:,nK) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(nK,:),ClustCrossK(:,:,nK),ClustSumK(nK),bprior);
                    case 'mult'
                        TPDFK(:,nK) = comp_pred_dirichlet(X,DataSumK(nK,:),mult_hyp_pars{1});
                end
            end
        end
        
        
        
        jc = jcount(:,ti); % This is the number of people at tables in here
        prior = [jc' bet0J(ti)];
        post = [temppdfj log(jnews)];
        post = post + log(prior);
        post = exp(post - max(post));
        post = post./sum(post);
        newttj = find(multsamp(post));
        if newttj <= nTJ
            % Current table
            % iTablesJ(ttj,ti) = 1 by defn
            tj = find(Jtables(newttj,:));
            Phi(tn,tj) = 1;
            jtp(tn,newttj) = 1;
            % Update the sums
            ClustSumJ(tj) = ClustSumJ(tj) + 1;
            tablesumj(newttj) = tablesumj(newttj) + 1;
            DataSumJ(tj,:) = DataSumJ(tj,:) + Y(tn,:);
            ClustCrossJ(:,:,tj) = ClustCrossJ(:,:,tj) + Y(tn,:)'*Y(tn,:);
            switch comp_type{2}
                case 'gauss'
                    TPDFJ(:,tj) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(tj,:),ClustCrossJ(:,:,tj),ClustSumJ(tj),bprior);
                case 'mult'
                    TPDFJ(:,tj) = comp_pred_dirichlet(Y,DataSumJ(tj,:),mult_hyp_pars{2});
            end
        else
            % This is a new table
            post = jnew./sum(jnew);
            newttj = find(multsamp(post));
            if newttj <= nTJ
                % It is a new one with parameter currently used in a different restaurant
                nTJ = nTJ + 1;
                jtp(:,nTJ) = zeros(N,1);
                jtp(tn,nTJ) = 1;
                tj = find(Jtables(newttj,:));
                Phi(tn,tj) = 1;
                iTablesJ(nTJ,:) = zeros(1,nI);
                iTablesJ(nTJ,ti) = 1; % Add this table to this restaurant
                Jtables(nTJ,:) = zeros(1,nJ);
                Jtables(nTJ,tj) = 1;
                Jtablesum(tj) = Jtablesum(tj) + 1;
                ClustSumJ(tj) = ClustSumJ(tj) + 1;
                tablesumj(nTJ) = 1;
                DataSumJ(tj,:) = DataSumJ(tj,:) + Y(tn,:);
                ClustCrossJ(:,:,tj) = ClustCrossJ(:,:,tj) + Y(tn,:)'*Y(tn,:);
                switch comp_type{2}
                    case 'gauss'
                        TPDFJ(:,tj) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(tj,:),ClustCrossJ(:,:,tj),ClustSumJ(tj),bprior);              
                    case 'mult'
                        TPDFJ(:,tj) = comp_pred_dirichlet(Y,DataSumJ(tj,:),mult_hyp_pars{2});
                end
            else
                % A completely new table
                nTJ = nTJ + 1;
                nJ = nJ + 1;
                if oldJID > 0
                    JID(nJ,1) = oldJID;
                else
                    JID(nJ,1) = nextJID;
                    nextJID = nextJID + 1;
                end
                Phi(:,nJ) = zeros(N,1);
                Phi(tn,nJ) = 1;
                jtp(:,nTJ) = zeros(N,1);
                jtp(tn,nTJ) = 1;
                iTablesJ(nTJ,:) = zeros(1,nI);
                iTablesJ(nTJ,ti) = 1;
                Jtables(nTJ,:) = zeros(1,nJ-1);
                Jtables(:,nJ) = zeros(nTJ,1);
                Jtables(nTJ,nJ) = 1;
                Jtablesum(1,nJ) = 1;
                tablesumj(1,nTJ) = 1;
                ClustSumJ(nJ) = 1;
                DataSumJ(nJ,:) = Y(tn,:);
                ClustCrossJ(:,:,nJ) = Y(tn,:)'*Y(tn,:);
                switch comp_type{2}
                    case 'gauss'
                        TPDFJ(:,nJ) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(nJ,:),ClustCrossJ(:,:,nJ),ClustSumJ(nJ),bprior);              
                    case 'mult'
                        TPDFJ(:,nJ) = comp_pred_dirichlet(Y,DataSumJ(nJ,:),mult_hyp_pars{2});
                end
            end
        end

        
    end   
    
    % Now, we resample the assignments of j's and k's to tables
    % Start with k's
    % New version where the likelihood is made by stacking
    for t = 1:nTK
       oldKID = -1;
       tk = find(Ktables(t,:));
       tn = find(ktp(:,t));
       % Update the stats
       Ktables(t,tk) = 0;
       Ktablesum(tk) = Ktablesum(tk) - 1;
       
       oldClustSumK = ClustSumK;
       oldDataSumK = DataSumK(tk,:);
       oldClustCrossK = ClustCrossK(:,:,tk);
       oldTPDFK = TPDFK(:,tk);
       
       if Ktablesum(tk) == 0
           % Remove this k
           nK = nK - 1;
           Theta(:,tk) = [];
           Ktables(:,tk) = [];
           Ktablesum(tk) = [];
           ClustSumK(tk) = [];
           DataSumK(tk,:) = [];
           ClustCrossK(:,:,tk) = [];
           TPDFK(:,tk) = [];
           oldKID = KID(tk);
           KID(tk) = [];
           tk = -1;
       else
           % Update the stats
           Theta(tn,tk) = 0;
           ClustSumK(tk) = ClustSumK(tk) - length(tn);
           DataSumK(tk,:) = DataSumK(tk,:) - sum(X(tn,:),1);
           ClustCrossK(:,:,tk) = ClustCrossK(:,:,tk) - X(tn,:)'*X(tn,:);
           switch comp_type{1}
               case 'gauss'
                   TPDFK(:,tk) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(tk,:),ClustCrossK(:,:,tk),ClustSumK(tk),bprior);
               case 'mult'
                   TPDFK(:,tk) = comp_pred_dirichlet(X,DataSumK(tk,:),mult_hyp_pars{1});
           end
       end
       
       prior = [Ktablesum gam0K];
       % Compute post by stacking
       post = zeros(1,nK);
       switch comp_type{1}
           case 'gauss'
               for k = 1:nK
                   post(1,k) = compstacklike(X(tn,:),ClustSumK(k),DataSumK(k,:),ClustCrossK(:,:,k),mu0K,Lam0K,v0K,kap0K,bprior);
               end
               post = [post compstacklike(X(tn,:),0,repmat(0,1,mX),zeros(mX),mu0K,Lam0K,v0K,kap0K,bprior)];
           case 'mult'
               for k = 1:nK
                   post(1,k) = compstacklikeDM(X(tn,:),DataSumK(k,:),mult_hyp_pars{1});
               end
               post = [post compstacklikeDM(X(tn,:),repmat(0,1,mX),mult_hyp_pars{1})];
       end
       
       post = post + log(prior);
       post = exp(post - max(post));
       post = post./sum(post);
       newk = find(multsamp(post));
       if newk > nK
            nK = nK + 1;
            if oldKID > 0
                KID(nK,1) = oldKID;
            else
                KID(nK,1) = nextKID;
                nextKID = nextKID + 1;
            end
            Ktables(:,nK) = zeros(nTK,1);
            Ktables(t,nK) = 1;
            Ktablesum(nK) = 1;
            ClustSumK(nK) = length(tn);
            DataSumK(nK,:) = sum(X(tn,:),1);
            ClustCrossK(:,:,nK) = X(tn,:)' * X(tn,:);
            switch comp_type{1}
                case 'gauss'
                    TPDFK(:,nK) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(nK,:),ClustCrossK(:,:,nK),ClustSumK(nK),bprior);
                case 'mult'
                    TPDFK(:,nK) = comp_pred_dirichlet(X,DataSumK(nK,:),mult_hyp_pars{1});
            end
            Theta(:,nK) = zeros(N,1);
            Theta(tn,nK) = 1;
            
       elseif newk == tk
            ClustSumK = oldClustSumK;
            DataSumK(tk,:) = oldDataSumK;
            ClustCrossK(:,:,tk) = oldClustCrossK;
            TPDFK(:,tk) = oldTPDFK;
            Ktables(t,tk) = 1;
            Ktablesum(tk) = Ktablesum(tk) + 1;
            Theta(tn,tk) = 1;
       else
            ClustSumK(newk) = ClustSumK(newk) + length(tn);
            DataSumK(newk,:) = DataSumK(newk,:) + sum(X(tn,:),1);
            ClustCrossK(:,:,newk) = ClustCrossK(:,:,newk) + X(tn,:)'*X(tn,:);
            switch comp_type{1}
                case 'gauss'
                    TPDFK(:,newk) = updateTpdf(X,mu0K,Lam0K,v0K,kap0K,DataSumK(newk,:),ClustCrossK(:,:,newk),ClustSumK(newk),bprior);
                case 'mult'
                    TPDFK(:,newk) = comp_pred_dirichlet(X,DataSumK(newk,:),mult_hyp_pars{1});
            end
            Ktables(t,newk) = 1;
            Ktablesum(newk) = Ktablesum(newk) + 1;
            Theta(tn,newk) = 1;
       end
            
    end
    
    
    
    
    for t = 1:nTJ
        oldJID = -1;
       tj = find(Jtables(t,:));
       tn = find(jtp(:,t));
       % Update the stats
       Jtables(t,tj) = 0;
       Jtablesum(tj) = Jtablesum(tj) - 1;
       
       oldClustSumJ = ClustSumJ;
       oldDataSumJ = DataSumJ(tj,:);
       oldClustCrossJ = ClustCrossJ(:,:,tj);
       oldTPDFJ = TPDFJ(:,tj);
       
       if Jtablesum(tj) == 0
           % Remove this j
           nJ = nJ - 1;
           Phi(:,tj) = [];
           Jtables(:,tj) = [];
           Jtablesum(tj) = [];
           ClustSumJ(tj) = [];
           DataSumJ(tj,:) = [];
           ClustCrossJ(:,:,tj) = [];
           TPDFJ(:,tj) = [];
           oldJID = JID(tj);
           JID(tj) = [];
           tj = -1;
       else
           % Update the stats
           Phi(tn,tj) = 0;
           ClustSumJ(tj) = ClustSumJ(tj) - length(tn);
           DataSumJ(tj,:) = DataSumJ(tj,:) - sum(Y(tn,:),1);
           ClustCrossJ(:,:,tj) = ClustCrossJ(:,:,tj) - Y(tn,:)'*Y(tn,:);
           switch comp_type{2}
               case 'gauss'
                   TPDFJ(:,tj) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(tj,:),ClustCrossJ(:,:,tj),ClustSumJ(tj),bprior);
               case 'mult'
                   TPDFJ(:,tj) = comp_pred_dirichlet(Y,DataSumJ(tj,:),mult_hyp_pars{2});
           end
       end
       
       prior = [Jtablesum gam0J];
       post = zeros(1,nJ);
       switch comp_type{2}
           case 'gauss'
               for j = 1:nJ
                   post(1,j) = compstacklike(Y(tn,:),ClustSumJ(j),DataSumJ(j,:),ClustCrossJ(:,:,j),mu0J,Lam0J,v0J,kap0J,bprior);
               end
               post = [post compstacklike(Y(tn,:),0,repmat(0,1,mY),zeros(mY),mu0J,Lam0J,v0J,kap0J,bprior)];
           case 'mult'
                for j = 1:nJ
		    %AKFIX: 
                    post(1,j) = compstacklikeDM(Y(tn,:),DataSumJ(j),mult_hyp_pars{2});
                end
                post = [post compstacklikeDM(Y(tn,:),repmat(0,1,mY),mult_hyp_pars{2})];
       end

%        post = [sum(TPDFJ(tn,:),1) sum(EMPTYJ(tn))];
       post = post + log(prior);
       post = exp(post - max(post));
       post = post./sum(post);
       newj = find(multsamp(post));
       if newj > nJ
            nJ = nJ + 1;
            if oldJID > 0
                JID(nJ,1) = oldJID;
            else
                JID(nJ,1) = nextJID;
                nextJID = nextJID + 1;
            end
            Jtables(:,nJ) = zeros(nTJ,1);
            Jtables(t,nJ) = 1;
            Jtablesum(nJ) = 1;
            ClustSumJ(nJ) = length(tn);
            DataSumJ(nJ,:) = sum(Y(tn,:),1);
            ClustCrossJ(:,:,nJ) = Y(tn,:)' * Y(tn,:);
            switch comp_type{2}
                case 'gauss'
                    TPDFJ(:,nJ) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(nJ,:),ClustCrossJ(:,:,nJ),ClustSumJ(nJ),bprior);
                case 'mult'
                    TPDFJ(:,nJ) = comp_pred_dirichlet(Y,DataSumJ(nJ,:),mult_hyp_pars{2});
            end
            Phi(:,nJ) = zeros(N,1);
            Phi(tn,nJ) = 1;
            
       elseif newj == tj
            ClustSumJ = oldClustSumJ;
            DataSumJ(tj,:) = oldDataSumJ;
            ClustCrossJ(:,:,tj) = oldClustCrossJ;
            TPDFJ(:,tj) = oldTPDFJ;
            Jtables(t,tj) = 1;
            Jtablesum(tj) = Jtablesum(tj) + 1;
            Phi(tn,tj) = 1;
       else
            ClustSumJ(newj) = ClustSumJ(newj) + length(tn);
            DataSumJ(newj,:) = DataSumJ(newj,:) + sum(Y(tn,:),1);
            ClustCrossJ(:,:,newj) = ClustCrossJ(:,:,newj) + Y(tn,:)'*Y(tn,:);
            switch comp_type{2}
                case 'gauss'
                    TPDFJ(:,newj) = updateTpdf(Y,mu0J,Lam0J,v0J,kap0J,DataSumJ(newj,:),ClustCrossJ(:,:,newj),ClustSumJ(newj),bprior);
                case 'mult'
                    TPDFJ(:,newj) = comp_pred_dirichlet(Y,DataSumJ(newj,:),mult_hyp_pars{2});
            end
            Jtables(t,newj) = 1;
            Jtablesum(newj) = Jtablesum(newj) + 1;
            Phi(tn,newj) = 1;
       end
            
    end

    
    if storeID & any(storepoints == it)
        pos = find(storepoints == it);
        out.allIID{pos} = IID;
        out.allKID{pos} = KID;
        out.allJID{pos} = JID;
        out.allIIDsizes{pos} = sum(Z,1)';
        out.allKIDsizes{pos} = sum(Theta,1)';
        out.allJIDsizes{pos} = sum(Phi,1)';
        out.allZ{pos} = sparse(Z);
        out.allTheta{pos} = sparse(Theta);
        out.allPhi{pos} = sparse(Phi);
    end

    
    if it>burn
    	out.Zcross = out.Zcross + 1.0*Z*Z';
    	out.Kcross = out.Kcross + 1.0*Theta*Theta';
        out.Jcross = out.Jcross + 1.0*Phi*Phi';
        out.nKall(it-burn) = nK;
        out.nJall(it-burn) = nJ;
        out.nIall(it-burn) = nI;
        out.nIall2(it-burn) = sum(sum(Z,1)>=2);
        out.nIall5(it-burn) = sum(sum(Z,1)>=5);
        out.nKall2(it-burn) = sum(sum(Theta,1)>=2);
        out.nKall5(it-burn) = sum(sum(Theta,1)>=5);
        out.nJall2(it-burn) = sum(sum(Phi,1)>=2);
        out.nJall5(it-burn) = sum(sum(Phi,1)>=5);
        
        
        if ~isempty(GeGo)
            [tempp,p] = midPval(GeGo,Z,'none');
            out.zp = out.zp + p;
            min(min(out.zp./(it-burn)))
            nsig = sum(sum(out.zp./(it-burn)<=0.1))
            [tempp,p] = midPval(GeGo,Theta,'none');
            out.kp = out.kp + p;
            nsig = sum(sum(out.kp./(it-burn)<=0.1))
            [tempp,p] = midPval(GeGo,Phi,'none');
            out.jp = out.jp + p;
            nsig = sum(sum(out.jp./(it-burn)<=0.1))
        end
    end
    
    if updatehyp
        % Update the hyper-paramters with MH
        % Alpha
%         alp0 = MHup(alp0,N,nI,1,10,hp);
        if strcmp(hypprior(1).type,'gamma')
            alp0 = GIup(alp0,N,nI,hypprior(1).par);
        else
            alp0 = MHup(alp0,N,nI,1,10,hypprior(1));
        end
%         gam0K = MHup(gam0K,N,nK,1,10,hp);
%         gam0J = MHup(gam0J,N,nJ,1,10,hp);
        if strcmp(hypprior(2).type,'gamma')
            gam0K = GIup(gam0K,N,nK,hypprior(2).par);
        else
            gam0K = MHup(gam0K,N,nK,1,10,hypprior(2));
        end
        if strcmp(hypprior(3).type,'gamma')
            gam0J = GIup(gam0J,N,nJ,hypprior(3).par);
        else
            gam0J = MHup(gam0J,N,nJ,1,10,hypprior(3));
        end
 
        if indiv_beta
            temp_indiv = sum(Z,1);
            no_tablesK = sum(iTablesK,1);
            no_tablesJ = sum(iTablesJ,1);
            if strcmp(hypprior(4).type,'gamma')
                for i = 1:nI
                    bet0K(i) = GIup(bet0K(i),temp_indiv(i),no_tablesK(i),hypprior(4).par);
                    bet0J(i) = GIup(bet0J(i),temp_indiv(i),no_tablesJ(i),hypprior(5).par);
                end
                    
            else
                for i = 1:nI
                    % How many in this restaurant
                    bet0K(i) = MHup(bet0K(i),temp_indiv(i),no_tablesK(i),1,10,hypprior(4));
                    bet0J(i) = MHup(bet0J(i),temp_indiv(i),no_tablesJ(i),1,10,hypprior(5));
                end
            end
        else
            bet0K(1) = MHup(bet0K(1),N,nTK,nI,10,hypprior(4));
            bet0J(1) = MHup(bet0J(1),N,nTJ,nI,10,hypprior(5));
            for i = 2:nI
                bet0K(i) = bet0K(1);
                bet0J(i) = bet0J(1);
            end
        end
        out.hypall(it,:) = [alp0,gam0K,gam0J,bet0K(1),bet0J(1)];
    end
    
    if verbose>1
        figure(1)
        imagesc(Z);
        figure(2)
        subplot(121)
        plotdata(X,Z);
        subplot(122)
        plotdata(Y,Z);
%         plotdata([X(:,1) Y(:,1)],Z);
%         figure(3)
%         plotdata(X,Theta);
%         figure(4)
%         plotdata(Y,Phi);
        figure(5);

        imagesc(Theta)
        figure(6)
        imagesc(Phi)
        figure(7)
        imagesc(out.Zcross);
%         figure(2)
%         for k = 1:nK
%             subplot(nK,1,k)
%             plot(X(find(Theta(:,k)),:)');
%         end
%         figure(3)
%         for j = 1:nJ
%             subplot(nJ,1,j)
%             plot(Y(find(Phi(:,j)),:)');
%         end
%         figure(4)
%         for i = 1:nI
%             subplot(nI,2,2*(i-1)+1)
%             plot(X(find(Z(:,i)),:)');
%             subplot(nI,2,2*(i-1)+2)
%             plot(Y(find(Z(:,i)),:)');
%         end
        figure(8)
        hold on
%         plot(it,alp0,'r.');
%         plot(it,gam0K,'b.');
%         plot(it,gam0J,'g.');
        plot(it,bet0K,'ko');
        plot(it,bet0J,'k^');
        drawnow
    end
%    fprintf('\nIID:');fprintf('\t%g',IID);
%    fprintf('\nKID:');fprintf('\t%g',KID);
%    fprintf('\nJID:');fprintf('\t%g',JID);
end
out.Z = Z;out.Theta = Theta;out.Phi = Phi;
if ~isempty(GeGo)
    out.zp = out.zp./(it-burn);
    out.kp = out.kp./(it-burn);
    out.jp = out.jp./(it-burn);
end
    
function val = MHup(val,N,k,po,Nsamp,hp)
    switch hp.type
        case 'igamma'
            oldpost = loghpostig(val,k,N,po,hp.par);
        case 'gamma'
            oldpost = loghpostg(val,k,N,po,hp.par);
    end
    for it = 1:Nsamp
        prop = val + randn;
        prop = abs(prop);
        switch hp.type
            case 'igamma'
                newpost = loghpostig(prop,k,N,po,hp.par);
            case 'gamma'
                newpost = loghpostg(prop,k,N,po,hp.par);
        end
        rat = exp(newpost - oldpost);
        if rand<=rat
            val = prop;
            oldpost = newpost;
        end
    end
    
function val=GIup(val,N,k,hp)
    % Sample from a beta with parameters val+1,n
    x = betarnd(val+1,N);
    pix = (hp(1) + k - 1)/(N*(hp(2)-log(x)) + hp(1) + k - 1);
    if rand<=pix
        val = gamrnd(hp(1)+k,1/(hp(2)-log(x)));
    else
        val = gamrnd(hp(1)+k-1,1/(hp(2)-log(x)));
    end
    
    
    function y = loghpostig(alp,k,n,I,hp)
        y = -(hp(1)+1).*log(alp) -hp(2)./alp;
        y = y + k.*log(alp) +I* gammaln(alp) -I* gammaln(n+alp);
        
    function y = loghpostg(alp,k,n,I,hp)
        y = (hp(1)-1).*log(alp) -hp(2).*alp;
        y = y + k.*log(alp) +I* gammaln(alp) -I* gammaln(n+alp);
    
function l = compstacklike(X,dcount,dsum,dcross,mu0,Lam0,v0,k0,bprior);
nx = size(X,1);
l = 0;
for n = 1:nx
    l = l + updateTpdf(X(n,:),mu0,Lam0,v0,k0,dsum,dcross,dcount,bprior);
    dcount = dcount + 1;
    dsum = dsum + X(n,:);
    dcross = dcross + X(n,:)'*X(n,:);
end

function l = compstacklikeDM(X,dsum,mhp)
nx = size(X,1);
l = 0;
for n = 1:nx
    l = l + comp_pred_dirichlet(X(n,:),dsum,mhp);
    dsum = dsum + X(n,:);
end

function TP = updateTpdf(X,mu0,Lam0,v0,k0,dsum,dcross,dcount,bprior)
    switch bprior
        case 'mv'
        kapn = k0 + dcount;
        vn = v0 + dcount;
        mun = (1/kapn)*(k0*mu0 + dsum);
        if dcount > 0
            Lamn = Lam0 + dcross - (1/dcount)*dsum'*dsum + ...
                ((k0*dcount)/(k0 + dcount))*(dsum./dcount - mu0)'*(dsum./dcount - mu0);
        else 
            Lamn = Lam0;
        end
        TP = compmvtpdf(X,mun,(Lamn*(1+kapn))./(kapn*(vn-size(X,2)+1)),vn-size(X,2)+1);
        case 'uv'
            % Univariate prior
            vn = v0 + dcount;
            kapn = k0 + dcount;
            % These next two are 1 x M
            mun = (1/kapn) * (k0*mu0 + dsum);
            if dcount > 0
                Lamn = v0*Lam0 + diag(dcross)' - (1/dcount)*dsum.^2+ ((k0*dcount)/kapn)*(dsum/dcount - mu0).^2;
            else
                Lamn = repmat(v0*Lam0,1,size(X,2));
            end
            TP = compuvtpdf(X,mun,Lamn * (kapn + 1)/(kapn*vn),vn);
    end

function T = compuvtpdf(X,mu,vs,v)
    [N,M] = size(X);
    const = gammaln((v+1)/2) - gammaln(v/2) - ...
        0.5*log(v*vs) - 0.5*log(pi);
    T = sum(const,2) - ((v+1)/2) * sum(log(...
        1 + repmat((1./(v*vs)),N,1).*(X-repmat(mu,N,1)).^2),2);
  
function P = comp_pred_dirichlet(X,clustsum,hyppars)
    [N,M] = size(X);
    P = repmat(gammaln(sum(hyppars+clustsum)) - sum(gammaln(hyppars + clustsum)),N,1) + ...
        sum(gammaln(repmat(hyppars + clustsum,N,1) + X),2) - gammaln(sum(repmat(hyppars + clustsum,N,1) + X,2));
    
function o = plotdata(X,As)
nK = size(As,2);
co = {'ro','go','bo','ko','mo',...
    'r.','g.','b.','k.','m.',...
    'rx','gx','bx','kx','mx',...
    'r+','g+','b+','k+','m+'};
hold off
for k = 1:nK
    pos = find(As(:,k));
    j = rem(k,length(co))+1;
    plot(X(pos,1),X(pos,2),co{j});
    hold on
end
function z = dirsamp(alp)
N = length(alp);
z = gamrnd(alp,1);
z = z./sum(z);
function z = multsamp(Pr)
C = cumsum(Pr);
z = zeros(1,length(Pr));
pos = find(rand<=C);
z(pos(1)) = 1;
function t = compmvtpdf(x,mu,sig,v)
[N,d] = size(x);
const = gammaln((v+d)/2) - gammaln(v/2) - (d/2)*log(v) - (d/2)*log(pi);
const = const - 0.5*log(det(sig));
t = repmat(const,N,1) - ((v+d)/2)*log(1 + ...
    (1/v)*diag((x-repmat(mu,N,1))*inv(sig)*(x-repmat(mu,N,1))'));
