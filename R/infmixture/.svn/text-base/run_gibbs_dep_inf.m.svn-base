%% Example for how to run the code
clear all;close all;

%% Set the size of K's and J's
nK = 5;
nJ = 4;
Lam0 = eye(2);
mu0 = [0 0];
v0 = 3;
kap0 = 0.1;

%% Sample the Gaussian components
muK = [-3 3;3 3;3 -3; -3 -3;0 0];
for k = 1:nK
    sigmaK(:,:,k) = 0.2*eye(2);
end

GamK = repmat(0.1,1,nK);
GamJ = repmat(0.1,1,nJ);
muJ = [-4 4;-2 2;2 -2;4 -4];
for j = 1:nJ
    sigmaJ(:,:,j) = 0.2*eye(2);
end
nI = 3;

jd = eye(4);
kd = [0 0 0 0 1;0.5 0 0.5 0 0;0 0.5 0 0.5 0];
jd = [1 0 0 0;0 0.5 0.5 0;0 0 0 1];

x = [];y = [];
N = 200;
Ni = ceil(N/nI);
pos = 0;
Ni = [20;80;40];
for i = 1:nI
    for n = 1:Ni(i)
        pos = pos + 1;
        cs =cumsum(kd(i,:));
        k = find(rand<=cs);
        k = k(1);
        cs =cumsum(jd(i,:));
        j = find(rand<=cs);
        j = j(1);

        x = [x;gausssamp(muK(k,:),sigmaK(:,:,k),1)];
        y = [y;gausssamp(muJ(j,:),sigmaJ(:,:,j),1)];
    end
end




%% Run the algorithm

%% Hyperpar setting
a = 50;b = 10;
hyp(1).type = 'gamma';hyp(1).par = [a,b];
hyp(2).type = 'gamma';hyp(2).par = [a,b];
hyp(3).type = 'gamma';hyp(3).par = [a,b];
hyp(4).type = 'gamma';hyp(4).par = [a,b];
hyp(5).type = 'gamma';hyp(5).par = [a,b];

% Run the sampler; see code for the meaning of the parameters
out = gibbs_dep_infall_marg_tables(x,y,...
    'no_its',1000,'burn',0,'infmix',[1 1 1],...
    'prior','uv','lam0k',1,'lam0j',1,'v0k',1,'v0j',1,'v0k',1,...
    'updatehyp',1,'verbose',1,'hypprior',hyp,'k',20,'j',20,...
    'comp_type',{'gauss','gauss'});


%% Some plotting
use = [1:1000];
%% NI
figure(1)
[a,b] = hist(out.nIall(use),3:13);
f = figure(1);
bar(b,a./sum(a))
h = get(1,'children');
set(h,'linewidth',2,'fontsize',24)
xlabel('$I$','interpreter','latex','fontsize',24)
ylabel('$p(I)$','interpreter','latex','fontsize',24)
xlim([2.5 9.5])

%% Plot the data
figure(2)
h = subplot(121)
hold off
co = {'ro','bs','gv'};
t = [];
for i = 1:length(Ni)
t = [t;repmat(i,Ni(i),1)];
end
for i = 1:3
    pos = find(t==i);
    plot(x(pos,1),x(pos,2),co{i});
    hold on
end
% h = get(1,'children');
set(h,'linewidth',2,'fontsize',14,'xtick',[],'ytick',[])
xlabel('$\mathbf{x}$','interpreter','latex','fontsize',30)
xlim([-5 5]);
ylim([-5 5])


h = subplot(122)
co = {'ro','bs','gv'};
for i = 1:3
    pos = find(t==i);
    plot(y(pos,1),y(pos,2),co{i});
    hold on
end
% h = get(2,'children');
set(h,'linewidth',2,'fontsize',14,'xtick',[],'ytick',[])
xlabel('$\mathbf{y}$','interpreter','latex','fontsize',30)
xlim([-5 5]);
ylim([-5 5])
[a,b] = hist([out.nIall out.nIall2],[3:11]);
a = a./1000;
ba = bar(b,a);
set(ba(1),'facecolor',[1 1 1])
set(ba(2),'facecolor',[0 0 0])
l = legend('$\geq 1$','$\geq 2$')
set(l,'interpreter','latex','fontsiz',20);
set(gca,'linewidth',2,'fontsize',14)
xlabel('$I$','interpreter','latex','fontsize',20)
ylabel('$p(I)$','interpreter','latex','fontsize',20)

%%
[a,b] = hist([out.nKall out.nJall],[3:8]);
a = a./1000;
ba = bar(b,a);
set(ba(1),'facecolor',[1 1 1])
set(ba(2),'facecolor',[0 0 0])
l = legend('$K\rightarrow x$','$J\rightarrow y$')
set(l,'interpreter','latex','fontsiz',20);
set(gca,'linewidth',2,'fontsize',14)
xlabel('$K,J$','interpreter','latex','fontsize',20)
ylabel('$p(K),p(J)$','interpreter','latex','fontsize',20)


%% Autocorrelation of i
use = [100:1000];
is = out.nIall(use);
is = is - mean(is);
is = is ./ std(is);
for lag = 0:400
    ac(lag+1) = mean(is(1:length(is)-lag).*is(lag+1:end));
end
figure(4)
bar([0:100],ac(1:101));
xlim([-0.5 20.5])
h = get(2,'children');
set(h,'linewidth',2,'fontsize',40)
%xlabel('$lag$','interpreter','latex','fontsize',50);
%ylabel('$autocorrelation$','interpreter','latex','fontsize',50);




%% How about this
figure(6)
h = subplot(2,8,[1 2])
imagesc(out.Theta'*diag(out.Z(:,1))*out.Phi)
set(h,'linewidth',2,'xtick',[],'ytick',[])
title('$i=1$','interpreter','latex','fontsize',20)
colormap(gray)
h = subplot(2,8,[4 5])
imagesc(out.Theta'*diag(out.Z(:,2))*out.Phi)
set(h,'linewidth',2,'xtick',[],'ytick',[])
title('$i=2$','interpreter','latex','fontsize',20)
h = subplot(2,8,[7 8])
imagesc(out.Theta'*diag(out.Z(:,3))*out.Phi)
set(h,'linewidth',2,'xtick',[],'ytick',[])
title('$i=3$','interpreter','latex','fontsize',20)
co = {'bs','ro','k^'};
for i = 1:3
    pos = find(out.Z(:,i));
    h=subplot(2,8,6+3*i);
    plot(x(pos,1),x(pos,2),co{i});
    xlim([-5 5])
    ylim([-5 5])
    xlabel('$\mathbf{x}$','interpreter','latex','fontsize',20)
    set(h,'linewidth',2,'xtick',[],'ytick',[]);
    h = subplot(2,8,6+3*i+1);
    plot(y(pos,1),y(pos,2),co{i});
    xlim([-5 5])
    ylim([-5 5])
    set(h,'linewidth',2,'xtick',[],'ytick',[]);
    xlabel('$\mathbf{y}$','interpreter','latex','fontsize',20)
end


%% Draw contingency table
figure(7)
CT = out.Theta'*out.Phi;
imagesc(CT)
colormap('gray')