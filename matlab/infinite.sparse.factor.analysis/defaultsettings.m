function settings=defaultsettings(Y)

settings.a=1; % noise variance hyperparameters
settings.b=1;
settings.a0=1; % hyperparameters on b
settings.b0=1;
settings.d=1; % factor variance hyperparameters
settings.c0=1;
settings.d0=1;
settings.e=1; % IBP alpha hyperparamters
settings.f=1;
settings.arho=1;
settings.brho=1; % a prior expect 1/4 of genes to be involved in the FA
settings.iterations=1000;
settings.lapFlag=false;
settings.plotFlag=false;
settings.plot=false;
settings.D=size(Y,1);
settings.N=size(Y,2);
settings.verbose=1;
settings.mv=0;
settings.addNoiseToMV=0; 
settings.bfrm=0;
settings.store_samples=0;
settings.sampleNoiseVariance=1;
settings.sampleAlpha=1;
settings.sampleBeta=0;
settings.sampleNewG=1;
settings.compareX=0;
settings.DP=1;
settings.altpriors=0;

settings.basedir='local_results/';

% Interesting settings
settings.isotropic=0; % enforce isotropic noise?
settings.sparse=1; % sparse prior on mixing coefficients?
settings.np=1; % nonparametric-use IBP? Only valid if sparse
settings.perfactor=1; % Per factor covariances? With c=1 corresponds to ARD prior
settings.learnscale=1; % Hierarchical prior on the factor covariances? Only valid if perfactor is true
settings.c=1; % 1 corresponds to an ARD prior. 2 corresponds to not. 
settings.K=0; % Whether we should initialise with a specific number of features
settings.betaFlag=false; % use two-parameter ibp?
settings.shareNoise=false; % share power across noise dimensions? 
settings.addOneFeature=0; % Between 0,1: mixing proportion of adding 1 feature
settings.accelerateFeatures=1; % Multiplied by rate in prior for new feature proposal
settings.variableSelection=0; % variable selection: still experimental
settings.fokoue=0; % use Fokoue's method
settings.richGetRicher=1; % use rich gets richer property of the IBP