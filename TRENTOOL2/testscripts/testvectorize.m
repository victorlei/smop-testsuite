
t1=1:3000;
t2=1:3000;

u = 20;
tau = 5;
dim = 10;
kth = 4;
TheilerT = 4;

cd('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL2.0/newstuff/')
addpath('/data/common/OpenTSTOOL_v1-2/tstoolbox/mex/mexa64/')
tic
[te1,mi1] = TECvalues(t1,t2,dim,tau,u,kth,TheilerT);
toc

tic
[te2,mi2] = TECvalues_test(t1,t2,dim,tau,u,kth,TheilerT);
toc


tdiff = te1-te2;
mdiff = mi1-mi2;

