% This script calls an old and a new version of TRENTOOL to compare them.

%% Clear 
clear all 
close all

clc

%% Selecting appropriate paths for fieldtrip and the TRENTOOL version
addpath('/data/common/FieldtripCurrent/fieldtrip-20111121');
addpath('/data/projects/TransferEntropy/code/BazarControlled/TRENTOOL1.0.2_unstable');
ft_defaults;

%% Loading of the data
load('/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2/testdata_small.mat');


%% Case 1 (Old version, maxdim, V, Cao)
% Creating the configuration for TE_prepare
cfg = [];
cfg.toi = [0 2.999];
cfg.sgncmb = {'1_x','1_y';'1_y','1_x';'2_x','2_y';'2_y','2_x';'3_x','3_y';'3_y','3_x'};
cfg.predicttime_u = 21;
cfg.TEcalctype = 'V';
cfg.optimizemethod = 'cao';
cfg.caodim = 1:8;
cfg.caokth_neighbors = 4;
cfg.trialselect = 'ACT';
cfg.actthrvalue = 120;
cfg.minnrtrials = 30;
cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';

Data_prepared = TEprepare(cfg,Data);

% Creating the configuration for TE_surrogatestats
cfg = [];
cfg.surrogatetype = 'trialshuffling';
cfg.shifttesttype = 'TEshift>TE';
cfg.fileidout = 'old_maxdim_V_cao';

TEsurrogatestats(cfg,Data_prepared)


%% Case 2 (Old version, maxdim, VW, Cao)
% Creating the configuration for TE_prepare
cfg = [];
cfg.toi = [0 2.999];
cfg.sgncmb = {'1_x','1_y';'1_y','1_x';'2_x','2_y';'2_y','2_x';'3_x','3_y';'3_y','3_x'};
cfg.predicttime_u = 21;
cfg.TEcalctype = 'VW';
cfg.optimizemethod = 'cao';
cfg.caodim = 1:8;
cfg.caokth_neighbors = 4;
cfg.trialselect = 'ACT';
cfg.actthrvalue = 120;
cfg.minnrtrials = 30;
cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';

Data_prepared = TEprepare(cfg,Data);

% Creating the configuration for TE_surrogatestats
cfg = [];
cfg.surrogatetype = 'trialshuffling';
cfg.shifttesttype = 'TEshift>TE';
cfg.fileidout = 'old_maxdim_VW_cao';

TEsurrogatestats(cfg,Data_prepared)


%% Case 3 (Old version, maxdim, V, Ragwitz)
% Creating the configuration for TE_prepare
cfg = [];
cfg.toi = [0 2.999];
cfg.sgncmb = {'1_x','1_y';'1_y','1_x';'2_x','2_y';'2_y','2_x';'3_x','3_y';'3_y','3_x'};
cfg.predicttime_u = 21;
cfg.TEcalctype = 'V';
cfg.optimizemethod = 'ragwitz';
cfg.ragdim = 1:10;
cfg.ragtaurange = [0.5 1.5];
cfg.flagNei = 'Mass';
cfg.sizeNei = 4;
cfg.repPred = 350;
cfg.trialselect = 'ACT';
cfg.actthrvalue = 120;
cfg.minnrtrials = 30;
cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';

Data_prepared = TEprepare(cfg,Data);

% Creating the configuration for TE_surrogatestats
cfg = [];
cfg.surrogatetype = 'trialshuffling';
cfg.shifttesttype = 'TEshift>TE';
cfg.fileidout = 'old_maxdim_V_rag';

TEsurrogatestats(cfg,Data_prepared)


%% Case 4 (Old version, maxdim, VW, Ragwitz)
% Creating the configuration for TE_prepare
cfg = [];
cfg.toi = [0 2.999];
cfg.sgncmb = {'1_x','1_y';'1_y','1_x';'2_x','2_y';'2_y','2_x';'3_x','3_y';'3_y','3_x'};
cfg.predicttime_u = 21;
cfg.TEcalctype = 'VW';
cfg.optimizemethod = 'ragwitz';
cfg.ragdim = 1:10;
cfg.ragtaurange = [0.5 1.5];
cfg.flagNei = 'Mass';
cfg.sizeNei = 4;
cfg.repPred = 350;
cfg.trialselect = 'ACT';
cfg.actthrvalue = 120;
cfg.minnrtrials = 30;
cfg.Path2TSTOOL = '/data/common/OpenTSTOOL_v1-2';

Data_prepared = TEprepare(cfg,Data);

% Creating the configuration for TE_surrogatestats
cfg = [];
cfg.surrogatetype = 'trialshuffling';
cfg.shifttesttype = 'TEshift>TE';
cfg.fileidout = 'old_maxdim_VW_rag';

TEsurrogatestats(cfg,Data_prepared)


