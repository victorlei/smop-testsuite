clear all
nruns=3;
% create the data
% TE_multi(1,0,0,'sdir','Reference4TRENTOOL2','cpl',{'lin'},'nruns',nruns,'nx',[0.1],'saveraw','yes','newcalc')
 
DataPath=  '/data/projects/TransferEntropy/AR_simulations/SimData/Reference4TRENTOOL2_rag_VW/';
 
% collect the data into a structure
for run=1:nruns
    if run<10
    filename=strcat(DataPath,...
    'lin_g1.0_nx0.1_ny0.1_u21_r0',...
    num2str(run),...
    '_nT40_raw_data.mat');
    else
      filename=strcat(DataPath,...
    'lin_g1.0_nx0.1_ny0.1_u21_r',...
    num2str(run),...
    '_nT40_raw_data.mat');  
    end
    
    load(filename)
    rawdata{run}=data; clear data;
end

% administrative info
  Data.fsample= rawdata{1}.fsample

 % for each trial, get the data from the separate datasets and concanate
 % the in the chanel direction, stor in trail of the  super-dataset
Data.trial=cell(1,size(rawdata{1}.trial,2))

for currenttrial=1:size(rawdata{1}.trial,2)
 for run=1:nruns
  Data.trial{currenttrial}(end+1:end+2,:)=rawdata{1,run}.trial{currenttrial}
  
 end
 Data.time{currenttrial}=rawdata{1}.time{1}
end 
 
 
%   
%     for run=1:nruns
%     Data.trial{1}=rawdata{run}.trial{1}
%     Data.time{1}=rawdata{1}.time{1}
%     end
% 
%  
%  
%  for currenttrial=2:size(rawdata{1}.trial,1)
%     Data.trial{currenttrial}=rawdata{1}.trial{currenttrial} 
%     for run=2:nruns
%     Data.trial{currenttrial}=[Data.trial{currenttrial}; rawdata{run}.trial{currenttrial}]
%     Data.time{currenttrial}=rawdata{1}.time{1}
%     end
%  end

  for run=1:nruns
      Data.label{2*run-1}=strcat(num2str(run),'_',rawdata{run}.label{1});
      Data.label{2*run}=strcat(num2str(run),'_',rawdata{run}.label{2});
  end
 
  Data
  
Savename=strcat(DataPath,...
    'lin_g1.0_nx0.1_ny0.1_u21_','small_','_nT40_raw_data.mat');
save(Savename,'Data');
  
