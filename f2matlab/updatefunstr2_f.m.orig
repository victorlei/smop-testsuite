function [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr2_f(funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,fs_good,oneline)
s=length(funstr);
to0out={'.*','./','.''','.^'};
%ss='a11(de(2))-3.111d+21.*.322E-1*(-21.4e-2./b3.'') .or. 2.3+.12-2. - sqrt(fe_b(a(2)))';
%regexp(ss,'\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?','match'); % this gets the numbers
%regexp(ss,'\<[a-z_A-Z]\w*','match') % this gets the words
if nargin==1
 lo=1;hi=s;fs_good=[];
 funstrwords=cell(s,1);funstrwords_b=cell(s,1);funstrwords_e=cell(s,1);
 funstrnumbers=cell(s,1);funstrnumbers_b=cell(s,1);funstrnumbers_e=cell(s,1);
else
 lo=oneline;hi=oneline;
 fs_good=fs_good(fs_good<lo|fs_good>hi);
 funstrwords{oneline}=cell(0);
 funstrwords_b{oneline}=[];funstrwords_e{oneline}=[];
 funstrnumbers{oneline}=cell(0);
 funstrnumbers_b{oneline}=[];funstrnumbers_e{oneline}=[];
end

empty=cellfun('isempty',{funstr{lo:hi}});
bad1=strncmp({funstr{lo:hi}},'!',1);
bad2=strncmp({funstr{lo:hi}},'%',1);
good=~(empty|bad1|bad2);
ind=lo:hi;
fs_good=[fs_good,ind(good)];
goodind=ind(good);

if length(fs_good)/s > 1/2 & nargin==1 %do all the lines
  funstrnumbers=regexp(funstr,'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','match');
  extents=regexp(funstr,'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','tokenExtents');
  % [funstrnumbers_b]=deal(extents);
  for i=1:length(extents)
   temp=[extents{i}{:}];
   if ~isempty(temp)
    funstrnumbers_b{i}=temp(1:2:end);   funstrnumbers_e{i}=temp(2:2:end);
   end
  end
  funstrwords=regexp(funstr,'(\<[a-z_A-Z]\w*)','match');
  extents=regexp(funstr,'(\<[a-z_A-Z]\w*)','tokenExtents');
  for i=1:length(extents)
   temp=[extents{i}{:}];
   if ~isempty(temp)
    funstrwords_b{i}=temp(1:2:end);   funstrwords_e{i}=temp(2:2:end);
   end
  end
 else %do only those lines defined by goodind 
  funnum=regexp({funstr{goodind}},'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','match');
  [funstrnumbers{goodind}]=deal(funnum{:});
  extents=regexp({funstr{goodind}},'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','tokenExtents');
  for i=1:length(extents)
   temp=[extents{i}{:}];
   if ~isempty(temp)
    funstrnumbers_b{goodind(i)}=temp(1:2:end);   funstrnumbers_e{goodind(i)}=temp(2:2:end);
   end
  end
  funword=regexp({funstr{goodind}},'(\<[a-z_A-Z]\w*)','match');
  [funstrwords{goodind}]=deal(funword{:});
  extents=regexp({funstr{goodind}},'(\<[a-z_A-Z]\w*)','tokenExtents');
  for i=1:length(extents)
   temp=[extents{i}{:}];
   if ~isempty(temp)
    funstrwords_b{goodind(i)}=temp(1:2:end);   funstrwords_e{goodind(i)}=temp(2:2:end);
   end
  end

 %'fffffffffff',funstrnumbers{i}, funstr{i},w,kb
end




%%%if length(fs_good)/s < 1/2 %do all the lines
%%%  funnum=regexp({funstr{fs_good}},'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','match');
%%%  [funstrnumbers{fs_good}]=deal(funnum{:});
%%%  extents=regexp({funstr{fs_good}},'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','tokenExtents');
%%%  for i=1:length(extents)
%%%   temp=[extents{i}{:}];
%%%   if ~isempty(temp)
%%%    funstrnumbers_b{fs_good(i)}=temp(1:2:end);   funstrnumbers_e{fs_good(i)}=temp(2:2:end);
%%%   end
%%%  end
%%%  funword=regexp({funstr{fs_good}},'(\<[a-z_A-Z]\w*)','match');
%%%  [funstrwords{fs_good}]=deal(funword{:});
%%%  extents=regexp({funstr{fs_good}},'(\<[a-z_A-Z]\w*)','tokenExtents');
%%%  for i=1:length(extents)
%%%   temp=[extents{i}{:}];
%%%   if ~isempty(temp)
%%%    funstrwords_b{fs_good(i)}=temp(1:2:end);   funstrwords_e{fs_good(i)}=temp(2:2:end);
%%%   end
%%%  end
%%% else %do only those lines defined by fs_good
%%%  funstrnumbers=regexp(funstr,'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','match');
%%%  extents=regexp(funstr,'(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?)','tokenExtents');
%%%  % [funstrnumbers_b]=deal(extents);
%%%  for i=1:length(extents)
%%%   temp=[extents{i}{:}];
%%%   if ~isempty(temp)
%%%    funstrnumbers_b{i}=temp(1:2:end);   funstrnumbers_e{i}=temp(2:2:end);
%%%   end
%%%  end
%%%  funstrwords=regexp(funstr,'(\<[a-z_A-Z]\w*)','match');
%%%  extents=regexp(funstr,'(\<[a-z_A-Z]\w*)','tokenExtents');
%%%  for i=1:length(extents)
%%%   temp=[extents{i}{:}];
%%%   if ~isempty(temp)
%%%    funstrwords_b{i}=temp(1:2:end);   funstrwords_e{i}=temp(2:2:end);
%%%   end
%%%  end
%%%
%%% %'fffffffffff',funstrnumbers{i}, funstr{i},w,kb
%%%end




fs_good=sort(fs_good);


'fffffffffff',funstrnumbers{i}, funstr{i},kb
