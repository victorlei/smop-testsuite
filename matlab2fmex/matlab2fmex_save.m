function out=matlab2fmex_save(callstr)
%function out=matlab2fmex_savews(callstr)
%This will create a temporary file which will save the 
%  workspace of the requested function at the line 
%  %matlab2femx_save
% Before saving the workspace, the script [funname,'_save.m'] where funname is the name of the function to be converted will be run. See the README for more details.

[funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s]=updatefunstr_full({callstr});
temp1=findstr(callstr,'=');
if isempty(temp1), temp1=funstrwords_b{1}(1)-1; end
start=find(funstrwords_b{1}>temp1);
start=start(1);

r=char(10);
funname=funstrwords{1}{start};
filename=[funname,'.m'];
if exist(filename)==2
 fid=fopen(filename); filestr=fscanf(fid,'%c'); fclose(fid);
 filestr=[filestr,r];
 rets=findstr(filestr,r);
 temp1='matlab2fmex_save';
 temp2=findstr(filestr,temp1);
 if isempty(temp2)
  insloc=length(filestr);
 else
  temp3=rets(rets>temp2);
  insloc=temp3(1);
 end
 extra=[];
 if exist([funname,'_save.m'])==2
  fid=fopen([funname,'_save.m']); extra=fscanf(fid,'%c'); fclose(fid);
 end
 out=[filestr(1:insloc),extra,r,'save ',funname,r,'return',r,filestr(insloc:end)];
 fid=fopen([funname,'_m2ftemp.m'],'w'); fprintf(fid,'%c',out); fclose(fid);
 rehash
 evalin('caller',[funstr{1}(1:funstrwords_b{1}(start)-1),funname,'_m2ftemp',funstr{1}(funstrwords_b{1}(start)+length(funname):end)]);
 if isunix
  unix(['rm -f ',funname,'_m2ftemp.m']);
 else
  dos(['del ',funname,'_m2ftemp.m']);
 end
else
 error(['I can''t find the file ',filename,'...']);
end
