% OK, so write out the converted code, and replace inline

while 1
 incBeg=regexpi(funstr,'f2matlab_begin_include_file');
 incBegF=find(~cellfun('isempty',incBeg));
 infEndStatement='f2matlab finish include file';
 incEnd=regexpi(funstr,infEndStatement);
 incEndF=find(~cellfun('isempty',incEnd));
 i=length(incBegF);
 if i==0, break, end
 j=find(incEndF>incBegF(end),1,'first');
 filestr=[];
 for ii=1:incEndF(j)-incBegF(i)-1, filestr=[filestr,funstr{incBegF(i)+ii},r]; end
 temp1={};
 incName=funstr{incEndF(j)}(length(infEndStatement)+6:end);
 disp(['found include file ',incName])
 [temp1{1},temp1{2},temp1{3}]=fileparts(incName);
 fid=fopen([temp1{1},filesep,temp1{2},'.m'],'w'); fprintf(fid,'%c',filestr);  fclose(fid);
 %funstr{incBegF(i)-1:incEndF(j)+1}
 funstr={funstr{1:incBegF(i)-1},temp1{2},funstr{incEndF(j)+1:end}};
 %funstr{incBegF(i)-1:incEndF(j)+1}
end % while 1
[funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr);
