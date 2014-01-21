% OK, scan through finding
%   include '
% then replace inline



while 1 %do this until no more includes (break condition is inside takeCareOfIncludeFiles)
 [temp1,temp2]=regexpi(filestr,'include\s*''');
 temp3=find(filestr=='''');
 rets=findstr(r,filestr);  rets=[0 rets];

 if length(temp1)==0, break, end
 temp10=0;
 for i=length(temp1):-1:1
  %is this in a valid spot
  if isempty(find(filestr(rets(find(rets<temp1(i),1,'last')):temp1(i))=='!')) && ...
       mod(length(find(filestr(rets(find(rets<temp1(i),1,'last')):temp1(i))=='''')),2)~=1 && ...
       mod(length(find(filestr(rets(find(rets<temp1(i),1,'last')):temp1(i))=='"')),2)~=1
   temp10=1;
   temp4=filestr(temp2(i)+1:temp3(find(temp3>temp2(i),1,'first'))-1);
   [temp5,temp6,temp7]=fileparts(temp4);
   if ~isempty(temp5), temp4=[temp6,temp7]; end
   if isempty(temp7) %no extension
    temp4=[temp4,'.for'];
   end
   disp(['   found include file ',temp4,' ... inserting']);
   [temp9{1},temp9{2},temp9{3}]=fileparts(which(temp4));
   temp9{4}=fullfile(temp9{1},[temp9{2},temp9{3}]);
   if ~isempty(temp9{1})
    fid=fopen(temp9{4}); temp8=fscanf(fid,'%c'); fclose(fid);
   else
    error(['had a problem finding include file ',temp4,r,'Is that directory on the Matlab path?']);
   end
   %splice this include in with delimiting lines so we can replace it later
   filestr=[filestr(1:rets(find(rets<temp1(i),1,'last'))),...
            'f2matlab_begin_include_file',r,r,...
            temp8,...
            '!!! f2matlab finish include file ',temp9{4},...
            filestr(rets(find(rets>temp1(i),1,'first')):end),...
           ];
   %filestr(rets(find(rets<temp1(i),1,'last'))-50:rets(find(rets<temp1(i),1,'last'))+800)
  end
 end
 if temp10==0, break, end %all remaining includes are in comments
 %'iiiiiiiiii',temp1,kb
end