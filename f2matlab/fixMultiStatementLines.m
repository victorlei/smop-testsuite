function funstr=fixMultiStatementLines(funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,fs_good,funwords,filename,varPrefix,shapeVar,origVar)

%find top level semicolons with something after them and divide the line

for i=fliplr(fs_good)
 temp=strfind(funstr{i},';');
%%% if any(strcmp(funstrwords{i},'maxnam'))
%%%  funstr{i},'ffffffffffffff',kb
%%% end
 if length(temp>1)
  for j=length(temp):-1:1
   if length(funstrwords{i})>0 && isempty(strfind(funstr{i},varPrefix)) && ...
        isempty(regexp(funstr{i},[shapeVar,'\>'])) && ...
        isempty(regexp(funstr{i},[origVar,'\>'])) && ...
        validSpot(funstr{i},temp(j)) && ...
        inwhichlast_f(i,temp(j),funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,filename)==0
    %make sure it is not the ending semicolon
    temp1=find(~isspace(funstr{i}));
    if temp(j)~=temp1(end) 
     [funstr{i+1:end+1}]=deal(funstr{i:end});
     funstr{i}=funstr{i}(1:temp(j));
     funstr{i+1}=funstr{i+1}(temp(j)+1:end);
    end
   end % if ~inastring_f(funstr{i},
  end % for j=length(temp):-1:1
 end % if length(temp>1)
end % for i=fliplr(fs_good)
