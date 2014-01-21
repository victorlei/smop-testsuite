% now put use muodule name in each subprg in that module
temp=0; %number of lines inserted so far
for i=size(sublist,1):-1:1
 if ~isempty(sublist{i,7}) % does it have a parent?
  temp1=find(strcmp({sublist{:,1}},sublist{i,7}));
  if ~isempty(temp1)
   if length(temp1)>1
    error(['found more than one module with name ',sublist{temp1(1),1}]);
   end
   if strcmp(sublist{temp1(1),4},'module') % is the parent a module?
    temp2=sublist{i,9};
    funstr(temp2+2:end+1)=funstr(temp2+1:end);
    funstr{temp2+1}=['use ',sublist{temp1(1),1}];    temp=1;
    %'piiiiiiiiip',{funstr{temp2-5:temp2+15}}',kb
   end % if strcmp(sublist{temp1(1),
  else
   error(['couldn''t find the parent module declaration for ',sublist{i,7}])
  end % if ~isempty(temp1)
 end % if ~isempty(sublist{i,
end % for i=1:size(sublist,
if temp>0
 [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr);
  [sublist,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good]=findendSub_f([],sublist,s,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good,funwords,var_words);
end

%'frfrfr',sublist,kb

%%%% now put use muodule name in each subprg in that module
%%%temp=0; %number of lines inserted so far
%%%for i=1:size(sublist,1)
%%% if ~isempty(sublist{i,7}) % does it have a parent?
%%%  temp1=find(strcmp({sublist{:,1}},sublist{i,7}));
%%%  if ~isempty(temp1)
%%%   if strcmp(sublist{temp1(1),4},'module') % is the parent a module?
%%%    temp2=sublist{i,2}+temp;
%%%    funstr(temp2+2:end+1)=funstr(temp2+1:end);
%%%    % [funstr{temp2+2:end+1}]=deal(funstr{temp2+1:end});
%%%    funstr{temp2+1}=['use ',sublist{temp1(1),1}];
%%%    temp=temp+1;
%%%   end % if strcmp(sublist{temp1(1),
%%%  end % if ~isempty(temp1)
%%% end % if ~isempty(sublist{i,
%%%end % for i=1:size(sublist,
%%%if temp>0
%%% [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr);
%%%  [sublist,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good]=findendSub_f([],sublist,s,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good,funwords,var_words);
%%%end


% move all the module stuff to the end
temp=find(strcmp({sublist{:,4}},'program'));
if ~isempty(temp)
 if temp>1
  fid=max([sublist{1:temp-1,3}]);
  funstr={funstr{fid+1:end},funstr{1:fid}};
  [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr);
  %'ffffffffffff',kb
  [sublist,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good]=findendSub_f([],sublist,s,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good,funwords,var_words);
 end % if temp>1
end % if ~isempty(temp)

% need a new file with just the mod decs
for i=size(sublist,1):-1:1
 if strcmp(sublist{i,4},'module')
  tempstr='';
  if isempty(sublist{i,5}) % no contains
   temp2=sublist{i,3};
  else
   temp2=sublist{i,5};
  end % if isempty(sublist{i,
  for j=sublist{i,2}:temp2-1
   %for j=sublist{i,2}+1:temp2-1
   tempstr=[tempstr,funstr{j},r];
  end % for j=
%%%  if length(tempstr)==0
%%%   tempstr=[tempstr,r];
%%%   %tempstr=[tempstr,'real placeholderVar000',r];
%%%  end
  if want_lc
   fid=fopen([regexprep(sublist{i,1},tempcc,changeCase,'ignorecase'),'.m'],'w');
   fprintf(fid,'%c',tempstr);   fclose(fid);
  else
   fid=fopen([sublist{i,1},'.m'],'w');  fprintf(fid,'%c',tempstr);   fclose(fid);
  end
  % now delete these lines from funstr
  if isempty(sublist{i,5}) % no contains
   funstr={funstr{1:sublist{i,2}-1},funstr{sublist{i,3}+1:end}};
  else
   funstr={funstr{1:sublist{i,2}-1},funstr{sublist{i,5}+1:sublist{i,3}-1},funstr{sublist{i,3}+1:end}};
  end % if isempty(sublist{i,
 end % if strcmp(sublist{i,
end % for i=1:size(sublist,

%comment out the blockdatas
for i=size(sublist,1):-1:1
 if strcmp(sublist{i,4},'blockdata')
  funstr={funstr{1:sublist{i,2}-1},funstr{sublist{i,3}+1:end}};
 end % if strcmp(sublist{i,
end % for i=size(sublist,

%sublist,funstr.',tempstr,'-----------2',kb
[funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr);
funstr_all=funstr;
s_all=s;
fs_good_all=fs_good;




%%%% write a findFortEnd to find the end of each block based on:
%%%END [PROGRAM [program-name]] 
%%% END [FUNCTION [function-name]] 
%%% END [SUBROUTINE [subroutine-name]] 
%%% END [MODULE [module-name]] 
%%% END [BLOCK DATA [block-data-name]]
%%% 
%%% "end do" should be all enddo's so they aren't a prblem
%%% 
%%% then --
%%% - put the module vars into their own little file. make all the vars global somehow
%%% - put "use modname" in each function and sub in the module
%%% - then delete module...contains, and the end module statement
%%% - make "use modname" just run the resultant script
%%% 
%%% 