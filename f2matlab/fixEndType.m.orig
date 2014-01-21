if length(funstrwords{i})==2 && strcmp('end',funstrwords{i}{1}) && strcmp('type',funstrwords{i}{2})
 %need to add the type name here
 for k=i:-1:1
  if length(funstrwords{k})>0
   if strcmp(funstrwords{k}{1},'type')
    funstr{i}=[strrep(funstr{i},';',''),' ',funstrwords{k}{2},';'];
    [s,fs_good]=updatefunstr_1line_f(funstr,fs_good,i);
    %'deeeeeeeeee',kb
    break
   end % if strcmp(funstrwords{k}{1},
  end % if length(funstrwords{k})>0
 end % for k=i:-1:1
end % if length(funstrwords{i})==2 && strcmp('end',
