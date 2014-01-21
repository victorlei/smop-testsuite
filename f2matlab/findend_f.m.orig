function out=findend_f(linenum,s,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords)
left=1;right=0;
i=linenum+1;
while ((left~=right)&(i<=s))
 if ~isempty(funstrwords{i})
  if ~inastring_f(funstr{i},funstrwords_b{i}(1)) & ~incomment(funstr{i},funstrwords_b{i}(1))
   if any(strcmpi(funstrwords{i}(1),{'for';'do';'while';'if';'switch';'where'}))
    left=left+1; %'1111111',i,funstr{i}
    if strcmpi(funstrwords{i}(1),'if')
     if strcmpi(funstrwords{i}(end),'end')
      right=right+1; %'22222222',i,funstr{i}
     end
    end
   end
   if strcmpi(funstrwords{i}(1),'end')
    right=right+1; %'333333333',i,funstr{i}
   end
  end % if ~inastring_f(funstr{i},
 end
 i=i+1;
end
out=i-1;

%'9999999999999',out,funstr{linenum},keyboard