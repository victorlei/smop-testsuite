function [linOut,wordOut]=findNextWord(linenum,str,linenumStop,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords)

if isempty(linenumStop), linenumStop=length(funstr); end

linOut=0; wordOut=0; goon=1;
direct=1; if linenumStop<linenum, direct=-1; end
for i=linenum:direct:linenumStop
 if goon
  for j=1:length(funstrwords{i})
   if strcmp(funstrwords{i}{j},str)
    if ~inastring_f(funstr{i},funstrwords_b{i}(j)) & ~incomment(funstr{i},funstrwords_b{i}(j))
     linOut=i;    wordOut=j;     goon=0;    break
    end % if ~inastring_f(funstr{i},
   end
  end % for j=1:length(funstrwords{i})
 end % if goon
end % for i=linenum:linenumStop


%'9999999999999',out,funstr{linenum},keyboard