function outflag=inconverted(i,spot);
declare_globals
outflag=0;
for j=1:length(filename_all)
 temp=find(strcmp(funstrwords{i},filename_all{j}));
 if length(temp)>0
  for jj=1:length(temp)
   [howmany,subscripts,centercomma,parens]=hassubscript(i,temp(jj));
   %If in there, currently not used, unfinished
  end
 end
end
