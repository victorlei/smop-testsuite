function [out,whichWord]=getLocalVarType(i,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords)

var_words={'complex';'integer';'logical';'character';'real';'double';'precision';'doubleprecision'};

out=var_words{5}; %default type real
whichWord=0;
for j=1:length(funstrwords{i})
 found=0;
 for ii=1:length(var_words)-4
  if strcmp(funstrwords{i}{j},var_words{ii})
   out=var_words{ii}; found=1; break
  end
 end % for jj=1:length(var_words)-4
 if found,  whichWord=j;break,  end
end % for j=1:length(funstrwords{i})
