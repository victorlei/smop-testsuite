function out=findend(linenum,s,funstr,funstrwords,keywords,keywordsbegin)
left=1;right=0;
i=linenum+1;
while ((left~=right)&(i<=s))
 if length(find(strcmpi(funstrwords{i}(1),{'for';'do';'while';'if';'switch'})))==1
  left=left+1; 
 end
 if length(find(strcmpi(funstrwords{i}(1),'end')))>0, right=right+1; end
 i=i+1;
end
out=i-1;
