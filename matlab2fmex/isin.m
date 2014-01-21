function out=isin(in,list)
% Returns a 1 if the string in is in list
out=0;
if length(find(strcmp(in,list)))>0,out=1;end
