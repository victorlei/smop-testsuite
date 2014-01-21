function out=inaDQstring_f(str,loc)

out=[];
str(loc)='='; %so loc doesn't count if it is a "
temp=str=='"';
ftemp=find(temp); 

good=~inastring_f(str,ftemp);
temp(ftemp)=good;
ftemp=ftemp(good);

%%%temp2=inastring_f(str,ftemp);
%%%if ~isempty(temp2) && ~isempty(ftemp)
%%% %number of single quotes before this double quote must be odd to not count this DQ
%%% temp3=length(find(ftemp(ftemp<
%%% if temp2(1)<ftemp(1) && length(temp2(temp2<
%%%  temp(ftemp)=temp(ftemp).*(~temp2);
%%% end % if temp2(1)<ftemp(1)
%%%end % if ~isempty(temp2)

temp1=cumsum(temp);

for ii=1:length(loc)
 if (temp1(loc(ii))/2 ~= round(temp1(loc(ii))/2)) && length(ftemp)>1
  out(ii)=1;
 else
  out(ii)=0;
 end
end

%%%if strfind(str,'type')
%%% 'rrrrrrrrrrrr',str,out,kb
%%%end
