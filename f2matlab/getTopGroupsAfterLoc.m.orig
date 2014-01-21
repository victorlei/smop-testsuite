function [groups,topCommas]=getTopGroupsAfterLoc(str,loc,delim);

if nargin<3, delim=''; end

%'dddddddddddd11',str,keyboard

topCommas=find(str==',');
topCommas=topCommas(topCommas>loc);
tl=str=='(' | str=='[';
tl(find(tl))=~inastring_f(str,find(tl));
tl(find(tl))=~inastring2_f(str,find(tl));
tl(find(tl))=~inaDQstring_f(str,find(tl));
tr=str==')' | str==']';
tr(find(tr))=~inastring_f(str,find(tr));
tr(find(tr))=~inastring2_f(str,find(tr));
tr(find(tr))=~inaDQstring_f(str,find(tr));
tb=tl-tr;   tlevel=[0,cumsum(tb)];
topCommas=topCommas(tlevel(topCommas+1)==tlevel(loc+1));
%don't count topCommas that occur after the tlevel has risen above loc
tt=find(tlevel<tlevel(loc+1));
tt=tt(tt>loc);
if ~isempty(tt)
 topCommas=topCommas(topCommas<tt(1));
end

if ~isempty(delim)
 dl=str==delim;
 dl(1:loc)=false;
 dlc=cumsum(dl);
 topCommas=topCommas(dlc(topCommas)/2==floor(dlc(topCommas)/2));
end

good=zeros(1,length(topCommas));
for ii=1:length(topCommas)
 good(ii)=~inastring_f(str,topCommas(ii)) & ~inaDQstring_f(str,topCommas(ii));% & ~inastring2_f(str,topCommas(ii));
end
%'dddddddddddd22',str,keyboard
topCommas=topCommas(find(good));
groups=cell(0);
howmany=length(topCommas)+1;
for ii=1:length(topCommas)+1
 if ii==1
  if ~isempty(topCommas)
   groups{ii}=str(loc+1:topCommas(1)-1);
  else %in case there is only 1 thing to print and no topCommas
   groups{1}=str(loc+1:end);
  end
 elseif ii==length(topCommas)+1
  groups{ii}=str(topCommas(end)+1:end);
 else
  groups{ii}=str(topCommas(ii-1)+1:topCommas(ii)-1);
 end
 temp=find(~isspace(groups{ii}));
 if groups{ii}(temp(end))==';'
  groups{ii}=[groups{ii}(1:temp(end)-1),groups{ii}(temp(end)+1:end)];
 end
end

%'dddddddddddd',str,groups,keyboard