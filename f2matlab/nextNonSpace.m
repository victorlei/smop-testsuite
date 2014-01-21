function out=nextNonSpace(str,locs)

%finds the next non space location after loc(s)

temp=find(~isspace(str));

out=zeros(size(locs));

for ii=1:length(locs)
 temp1=temp(temp>locs(ii));
 if ~isempty(temp1)
  out(ii)=temp1(1);
 else % => inf?
  %out(ii)=length(str);
  out(ii)=length(str)+1; %original
 end
end % for ii=1:locs
 