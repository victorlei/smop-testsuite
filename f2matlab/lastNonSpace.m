function out=lastNonSpace(str,locs)

%finds the last non space location before loc(s)

temp=find(~isspace(str));

out=zeros(size(locs));

for ii=1:length(locs)
 temp1=temp(temp<locs(ii));
 if ~isempty(temp1)
  out(ii)=temp1(end);
 else % => inf?
  out(ii)=0;
 end
end % for ii=1:locs
 