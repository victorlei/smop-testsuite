function localVar=remLocalVar(localVar,name)
%removes a localVar unless protected

t1=strcmp(name,{localVar{:,1}});
thisLV=find(t1);
notThisLV=find(~t1);
if ~isempty(thisLV)
 if isempty(localVar{thisLV,8})
  localVar=localVar(notThisLV,:);
 end
end

%'[[[[[[[[[[2323232',localVar,kb