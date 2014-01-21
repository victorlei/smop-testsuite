function [vararginout,vararginin,vararginlocal]=parsevarargin(argin);
temp=inf;temp2=inf;lenargin=length(argin);
vararginout=cell(0,0);vararginin=cell(0,0);vararginlocal=cell(0,0);
for i=1:lenargin
 if isstr(argin{i})
  if strcmpi(argin{i},'in')
   temp=i;
  elseif strcmpi(argin{i},'local')
   temp2=i;
  end
 end
end
[a,b]=min([temp temp2 lenargin+1]);
for i=1:(a-1)
 vararginout{i}=argin{i};
end
if ~isinf(temp)
 if ~isinf(temp2)
  count=1;
  for i=(temp+1):(temp2-1)
   vararginin{count}=argin{i};
   count=count+1;
  end
  count=1;
  for i=(temp2+1):lenargin
   vararginlocal{count}=argin{i};
   count=count+1;
  end
 else
  count=1;
  for i=(temp+1):lenargin
   vararginin{count}=argin{i};
   count=count+1;
  end
 end
elseif ~isinf(temp2)
 count=1;
 for i=(temp2+1):lenargin
  vararginlocal{count}=argin{i};
  count=count+1;
 end
end
