function out=breakOffFirstFormat(in)

%either a string is first, or a number is first

in=strtrim(in);

legit=find(~isspace(in));

out={in};

if in(1)==''''
 nQ=nnz(in=='''')-1;
 for ii=1:length(nQ)
  nextQ=findNext(1,'''',in);
  nextNonSpace=legit(legit>nextQ);
  if ~isempty(nextNonSpace)
   nextNonSpace=nextNonSpace(1);
   if nextNonSpace~=''''
    if ~inastring_f(in,nextNonSpace)
     out{1}=in(1:nextNonSpace-1);
     out{2}=in(nextNonSpace:end);
     break
    end
   end
  end
 end
else
 if any(in=='''')
  nextQ=findNext(1,'''',in);

  goon=1;
  temp=find(in=='(');
  if ~isempty(temp)
   temp2=find(in=='''');
   if ~isempty(temp2)
    if min(temp)<min(temp2)
     goon=0;
    end
   end
  end
  if goon
   out{1}=in(1:nextQ-1);
   out{2}=in(nextQ:end);
  end
 end
end

%'\\\\\\\\\\\\\22', in,out,kb

if length(out)<2 & length(in)>1
 if any(in=='/')
  temp=find(in=='/');
  temp1=find(~inastring_f(in,temp));
  temp2=find(in=='(');
  
  if ~isempty(temp1)
   goon=1;
   if ~isempty(temp2) && temp2(1)<temp(temp1(1)), goon=0; end
   if goon
    temp1=temp1(1);
    if temp(temp1)==1 %Then it's the first one
     out{1}='/';
     out{2}=in(2:end);
    else
     out{1}=in(1:temp(temp1)-1);
     out{2}=in(temp(temp1):end);
    end
   end
  end
 end
end

%%%if ~isempty(strfind(in,'3(e15.6,1x,'))
%'\\\\\\\\\\\\\', in,out,kb
%%%end
