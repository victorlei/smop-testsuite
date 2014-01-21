function out=iskeep(str)
if ~isempty(str)
 logicalops={'.not.';'.and.';'.or.'};
 out=((isletter(str))|(str=='_')|(str=='.')|((str>47)&(str<58)));
 temp1=findstr(str,'.');
 if ~isempty(temp1) %Don't want to count periods which are part of logical words
  for j=1:length(logicalops)
   temp2=findstr(str,logicalops{j});
   if ~isempty(temp2)
    for i=1:length(temp1)
     if any(temp1(i)==temp2+length(logicalops{j})-1)
      out(temp1(i))=logical(0);
     end
    end
   end
  end
 end
else
 out=[];
end
