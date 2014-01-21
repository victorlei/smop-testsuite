function str=convertconditional(str)
goon=1;
temp=findstr(str,'~=');
while ~isempty(temp)
 str=[str(1:temp(1)-1),'/=',str(temp(1)+2:length(str))];
 temp=findstr(str,'~=');
end

temp=findstr(str,'~');
while ~isempty(temp)
 str=[str(1:temp(1)-1),'.not.',str(temp(1)+1:length(str))];
 temp=findstr(str,'~');
end

temp=findstr(str,'|');
while ~isempty(temp)
 str=[str(1:temp(1)-1),'.or.',str(temp(1)+1:length(str))];
 temp=findstr(str,'|');
end

temp=findstr(str,'&');
while ~isempty(temp)
 str=[str(1:temp(1)-1),'.and.',str(temp(1)+1:length(str))];
 temp=findstr(str,'&');
end
