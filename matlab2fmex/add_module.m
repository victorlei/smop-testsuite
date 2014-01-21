function oldints=add_module(oldints,newints,modname)
r=char(10);
contains=findstr('contains',newints);
if isempty(findstr(modname,oldints{1}))
 rets=findstr(r,newints);
 occur=findstr(modname,newints);
 first=occur(1);
 start=rets(rets<first);
 start=start(end)+1;
 last=occur(occur<contains);
 last=last(end);
 finish=rets(rets>last);
 finish=finish(1);
 oldints{1}=[oldints{1},newints(start:finish)];
 first=occur(occur>contains);
 first=first(1);
 start=rets(rets<first);
 start=start(end)+1;
 last=occur(end);
 finish=rets(rets>last);
 finish=finish(1);
 oldints{2}=[oldints{2},newints(start:finish)];
end
