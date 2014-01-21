function out=validSpot(str,loc)

for i=1:length(loc)
 out(i)=~incomment(str,loc(i)) && ~inastring_f(str,loc(i)) && ~inaDQstring_f(str,loc(i));
end