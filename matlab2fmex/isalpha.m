function out=isalpha
out=0;
if isunix
 [a,b]=system('uname -a | grep alpha');
 if ~isempty(strfind(lower(b),'alpha')), out=1; end
end
