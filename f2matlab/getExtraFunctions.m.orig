function out=getExtraFunctions(extraFunctions)

out='';
r=char(10);

if any(extraFunctions==1)
 out=[out,' function tf = strlexcmp(a, b)',r];
 out=[out,'%STRLEXCMP Lexicographic comparison of two strings.',r];
 out=[out,'%',r];
 out=[out,'%   STRLEXCMP(A, B) returns -1, 0, or 1 depending on whether the left argument',r];
 out=[out,'%   is stringwise less than, equal to, or greater than the right argument.',r];
 out=[out,'%',r];
 out=[out,'%   This is a MATLAB version of the Perl `cmp'' operator.',r];
 out=[out,'%',r];
 out=[out,'%   See also EQ, ISEQUAL.',r];
 out=[out,'',r];
 out=[out,'%   Author:      Peter J. Acklam',r];
 out=[out,'%   Time-stamp:  2004-09-22 19:49:47 +0200',r];
 out=[out,'%   E-mail:      pjacklam@online.no',r];
 out=[out,'%   URL:         http://home.online.no/~pjacklam',r];
 out=[out,'',r];
 out=[out,'   % check arguments',r];
 out=[out,'   error(nargchk(2, 2, nargin));',r];
 out=[out,'   if ~ischar(a) || ~ischar(b)',r];
 out=[out,'      error(''Both arguments must be char arrays (strings).'');',r];
 out=[out,'   end',r];
 out=[out,'',r];
 out=[out,'   % get lengths of strings',r];
 out=[out,'   na = length(a);',r];
 out=[out,'   nb = length(b);',r];
 out=[out,'   n = min(na, nb);',r];
 out=[out,'',r];
 out=[out,'   % find characters that differ',r];
 out=[out,'   k = find(a(1:n) ~= b(1:n));',r];
 out=[out,'   if isempty(k)',r];
 out=[out,'      % all characters are identical -- compare lengths',r];
 out=[out,'      tf = sign(na - nb);',r];
 out=[out,'   else',r];
 out=[out,'      % compare first character that is different',r];
 out=[out,'      k = k(1);',r];
 out=[out,'      tf = sign(a(k) - b(k));',r];
 out=[out,'   end',r];
 out=[out,' end',r];
 
end