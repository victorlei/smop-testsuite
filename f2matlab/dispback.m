function dispback(str)
% DISPBACK - Displays a string then backspaces to the beginning of the string.
%   dispback(str)

fprintf(1,[str,repmat(char(8),1,length(str))])

