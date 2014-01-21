function out=empt0(in)
% converts empty to 0, otherwise leaves

out=in;
if isempty(out), out=0; end