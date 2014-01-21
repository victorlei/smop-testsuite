function varargout=readf(fid,fmtStr,n)
% function varargout=readf(fid,varargin),  Catches string fid's
if isnumeric(fid)
 if n==1
  [varargout]=textscan(fid,fmtStr);
 else
  [varargout{1:n}]=textscan(fid,fmtStr);
 end
elseif ischar(fid)
 [varargout{1:n}]=strread(fid,fmtStr);
%%% [varargout]=strread(fid,fmtStr);
%%% varargout=varargout{1};
%%% 'sssssss',kb
end
end

