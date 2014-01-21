function out=findstrexact(strin,strbig)
%%%out=[];
%%%if length(strbig)>=length(strin)
%%% for i=1:(length(strbig)-length(strin)+1)
%%%  if strncmp(strin,strbig(i:length(strbig)),length(strin))
%%%   out=[out i];
%%%  end
%%% end
%%%end

%out=regexp(strbig,strin);

out=strfind(strbig,strin);