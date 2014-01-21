function funlist=makesize1(wantsizes,funlist,len,r)
if wantsizes
 funlist=[funlist,'       integer '];
 for ii=1:len
  if ii~=len
   funlist=[funlist,'in_',num2str(ii),'_m, in_',num2str(ii),'_n, '];
  else
   funlist=[funlist,'in_',num2str(ii),'_m, in_',num2str(ii),'_n',r];
  end
 end
end
