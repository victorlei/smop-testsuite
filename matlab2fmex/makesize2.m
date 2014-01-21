function funlist=makesize2(wantsizes,funlist,len,r,typestr)
if wantsizes
 for ii=1:len
  if ((typestr(ii)=='r')|(typestr(ii)=='c')|(typestr(ii)=='i')|(typestr(ii)=='l'))
   funlist=[funlist,'        in_',num2str(ii),'_m=size(in_',num2str(ii),',dim=1); in_',num2str(ii),'_n=size(in_',num2str(ii),',dim=2)',r];
  elseif ((typestr(ii)=='w')|(typestr(ii)=='x')|(typestr(ii)=='y')|(typestr(ii)=='z'))
   funlist=[funlist,'        in_',num2str(ii),'_m=1; in_',num2str(ii),'_n=size(in_',num2str(ii),')',r];  
  else
   funlist=[funlist,'        in_',num2str(ii),'_m=1; in_',num2str(ii),'_n=1',r];
  end
 end
end
