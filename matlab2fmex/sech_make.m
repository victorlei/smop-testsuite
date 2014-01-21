function [modlist,funlist,fun_info]=sech_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='sech';
modlist='';funlist='';  r=[char(10)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist,...
	 '       interface ',funname,'f',r];
if ~iscell(typestrlist)
 typestrlist={typestrlist};
end
for i=1:length(typestrlist)
 typestr=typestrlist{i};
  for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
 [modlist,funlist,len]=makeheader(funname,2,typestr,modlist,funlist,r); 
 arr={'r';'c';'i'}; noi={'r';'c';'s';'t'}; com={'c';'t'};
 temp1='complex';
 if strcmp(typestr,arr)
  if strcmp(typestr,'c') ;  fun_info{2}='c';  else   fun_info{2}='r';  end
 else
  if strcmp(typestr,'t') ;  fun_info{2}='t';  else   fun_info{2}='s';  end
 end
 if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
  temp1='real';
 end
 if any(strcmp(typestr,arr))
  funlist=[funlist,'       ',temp1,', dimension(size(in_1,1),size(in_1,2)) :: out',r];
 else
  funlist=[funlist,'       ',temp1,' :: out',r];
 end
 funlist=[funlist,'       complex :: i=(0.0,1.0)',r]; 
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel='';
 kernel=[kernel,'        out=1/cos(i*in_1)',r];
 funlist=[funlist,kernel];

 %End of the function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,'       end function ',funname,'f_',typestr,r];
 if strcmp(typestr,typestrlist{length(typestrlist)})
 else
  funlist=[funlist,r];
 end
end
%And end the module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist...
	 '       end interface ',funname,'f',char(10)];
fun_info{1}=0;
