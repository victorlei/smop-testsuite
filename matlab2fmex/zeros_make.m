function [modlist,funlist,fun_info]=zeros_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='zeros';
modlist='';funlist='';
r=[char(10)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist,...
	 ' interface ',funname,'f',r];
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
 [modlist,funlist,len]=makeheader('zeros',2,typestr,modlist,funlist,r); 
 arr={'r';'c';'i'}; noi={'r';'c';'s';'t'}; com={'c';'t'};
 temp1='real'; fun_info{2}='r';
 temp2{1,1}=''; temp2{1,2}=''; temp2{2,1}=''; temp2{2,2}='';
 temp3{1,1}=''; temp3{1,2}=''; temp3{2,1}=''; temp3{2,2}='';
 if length(typestr)==1
  temp2{1,1}='1';        temp2{2,1}='1';
  if any(strcmp(typestr(1),noi)),          temp3{1,1}='NInt'; temp3{2,1}='NInt'; end
  if any(strcmp(typestr(1),com)),          temp3{1,2}='real'; temp3{2,1}='real'; end
  if any(strcmp(typestr(1),arr)),          temp2{1,2}='(1,1)';temp2{2,2}='(1,2)';end
 elseif length(typestr)==2
  temp2{1,1}='1';        temp2{2,1}='2';
  if any(strcmp(typestr(1),arr)),                             temp2{1,2}='(1,1)';end
  if any(strcmp(typestr(2),arr)),                             temp2{2,2}='(1,1)';end
  if any(strcmp(typestr(1),noi)),          temp3{1,1}='NInt';                    end
  if any(strcmp(typestr(1),com)),          temp3{1,2}='real';                    end
  if any(strcmp(typestr(2),noi)),                             temp3{2,1}='NInt'; end
  if any(strcmp(typestr(2),com)),                             temp3{2,2}='real'; end
 end
 funlist=[funlist,'  ',temp1,', dimension(',temp3{1,1},'(',temp3{1,2},'(in_',temp2{1,1},temp2{1,2},')),',temp3{2,1},'(',temp3{2,2},'(in_',temp2{2,1},temp2{2,2},'))) :: out',r];

 fun_info{2}='r';
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=['  out=0',r];
 funlist=[funlist,kernel];
 
 %End of the function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,' end function ',funname,'f_',typestr,r];
 if strcmp(typestr,typestrlist{length(typestrlist)})
 else
  funlist=[funlist,r];
 end
end
%And end the module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist...
	 ' end interface ',funname,'f',char(10)];
fun_info{1}=0;
