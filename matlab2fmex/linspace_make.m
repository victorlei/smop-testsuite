function [modlist,funlist,fun_info]=linspace_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='linspace';
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
 typestr2=typestr;
 for m=1:length(typestr)
  temp2=any(strcmp(typestr(m),{'d','m'})); if temp2,typestr2(m)='c';end
  temp2=any(strcmp(typestr(m),{'e','n'})); if temp2,typestr2(m)='r';end
  temp2=any(strcmp(typestr(m),{'f','o'})); if temp2,typestr2(m)='i';end
  temp2=any(strcmp(typestr(m),{'g','p'})); if temp2,typestr2(m)='l';end
 end
 [modlist,funlist,len]=makeheader('linspace',2,typestr,modlist,funlist,r);
 fun_info{2}='i';    temp{1}='integer';
 for k=1:length(typestr)
  if any(strcmp(typestr(k),typs{3}))
   fun_info{2}='c';  temp{1}='complex';
  elseif any(strcmp(typestr(k),typs{6}))
   fun_info{2}='r';  temp{1}='real';
  end
 end
 if length(typestr)<3
  funlist=[funlist,' ',temp{1},', dimension(1,100) :: out',r];
 else
  temp1{1,1}='';              temp1{2,1}='';                temp1{2,2}='';
  if any(strcmp(typestr(3),typs{11})) %2-D
   temp1{1,1}='(1,1)';
  elseif any(strcmp(typestr(3),typs{4}))%1-D
   temp1{1,1}='(1)';
  end
  if ~any(strcmp(typestr(3),typs{7})) %Not an integer
   temp1{2,1}='NInt(';                temp1{2,2}=')';
  end
  
  funlist=[funlist,'  ',temp{1},', dimension(1,',temp1{2,1},'in_3',temp1{1,1},'',temp1{2,2},') :: out',r];
  
 end
 wantsizes=0;
 funlist=makesize1(wantsizes,funlist,len,r);
 %Here we can insert any global vars or preliminary options %%%%%%
 prelim='';
 for ii=1:2
  if any(strcmp(typestr(ii),typs{3}))
   prelim=[prelim,'  complex temp',num2str(ii),r];
  else
   prelim=[prelim,'  real temp',num2str(ii),r];
  end
 end
 prelim=[prelim,'  real increment',r];
 prelim=[prelim,'  integer temp3, i',r];
 funlist=[funlist,prelim];
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 for ii=1:length(typestr)
  temp1{1,1}='';
  if any(strcmp(typestr(3),typs{11})) %2-D
   temp1{1,1}='(1,1)';
  elseif any(strcmp(typestr(3),typs{4}))%1-D
   temp1{1,1}='(1)';
  end
  funlist=[funlist,['  temp',num2str(ii),'=in_',num2str(ii),temp1{1,1}],r];
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 if length(typestr)<3
  kernel=['  temp3=100',r];
 else
  kernel=[''];
 end
 kernel=[kernel,'  increment=(temp2-temp1)/(dble(temp3)-1)',r];
 kernel=[kernel,'  out(1,1)=temp1',r];
 kernel=[kernel,'  do i=2,temp3',r];
 kernel=[kernel,'   out(1,i)=out(1,i-1)+increment',r];
 kernel=[kernel,'  end do',r];
 funlist=[funlist,kernel]; 
 %End of the function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,' end function ',funname,'f_',typestr2,r];
 if strcmp(typestr,typestrlist{length(typestrlist)})
 else
  funlist=[funlist,r];
 end
end%Typestr loop
%And end the module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist...
	 ' end interface ',funname,'f',char(10)];
fun_info{1}=0;
