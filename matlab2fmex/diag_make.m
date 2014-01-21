function [modlist,funlist,fun_info]=diag_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='diag';
modlist='';funlist='';
r=[char(10)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist,...
	 '       interface ',funname,'f',r];
if ~iscell(typestrlist)
 typestrlist={typestrlist};
end
for i=1:length(typestrlist)
 typestr=typestrlist{i};
 temp1='';
 for k=1:length(typestr)
  if ~isempty(find(strcmp(typestr(k),barr)))
   [a(k),b(k)]=find(strcmp(typestr(k),barr)); b2(k)=0;
  else
   [a(k),b(k)]=find(strcmp(typestr(k),barr2));b2(k)=1;
  end
 end
 if a(1)==1, temp1='complex'; elseif a(1)==2, temp1='real'; else temp1='integer'; end
 for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='x';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='w';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='y';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='z';end
 end 
 %The 1 at the end lets me make a 1-D input version.
 [modlist,funlist,len]=makeheader('diag',2,typestr,modlist,funlist,r,1);
 if ~any(strcmp(typestr(1),typs{5})) %scalar
  temp2='size(in_1)';  temp3='min(size(in_1,1),size(in_1,2))'; else temp2='1';temp3='1';
 end
 if length(typestr)==2
  if ~any(strcmp(typestr(2),typs{7})) %scalar
   temp4{1}='int(';  temp4{2}=')';  else   temp4{1}='';  temp4{2}='';
  end
 end
 if any(strcmp(typestr(1),typs{1})) %Full 2-D matrix, result is col
  if length(typestr)==1
   funlist=[funlist,'       integer, parameter :: in_2=0',r];
   funlist=[funlist,'       ',temp1,', dimension(',temp3,',1) :: out',r];
   fun_info{2}=barr2{a(1),2}; %col of same type result
  else
   funlist=[funlist,'       ',temp1,', dimension(',temp3,'-max(0,abs(in_2)-max(0,(size(in_1,2)-size(in_1,1))*in_2/abs(in_2))),1) :: out',r];
   fun_info{2}=barr2{a(1),2}; %col of same type result
  end
 else
  if length(typestr)==1
   funlist=[funlist,'       integer, parameter :: in_2=0',r];
   funlist=[funlist,'       ',temp1,', dimension(',temp2,',',temp2,') :: out',r];
   fun_info{2}=barr{a(1),1};
  else
   funlist=[funlist,'       ',temp1,', dimension(',temp2,'+abs(',temp4{1},'in_2',temp4{2},'),',temp2,'+abs(',temp4{1},'in_2',temp4{2},')) :: out',r];
   fun_info{2}=barr{a(1),1};   
  end
 end  
 
 %Here we can insert any global vars or preliminary options %%%%%%
 if any(strcmp(typestr(1),typs{1}))
  funlist=[funlist,'       integer c',r];
 end

 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 if any(strcmp(typestr(1),typs{1})) %Full 2-D matrix, result is col
  kernel=[kernel,'        if (in_2/=0) then',r];
  kernel=[kernel,'         c=',temp3,'-max(0,abs(in_2)-max(0,(size(in_1,2)-size(in_1,1))*in_2/abs(in_2)))',r];
  kernel=[kernel,'        else',r];
  kernel=[kernel,'         c=',temp3,'-max(0,abs(in_2))',r];
  kernel=[kernel,'        endif',r];
  kernel=[kernel,'        do i=1,c',r];
  kernel=[kernel,'         if (in_2>=0) then !k>=0',r];
  kernel=[kernel,'          out(i,1)=in_1(i,i+in_2)',r];
  kernel=[kernel,'         else             !k<0',r];
  kernel=[kernel,'          out(i,1)=in_1(i-in_2,i)',r];
  kernel=[kernel,'         endif',r];
  kernel=[kernel,'        enddo',r];
 else
  kernel=[kernel,'        out=0',r];
  kernel=[kernel,'        do i=1,in_1_n',r];
  kernel=[kernel,'         if (in_2>=0) then',r];
  kernel=[kernel,'          out(i,i+in_2)=in_1(i)',r];
  kernel=[kernel,'         else      !k<0',r];
  kernel=[kernel,'          out(i-in_2,i)=in_1(i)',r];
  kernel=[kernel,'         endif',r];
  kernel=[kernel,'        enddo',r];
 end
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
