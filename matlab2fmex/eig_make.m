function [modlist,funlist,fun_info]=eig_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='eig';
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
  for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
 [modlist,funlist,len]=makeheader('eig',2,typestr,modlist,funlist,r); 
 if ((typestr(1)=='r')|(typestr(1)=='i')|(typestr(1)=='c'))
  funlist=[funlist,'       complex, dimension(size(in_1,dim=1),1) :: out',r];
 else
  funlist=[funlist,'       complex, dimension(1,1) :: out',r];
 end
 fun_info{2}='c';
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 if length(typestr)==1
  funlist=[funlist,'       integer :: info=0',r];
 end
 if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
  funlist=[funlist,'       complex, dimension(min(size(in_1,1),size(in_1,2))) :: s',r];
  funlist=[funlist,'       integer lwork',r];
  funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: u',r];
  funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: vt',r];
  funlist=[funlist,'       complex, dimension(size(in_1,1)*8) :: work',r];
  funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: mat',r];
  funlist=[funlist,'       complex, dimension(size(in_1,1)*2) :: rwork',r];
 else
  funlist=[funlist,'       complex, dimension(1) :: s',r];
  funlist=[funlist,'       complex, dimension(1,1) :: u',r];
  funlist=[funlist,'       complex, dimension(1,1) :: vt',r];
  funlist=[funlist,'       complex, dimension(8) :: work',r];
  funlist=[funlist,'       complex, dimension(1,1) :: mat',r];
  funlist=[funlist,'       complex, dimension(2) :: rwork',r];
 end
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 funlist=[funlist,'        lwork=in_1_m*8',r];
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 for ii=1:length(typestr)
  switch ii
   case 1
    switch typestr(ii)
     case 'r'
      funlist=[funlist,['        mat=in_1',r]];
     case 'c'
      funlist=[funlist,['        mat=in_1',r]];
     case 'i'
      funlist=[funlist,['        mat=in_1',r]];
     case 's'
      funlist=[funlist,['        mat(1,1)=in_1',r]];
     case 't'
      funlist=[funlist,['        mat(1,1)=in_1',r]];
     case 'u'
      funlist=[funlist,['        mat(1,1)=in_1',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 kernel=[kernel,'        call zgeev(''N'',''N'',in_1_m,mat,in_1_m,s,u,in_1_m,vt,in_1_m,work,lwork,rwork,info)',r];
 kernel=[kernel,'        out(:,1)=s',r];
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
