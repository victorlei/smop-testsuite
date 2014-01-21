function [modlist,funlist,fun_info]=svd_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='svd';
modlist='';funlist='';
r=[char(10)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist,...
	 '       interface ',funname,'f',r];
if ~iscell(typestrlist)
 typestrlist={typestrlist};
end
wantsizes=1;
for i=1:length(typestrlist)
 typestr=typestrlist{i};
  for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
 [modlist,funlist,len]=makeheader('svd',2,typestr,modlist,funlist,r); 
 if any(strcmp(typestr(1),typs{11}))
  funlist=[funlist,'       real, dimension(min(size(in_1,1),size(in_1,2)),1) :: out',r];
 else
  funlist=[funlist,'       real, dimension(1,1) :: out',r];
 end
 fun_info{2}='r';
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 if length(typestr)==1
  funlist=[funlist,'       integer :: info=0',r];
 end
 if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
  funlist=[funlist,'       real, dimension(min(size(in_1,1),size(in_1,2))) :: s',r];
  funlist=[funlist,'       integer lwork',r];
  if ~((typestr(1)=='c')|(typestr(1)=='t'))
   funlist=[funlist,'       real, dimension(size(in_1,1),size(in_1,2)) :: u',r];
   funlist=[funlist,'       real, dimension(size(in_1,1),size(in_1,2)) :: vt',r];
   funlist=[funlist,'       real, dimension(max(3*min(size(in_1,1),size(in_1,2))+max(size(in_1,1),size(in_1,2)),5*min(size(in_1,1),size(in_1,2))-4)*3) :: work',r];
   funlist=[funlist,'       real, dimension(size(in_1,1),size(in_1,2)) :: mat',r];
  else
   funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: u',r];
   funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: vt',r];
   funlist=[funlist,'       complex, dimension(max(3*min(size(in_1,1),size(in_1,2))+max(size(in_1,1),size(in_1,2)),5*min(size(in_1,1),size(in_1,2))-4)*3) :: work',r];
   funlist=[funlist,'       complex, dimension(size(in_1,1),size(in_1,2)) :: mat',r];
   funlist=[funlist,'       complex, dimension(max(3*min(size(in_1,1),size(in_1,2)),5*min(size(in_1,1),size(in_1,2))-4)) :: rwork',r];
  end
 else
  if ~((typestr(1)=='c')|(typestr(1)=='t'))
   funlist=[funlist,'       real, dimension(1) :: s',r];
   funlist=[funlist,'       real, dimension(1,1) :: u',r];
   funlist=[funlist,'       real, dimension(1,1) :: vt',r];
   funlist=[funlist,'       integer lwork',r];
   funlist=[funlist,'       real, dimension(12) :: work',r];
   funlist=[funlist,'       real, dimension(1,1) :: mat',r];
  else
   funlist=[funlist,'       complex, dimension(1) :: s',r];
   funlist=[funlist,'       complex, dimension(1,1) :: u',r];
   funlist=[funlist,'       complex, dimension(1,1) :: vt',r];
   funlist=[funlist,'       integer lwork',r];
   funlist=[funlist,'       complex, dimension(12) :: work',r];
   funlist=[funlist,'       complex, dimension(1,1) :: mat',r];
   funlist=[funlist,'       complex, dimension(3) :: rwork',r];
  end
 end
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 funlist=[funlist,'       lwork=max(3*min(in_1_m,in_1_n)+max(in_1_m,in_1_n),5*min(in_1_m,in_1_n)-4)*3',r];
 
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
 if ~((typestr(1)=='c')|(typestr(1)=='t'))
  kernel=[kernel,'        call dgesvd(''N'',''O'',in_1_m,in_1_n,mat,in_1_m,s,u,in_1_m,vt,in_1_m,work,lwork,info)',r]; 
 else
  kernel=[kernel,'        call zgesvd(''N'',''O'',in_1_m,in_1_n,mat,in_1_m,s,u,in_1_m,vt,in_1_m,work,lwork,rwork,info)',r];
 end
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
