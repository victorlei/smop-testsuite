function [modlist,funlist,fun_info]=gammainc_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='gammainc';
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
 [modlist,funlist,len]=makeheader('gammainc',2,typestr,modlist,funlist,r);
 if ((typestr(1)=='r')|(typestr(1)=='i')|(typestr(1)=='c'))
  if ((typestr(2)=='r')|(typestr(2)=='i')|(typestr(2)=='c'))
   funlist=[funlist,'       real, dimension(max(size(in_1,dim=1),size(in_2,dim=1)),max(size(in_1,dim=2),size(in_2,dim=2))) :: out',r];
  else
   funlist=[funlist,'       real, dimension(max(size(in_1,dim=1),1),max(size(in_1,dim=2),1)) :: out',r];
  end
 else
  if ((typestr(2)=='r')|(typestr(2)=='i')|(typestr(2)=='c'))
   funlist=[funlist,'       real, dimension(max(size(in_2,dim=1),1),max(size(in_2,dim=2),1)) :: out',r];
  else
   funlist=[funlist,'       real, dimension(1,1) :: out',r];
  end
 end
 fun_info{2}='r';
  
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 for ii=1:len
  if ((typestr(ii)=='r')|(typestr(ii)=='i')|(typestr(ii)=='c'))
   if typestr(ii)=='c'
    funlist=[funlist,'       complex mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   else
    funlist=[funlist,'       real mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   end
  else
   if typestr(ii)=='t'
    funlist=[funlist,'       complex mat',num2str(ii),'(1,1)',r];
   else
    funlist=[funlist,'       real mat',num2str(ii),'(1,1)',r];
   end
  end
 end
 funlist=[funlist,'       integer i,j',r];  
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 for ii=1:length(typestr)
  switch ii
   case 1
    switch typestr(ii)
     case 'r'
      funlist=[funlist,['        mat1=in_1',r]];
     case 'c'
      funlist=[funlist,['        mat1=in_1',r]];
     case 'i'
      funlist=[funlist,['        mat1=in_1',r]];
     case 's'
      funlist=[funlist,['        mat1(1,1)=in_1',r]];
     case 't'
      funlist=[funlist,['        mat1(1,1)=in_1',r]];
     case 'u'
      funlist=[funlist,['        mat1(1,1)=in_1',r]];
    end
   case 2
    switch typestr(ii)
     case 'r'
      funlist=[funlist,['        mat2=in_2',r]];
     case 'c'
      funlist=[funlist,['        mat2=in_2',r]];
     case 'i'
      funlist=[funlist,['        mat2=in_2',r]];
     case 's'
      funlist=[funlist,['        mat2(1,1)=in_2',r]];
     case 't'
      funlist=[funlist,['        mat2(1,1)=in_2',r]];
     case 'u'
      funlist=[funlist,['        mat2(1,1)=in_2',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 kernel=[kernel,'        if (in_1_m==1) then',r];
 kernel=[kernel,'         if (in_1_n==1) then',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! scalar x scalar',r];
 kernel=[kernel,'           out(1,1)=dgamit(mat2(1,1),mat1(1,1))/mat1(1,1)**(-mat2(1,1))',r];
 kernel=[kernel,'           else                ! scalar x row',r];
 kernel=[kernel,'            do i=1,in_2_n',r];
 kernel=[kernel,'             out(1,i)=dgamit(mat2(1,i),mat1(1,1))/mat1(1,1)**(-mat2(1,i))',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_2_n==1) then ! scalar x column',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             out(i,1)=dgamit(mat2(i,1),mat1(1,1))/mat1(1,1)**(-mat2(i,1))',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! scalar x matrix',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             do j=1,in_2_n',r];
 kernel=[kernel,'              out(i,j)=dgamit(mat2(i,j),mat1(1,1))/mat1(1,1)**(-mat2(i,j))',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! row x scalar',r];
 kernel=[kernel,'            do i=1,in_1_n',r];
 kernel=[kernel,'             out(1,i)=dgamit(mat2(1,1),mat1(1,i))/mat1(1,i)**(-mat2(1,1))',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! row x row',r];
 kernel=[kernel,'            do i=1,in_1_n',r];
 kernel=[kernel,'             out(1,i)=dgamit(mat2(1,i),mat1(1,i))/mat1(1,i)**(-mat2(1,i))',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        else',r];
 kernel=[kernel,'         if (in_1_n==1) then',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! column x scalar',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             out(i,1)=dgamit(mat2(1,1),mat1(i,1))/mat1(i,1)**(-mat2(1,1))',r];
 kernel=[kernel,'            enddo',r]; 
 kernel=[kernel,'           else                ! column x row',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_1_n==1) then ! column x column',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             out(i,1)=dgamit(mat2(i,1),mat1(i,1))/mat1(i,1)**(-mat2(i,1))',r];
 kernel=[kernel,'            enddo',r]; 
 kernel=[kernel,'           else                ! column x matrix',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! matrix x scalar',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             do j=1,in_1_n',r];
 kernel=[kernel,'              out(i,j)=dgamit(mat2(1,1),mat1(i,j))/mat1(i,j)**(-mat2(1,1))',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! matrix x row',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_2_n==1) then ! matrix x column',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           else                ! matrix x matrix',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             do j=1,in_1_n',r];
 kernel=[kernel,'              out(i,j)=dgamit(mat2(i,j),mat1(i,j))/mat1(i,j)**(-mat2(i,j))',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        endif',r];
 
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
