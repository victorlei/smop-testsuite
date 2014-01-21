function [modlist,funlist,fun_info]=airy_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='airy';
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
 [modlist,funlist,len]=makeheader('airy',2,typestr,modlist,funlist,r); 
 if ((typestr(len)=='r')|(typestr(len)=='i')|(typestr(len)=='c'))
  if typestr(len)=='c'
   funlist=[funlist,'       complex, dimension(size(in_',num2str(len),',dim=1),size(in_',num2str(len),',dim=2)) :: out',r];
  else
   funlist=[funlist,'       real, dimension(size(in_',num2str(len),',dim=1),size(in_',num2str(len),',dim=2)) :: out',r];
  end
 else
  if typestr(len)=='t'
   funlist=[funlist,'       complex, dimension(1,1) :: out',r];
  else
   funlist=[funlist,'       real, dimension(1,1) :: out',r];   
  end
 end
 funlist=[funlist,'       real :: a, b',r];
 
 if ((typestr(len)=='t')|(typestr(len)=='c'))
  fun_info{2}='c';
 else
  fun_info{2}='r';
 end
  
 wantsizes=1;
 if wantsizes
  funlist=[funlist,'       integer '];
  for ii=len
   funlist=[funlist,'in_1_m, in_1_n',r];
  end
 end
 
 %Here we can insert any global vars or preliminary options %%%%%%
 for ii=len
  if ((typestr(ii)=='r')|(typestr(ii)=='i')|(typestr(ii)=='c'))
   if typestr(ii)=='c'
    funlist=[funlist,'       complex mat1(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   else
    funlist=[funlist,'       real mat1(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   end
  else
   if typestr(ii)=='t'
    funlist=[funlist,'       complex mat1(1,1)',r];
   else
    funlist=[funlist,'       real mat1(1,1)',r];
   end
  end
 end
 funlist=[funlist,'       integer id,info,ierr,i,j',r]; 
 funlist=[funlist,'       integer, parameter :: kode=1',r]; 
 if len==1
  funlist=[funlist,'       id=0',r] ;
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
 elseif len==2
  switch typestr(1)
   case 'r'
    funlist=[funlist,'       id=int(in_1(1,1))',r];
   case 'c'
    funlist=[funlist,'       id=int(in_1(1,1))',r];
   case 'i'
    funlist=[funlist,'       id=in_1(1,1)',r];
   case 's'
    funlist=[funlist,'       id=int(in_1)',r];
   case 't'
    funlist=[funlist,'       id=int(in_1)',r];
   case 'u'
    funlist=[funlist,'       id=in_1',r];
  end
  switch typestr(ii)
   case 'r'
    funlist=[funlist,['        mat1=in_2',r]];
   case 'c'
    funlist=[funlist,['        mat1=in_2',r]];
   case 'i'
    funlist=[funlist,['        mat1=in_2',r]];
   case 's'
    funlist=[funlist,['        mat1(1,1)=in_2',r]];
   case 't'
    funlist=[funlist,['        mat1(1,1)=in_2',r]];
   case 'u'
    funlist=[funlist,['        mat1(1,1)=in_2',r]];
  end  
 end
 if wantsizes
  for ii=len
   if ((typestr(ii)=='r')|(typestr(ii)=='c')|(typestr(ii)=='i'))
    funlist=[funlist,'        in_1_m=size(in_',num2str(ii),',dim=1); in_1_n=size(in_',num2str(ii),',dim=2)',r];
   else
    funlist=[funlist,'        in_1_m=1; in_1_n=1',r];
   end
  end
 end
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 if ((typestr(len)=='c')|(typestr(len)=='t'))
  kernel=[kernel,'          if ((id==0).or.(id==1)) then',r];
  kernel=[kernel,'           if (in_1_m==1) then',r];
  kernel=[kernel,'            if (in_1_n==1) then ! scalar',r];
  kernel=[kernel,'             call zairy(real(mat1(1,1)),aimag(mat1(1,1)),id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'             out(1,1)=cmplx(a,b)',r];
  kernel=[kernel,'            else                ! row',r];
  kernel=[kernel,'             do i=1,in_1_n',r];
  kernel=[kernel,'              call zairy(real(mat1(1,i)),aimag(mat1(1,i)),id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           else',r];
  kernel=[kernel,'            if (in_1_n==1) then ! column',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              call zairy(real(mat1(i,1)),aimag(mat1(i,1)),id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            else                ! matrix',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              do j=1,in_1_n',r];
  kernel=[kernel,'               call zairy(real(mat1(i,j)),aimag(mat1(i,j)),id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
  kernel=[kernel,'              enddo',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           endif',r];
  kernel=[kernel,'          else                ! Airy function of 2nd kind',r];
  kernel=[kernel,'           id=id-2',r];
  kernel=[kernel,'           if (in_1_m==1) then',r];
  kernel=[kernel,'            if (in_1_n==1) then ! scalar',r];
  kernel=[kernel,'             call zbiry(real(mat1(1,1)),aimag(mat1(1,1)),id,kode,a,b,ierr)',r];
  kernel=[kernel,'             out(1,1)=cmplx(a,b)',r];
  kernel=[kernel,'            else                ! row',r];
  kernel=[kernel,'             do i=1,in_1_n',r];
  kernel=[kernel,'              call zbiry(real(mat1(1,i)),aimag(mat1(1,i)),id,kode,a,b,ierr)',r];
  kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           else',r];
  kernel=[kernel,'            if (in_1_n==1) then ! column',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              call zbiry(real(mat1(i,1)),aimag(mat1(i,1)),id,kode,a,b,ierr)',r];
  kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            else                ! matrix',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              do j=1,in_1_n',r];
  kernel=[kernel,'               call zbiry(real(mat1(i,j)),aimag(mat1(i,j)),id,kode,a,b,ierr)',r];
  kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
  kernel=[kernel,'              enddo',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           endif',r];
  kernel=[kernel,'          endif',r];
 else
  kernel=[kernel,'          if ((id==0).or.(id==1)) then',r];
  kernel=[kernel,'           if (in_1_m==1) then',r];
  kernel=[kernel,'            if (in_1_n==1) then ! scalar',r];
  kernel=[kernel,'             call zairy(mat1(1,1),0.0,id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'             out(1,1)=a',r];
  kernel=[kernel,'            else                ! row',r];
  kernel=[kernel,'             do i=1,in_1_n',r];
  kernel=[kernel,'              call zairy(mat1(1,i),0.0,id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'              out(1,i)=a',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           else',r];
  kernel=[kernel,'            if (in_1_n==1) then ! column',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              call zairy(mat1(i,1),0.0,id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'              out(i,1)=a',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            else                ! matrix',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              do j=1,in_1_n',r];
  kernel=[kernel,'               call zairy(mat1(i,j),0.0,id,kode,a,b,info,ierr)',r];
  kernel=[kernel,'               out(i,j)=a',r];
  kernel=[kernel,'              enddo',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           endif',r];
  kernel=[kernel,'          else                ! Airy function of 2nd kind',r];
  kernel=[kernel,'           id=id-2',r];
  kernel=[kernel,'           if (in_1_m==1) then',r];
  kernel=[kernel,'            if (in_1_n==1) then ! scalar',r];
  kernel=[kernel,'             call zbiry(mat1(1,1),0.0,id,kode,a,b,ierr)',r];
  kernel=[kernel,'             out(1,1)=a',r];
  kernel=[kernel,'            else                ! row',r];
  kernel=[kernel,'             do i=1,in_1_n',r];
  kernel=[kernel,'              call zbiry(mat1(1,i),0.0,id,kode,a,b,ierr)',r];
  kernel=[kernel,'              out(1,i)=a',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           else',r];
  kernel=[kernel,'            if (in_1_n==1) then ! column',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              call zbiry(mat1(i,1),0.0,id,kode,a,b,ierr)',r];
  kernel=[kernel,'              out(i,1)=a',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            else                ! matrix',r];
  kernel=[kernel,'             do i=1,in_1_m',r];
  kernel=[kernel,'              do j=1,in_1_n',r];
  kernel=[kernel,'               call zbiry(mat1(i,j),0.0,id,kode,a,b,ierr)',r];
  kernel=[kernel,'               out(i,j)=a',r];
  kernel=[kernel,'              enddo',r];
  kernel=[kernel,'             enddo',r];
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           endif',r]; 
  kernel=[kernel,'          endif',r];
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
