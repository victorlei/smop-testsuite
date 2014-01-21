function [modlist,funlist,fun_info]=besselk_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='besselk';
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
 [modlist,funlist,len]=makeheader('besselk',1,typestr,modlist,funlist,r); 
%  if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
%   funlist=[funlist,'       real, pointer, dimension(:,:) :: out',r];
%   fun_info{2}='r';
%  else
  funlist=[funlist,'       complex, pointer, dimension(:,:) :: out',r];
  funlist=[funlist,'       real :: a, b, c, d',r];
  fun_info{2}='c';
%  end
  
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 for ii=1:min(len,2)
  if ((typestr(ii)=='r')|(typestr(ii)=='i')|(typestr(ii)=='c'))
   if ((typestr(ii)=='c')|(ii==2))
    funlist=[funlist,'       complex mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   else
    funlist=[funlist,'       real mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
   end
  else
   if ((typestr(ii)=='t')|(ii==2))
    funlist=[funlist,'       complex mat',num2str(ii),'(1,1)',r];
   else
    funlist=[funlist,'       real mat',num2str(ii),'(1,1)',r];
   end
  end
 end
 funlist=[funlist,'       integer dim,info,ierr,i,j',r]; 
 funlist=[funlist,'       real, parameter :: pi=3.14159265358979',r]; 
 if length(typestr)==2
  funlist=[funlist,'       dim=0',r] ;
 elseif length(typestr)==3
  switch typestr(3)
   case 'r'
    funlist=[funlist,'       dim=int(in_3(1,1))',r];
   case 'c'
    funlist=[funlist,'       dim=int(in_3(1,1))',r];
   case 'i'
    funlist=[funlist,'       dim=in_3(1,1)',r];
   case 's'
    funlist=[funlist,'       dim=int(in_3)',r];
   case 't'
    funlist=[funlist,'       dim=int(in_3)',r];
   case 'u'
    funlist=[funlist,'       dim=in_3',r];
  end  
 end
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
      funlist=[funlist,['        mat2=cmplx(in_2,0.0)',r]];
     case 'c'
      funlist=[funlist,['        mat2=in_2',r]];
     case 'i'
      funlist=[funlist,['        mat2=cmplx(in_2,0.0)',r]];
     case 's'
      funlist=[funlist,['        mat2(1,1)=cmplx(in_2,0.0)',r]];
     case 't'
      funlist=[funlist,['        mat2(1,1)=in_2',r]];
     case 'u'
      funlist=[funlist,['        mat2(1,1)=cmplx(in_2,0.0)',r]];
    end
   case 3
    switch typestr(ii)
     case 'r'
      funlist=[funlist,['        dim=int(in_3(1,1))',r]];
     case 'c'
      funlist=[funlist,['        dim=int(in_3(1,1))',r]];
     case 'i'
      funlist=[funlist,['        dim=(in_3(1,1))',r]];
     case 's'
      funlist=[funlist,['        dim=int(in_3)',r]];
     case 't'
      funlist=[funlist,['        dim=int(in_3)',r]];
     case 'u'
      funlist=[funlist,['        dim=(in_3)',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 kernel=[kernel,'        dim=dim+1',r];
 kernel=[kernel,'        if (in_1_m==1) then',r];
 kernel=[kernel,'         if (in_1_n==1) then',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! scalar x scalar',r];
 kernel=[kernel,'            allocate(out(1,1))',r];
 kernel=[kernel,'            if (mat1(1,1)>=0) then',r];
 kernel=[kernel,'             call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                  mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'             out(1,1)=cmplx(a,b)',r];
 kernel=[kernel,'            else',r];
 kernel=[kernel,'             call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                  -mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'             out(1,1)=cmplx(a,b)',r];
 kernel=[kernel,'            endif',r];
 kernel=[kernel,'           else                ! scalar x row',r];
 kernel=[kernel,'            allocate(out(1,in_2_n))',r];
 kernel=[kernel,'            do i=1,in_2_n',r];
 kernel=[kernel,'             if (mat1(1,1)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,i)),aimag(mat2(1,i)),',r];
 kernel=[kernel,'     &                   mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,i)),aimag(mat2(1,i)),',r];
 kernel=[kernel,'     &                   -mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_2_n==1) then ! scalar x column',r];
 kernel=[kernel,'            allocate(out(in_2_m,1))',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             if (mat1(1,1)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                   mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                   -mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! scalar x matrix',r];
 kernel=[kernel,'            allocate(out(in_2_m,in_2_n))',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             do j=1,in_2_n',r];
 kernel=[kernel,'              if (mat1(1,1)>=0) then',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,j)),aimag(mat2(i,j)),',r];
 kernel=[kernel,'     &                    mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              else',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,j)),aimag(mat2(i,j)),',r];
 kernel=[kernel,'     &                    -mat1(1,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              endif',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! row x scalar',r];
 kernel=[kernel,'            allocate(out(1,in_1_n))',r];
 kernel=[kernel,'            do i=1,in_1_n',r];
 kernel=[kernel,'             if (mat1(1,i)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                   mat1(1,i),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                   -mat1(1,i),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! row x row',r];
 kernel=[kernel,'            allocate(out(1,in_1_n))',r];
 kernel=[kernel,'            do i=1,in_1_n',r];
 kernel=[kernel,'             if (mat1(1,i)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,i)),aimag(mat2(1,i)),',r];
 kernel=[kernel,'     &                   mat1(1,i),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,i)),aimag(mat2(1,i)),',r];
 kernel=[kernel,'     &                   -mat1(1,i),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(1,i)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_2_n==1) then ! row x column',r];
 kernel=[kernel,'            allocate(out(in_2_m,in_1_n))',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             do j=1,in_1_n',r];
 kernel=[kernel,'              if (mat1(1,j)>=0) then',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                    mat1(1,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              else',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                    -mat1(1,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              endif',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! row x matrix',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        else',r];
 kernel=[kernel,'         if (in_1_n==1) then',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! column x scalar',r];
 kernel=[kernel,'            allocate(out(in_2_m,1))',r];
 kernel=[kernel,'            do i=1,in_2_m',r];
 kernel=[kernel,'             if (mat1(i,1)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                   mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                   -mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r]; 
 kernel=[kernel,'           else                ! column x row',r];
 kernel=[kernel,'            allocate(out(in_1_m,in_2_n))',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             do j=1,in_2_n',r];
 kernel=[kernel,'              if (mat1(i,1)>=0) then',r];
 kernel=[kernel,'               call zbesk(real(mat2(1,j)),aimag(mat2(1,j)),',r];
 kernel=[kernel,'     &                    mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              else',r];
 kernel=[kernel,'               call zbesk(real(mat2(1,j)),aimag(mat2(1,j)),',r];
 kernel=[kernel,'     &                    -mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              endif',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_1_n==1) then ! column x column',r];
 kernel=[kernel,'            allocate(out(in_1_m,1))',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             if (mat1(i,1)>=0) then',r];
 kernel=[kernel,'              call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                   mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             else',r];
 kernel=[kernel,'              call zbesk(real(mat2(i,1)),aimag(mat2(i,1)),',r];
 kernel=[kernel,'     &                   -mat1(i,1),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'              out(i,1)=cmplx(a,b)',r];
 kernel=[kernel,'             endif',r];
 kernel=[kernel,'            enddo',r]; 
 kernel=[kernel,'           else                ! column x matrix',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          if (in_2_m==1) then',r];
 kernel=[kernel,'           if (in_2_n==1) then ! matrix x scalar',r];
 kernel=[kernel,'            allocate(out(in_1_m,in_1_n))',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             do j=1,in_1_n',r];
 kernel=[kernel,'              if (mat1(i,j)>=0) then',r];
 kernel=[kernel,'               call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                    mat1(i,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              else',r];
 kernel=[kernel,'               call zbesk(real(mat2(1,1)),aimag(mat2(1,1)),',r];
 kernel=[kernel,'     &                    -mat1(i,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              endif',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           else                ! matrix x row',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          else',r];
 kernel=[kernel,'           if (in_2_n==1) then ! matrix x column',r];
 kernel=[kernel,'            ! error',r];
 kernel=[kernel,'           else                ! matrix x matrix',r];
 kernel=[kernel,'            allocate(out(in_1_m,in_1_n))',r];
 kernel=[kernel,'            do i=1,in_1_m',r];
 kernel=[kernel,'             do j=1,in_1_n',r];
 kernel=[kernel,'              if (mat1(i,j)>=0) then',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,j)),aimag(mat2(i,j)),',r];
 kernel=[kernel,'     &                    mat1(i,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              else',r];
 kernel=[kernel,'               call zbesk(real(mat2(i,j)),aimag(mat2(i,j)),',r];
 kernel=[kernel,'     &                    -mat1(i,j),dim,1,a,b,info,ierr)',r];
 kernel=[kernel,'               out(i,j)=cmplx(a,b)',r];
 kernel=[kernel,'              endif',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            enddo',r];
 kernel=[kernel,'           endif',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        endif',r];
 
  funlist=[funlist,kernel];
 
 %End of the function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,'       end subroutine ',funname,'f_',typestr,r];
 if strcmp(typestr,typestrlist{length(typestrlist)})
 else
  funlist=[funlist,r];
 end
end
%And end the module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist...
	 '       end interface ',funname,'f',char(10)];
fun_info{1}=1;
