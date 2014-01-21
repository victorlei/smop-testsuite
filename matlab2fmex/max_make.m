function [modlist,funlist,fun_info]=max_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='max';
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
 len=length(typestr);
 modlist=[modlist,...
	  '        module procedure ',funname,'f_',typestr,r];
 funlist=[funlist,...
	  '       subroutine ',funname,'f_',typestr,'(out,'];
 for ii=1:len 
  if ii<len
   funlist=[funlist,'in_',num2str(ii),','];
  else
   funlist=[funlist,'in_',num2str(ii),')',r];
  end
 end
 for ii=1:len
  switch typestr(ii)
   case 'r'
    funlist=[funlist,'       real, dimension(:,:) :: in_',num2str(ii),r];
   case 'c'
    funlist=[funlist,'       complex, dimension(:,:) :: in_',num2str(ii),r];
    funlist=[funlist,'       complex, dimension(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2)) :: temp',num2str(ii),r];
   case 'i'
    funlist=[funlist,'       integer, dimension(:,:) :: in_',num2str(ii),r];
   case 's'
    funlist=[funlist,'       real :: in_',num2str(ii),r];
   case 't'
    funlist=[funlist,'       complex :: in_',num2str(ii),r];
   case 'u'
    funlist=[funlist,'       integer :: in_',num2str(ii),r];
  end
 end
 if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
  funlist=[funlist,'       real, pointer, dimension(:,:) :: out',r];
  fun_info{2}='r';
 else
  funlist=[funlist,'       complex, pointer, dimension(:,:) :: out',r];
  fun_info{2}='c';
 end
  
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 for ii=1:min(len,2)
  if ((typestr(ii)=='r')|(typestr(ii)=='i')|(typestr(ii)=='c'))
   funlist=[funlist,'       real mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
  else
   funlist=[funlist,'       real mat',num2str(ii),'(1,1)',r];
  end
 end
 if length(typestr)==1
  funlist=[funlist,'       integer :: dim=0',r]; 
  funlist=[funlist,'       integer temp1(1)',r]; 
 elseif length(typestr)==2
  funlist=[funlist,'       integer :: dim=0',r]; 
  funlist=[funlist,'       integer i,j',r]; 
 elseif length(typestr)==3
  funlist=[funlist,'       integer temp1(1)',r]; 
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
      funlist=[funlist,['        mat1=abs(in_1)',r]];
     case 'i'
      funlist=[funlist,['        mat1=in_1',r]];
     case 's'
      funlist=[funlist,['        mat1(1,1)=in_1',r]];
     case 't'
      funlist=[funlist,['        mat1(1,1)=abs(in_1)',r]];
     case 'u'
      funlist=[funlist,['        mat1(1,1)=in_1',r]];
    end
   case 2
    if length(typestr)==2
     unique_r=['        mat2=in_2',r];
     unique_c=['        mat2=abs(in_2)',r];
     unique_i=['        mat2=in_2',r];
     unique_s=['        mat2(1,1)=in_2',r];
     unique_t=['        mat2(1,1)=abs(in_2)',r];
     unique_u=['        mat2(1,1)=in_2',r];
    elseif length(typestr)==3
     unique_r=[];
     unique_c=[];
     unique_i=[];
     unique_s=[];
     unique_t=[];
     unique_u=[];
    end     
    switch typestr(ii)
     case 'r'
      funlist=[funlist,unique_r];
     case 'c'
      funlist=[funlist,unique_c];
     case 'i'
      funlist=[funlist,unique_i];
     case 's'
      funlist=[funlist,unique_s];
     case 't'
      funlist=[funlist,unique_t];
     case 'u'
      funlist=[funlist,unique_u];
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
 if length(typestr)==1
  kernel=[kernel,'        if ((in_1_m==1).and.(in_1_n==1)) then',r];
  kernel=[kernel,'         allocate(out(1,1))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'         out(1,1)=in_1(1,1)',r];
  else
   kernel=[kernel,'         out(1,1)=in_1',r];
  end
  kernel=[kernel,'        elseif (in_1_m==1) then',r];
  kernel=[kernel,'         allocate(out(1,1))',r];
  kernel=[kernel,'         temp1=maxloc(mat1(1,:))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'         out(1,(/1/))=in_1(1,temp1)',r];
  else
   kernel=[kernel,'         out(1,(/1/))=in_1',r];
  end
  kernel=[kernel,'        elseif (in_1_n==1) then',r];
  kernel=[kernel,'         allocate(out(1,1))',r];
  kernel=[kernel,'         temp1=maxloc(mat1(:,1))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'         out((/1/),1)=in_1(temp1,1)',r];
  else
   kernel=[kernel,'         out((/1/),1)=in_1',r];
  end
  kernel=[kernel,'        else',r];
  kernel=[kernel,'         allocate(out(1,in_1_n))',r];
  kernel=[kernel,'         do i=1,in_1_n',r];
  kernel=[kernel,'          temp1=maxloc(mat1(:,i))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'          out((/1/),i)=in_1(temp1,i)',r];
  else
   kernel=[kernel,'          out((/1/),i)=in_1',r];
  end
  kernel=[kernel,'         enddo',r];
  kernel=[kernel,'        endif',r];
 elseif length(typestr)==2
  kernel=[kernel,'        allocate(out(in_1_m,in_1_n))',r];
  kernel=[kernel,'        if ((in_1_m==1).and.(in_1_n==1)) then',r];
  kernel=[kernel,'         if ((in_2_m==1).and.(in_2_n==1)) then',r];
  kernel=[kernel,'          if (mat1(1,1)>=mat2(1,1)) then',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'           out(1,1)=in_1(1,1)',r];
  else
   kernel=[kernel,'           out(1,1)=in_1',r];
  end
  kernel=[kernel,'          else',r];
  if ((typestr(2)=='r')|(typestr(2)=='c')|(typestr(2)=='i'))
   kernel=[kernel,'           out(1,1)=in_2(1,1)',r];
  else
   kernel=[kernel,'           out(1,1)=in_2',r];
  end
  kernel=[kernel,'          endif',r];
  kernel=[kernel,'         else',r];
  kernel=[kernel,'          do i=1,in_2_m',r];
  kernel=[kernel,'           do j=1,in_2_n',r];
  kernel=[kernel,'            if (mat1(1,1)>=mat2(i,j)) then',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'             out(i,j)=in_1(1,1)',r];
  else
   kernel=[kernel,'             out(i,j)=in_1',r];
  end
  kernel=[kernel,'            else',r];
  if ((typestr(2)=='r')|(typestr(2)=='c')|(typestr(2)=='i'))
   kernel=[kernel,'             out(i,j)=in_2(i,j)',r];
  else
   kernel=[kernel,'             out(i,j)=in_2',r];
  end
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           enddo',r];
  kernel=[kernel,'          enddo',r];  
  kernel=[kernel,'         endif',r];
  kernel=[kernel,'        else',r];
  kernel=[kernel,'         if ((in_2_m==1).and.(in_2_n==1)) then',r];
  kernel=[kernel,'          do i=1,in_1_m',r];
  kernel=[kernel,'           do j=1,in_1_n',r];
  kernel=[kernel,'            if (mat1(i,j)>=mat2(1,1)) then',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'             out(i,j)=in_1(i,j)',r];
  else
   kernel=[kernel,'             out(i,j)=in_1',r];
  end
  kernel=[kernel,'            else',r];
  if ((typestr(2)=='r')|(typestr(2)=='c')|(typestr(2)=='i'))
   kernel=[kernel,'             out(i,j)=in_2(1,1)',r];
  else
   kernel=[kernel,'             out(i,j)=in_2',r];
  end
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           enddo',r];
  kernel=[kernel,'          enddo',r];
  kernel=[kernel,'         else',r];
  kernel=[kernel,'          do i=1,in_1_m',r];
  kernel=[kernel,'           do j=1,in_1_n',r];
  kernel=[kernel,'            if (mat1(i,j)>=mat2(i,j)) then',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'             out(i,j)=in_1(i,j)',r];
  else
   kernel=[kernel,'             out(i,j)=in_1',r];
  end
  kernel=[kernel,'            else',r];
  if ((typestr(2)=='r')|(typestr(2)=='c')|(typestr(2)=='i'))
   kernel=[kernel,'             out(i,j)=in_2(i,j)',r];
  else
   kernel=[kernel,'             out(i,j)=in_2',r];
  end
  kernel=[kernel,'            endif',r];
  kernel=[kernel,'           enddo',r];
  kernel=[kernel,'          enddo',r];
  kernel=[kernel,'         endif',r];
  kernel=[kernel,'        endif',r];
 elseif length(typestr)==3
  kernel=[kernel,'        if (dim==1) then',r];
  kernel=[kernel,'         allocate(out(1,in_1_n))',r];
  kernel=[kernel,'         do i=1,in_1_n',r];
  kernel=[kernel,'          temp1=maxloc(mat1(:,i))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'          out((/1/),i)=in_1(temp1,i)',r];
  else
   kernel=[kernel,'          out((/1/),i)=in_1',r];
  end
  kernel=[kernel,'         enddo',r];
  kernel=[kernel,'        elseif (dim==2) then',r];
  kernel=[kernel,'         allocate(out(in_1_m,1))',r];
  kernel=[kernel,'         do i=1,in_1_m',r];
  kernel=[kernel,'          temp1=maxloc(mat1(i,:))',r];
  if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
   kernel=[kernel,'          out(i,(/1/))=in_1(i,temp1)',r];
  else
   kernel=[kernel,'          out(i,(/1/))=in_1',r];
  end
  kernel=[kernel,'         enddo',r];
  kernel=[kernel,'        endif',r];
 end
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
