function [modlist,funlist,fun_info]=gamma_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='gamma';
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
 [modlist,funlist,len]=makeheader('gamma',2,typestr,modlist,funlist,r); 
 if ((typestr(len)=='r')|(typestr(len)=='i')|(typestr(len)=='c'))
  funlist=[funlist,'       real, dimension(size(in_',num2str(len),',dim=1),size(in_',num2str(len),',dim=2)) :: out',r];
 else
  funlist=[funlist,'       real, dimension(1,1) :: out',r];   
 end
 fun_info{2}='r';
  
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
   funlist=[funlist,'       real mat1(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
  else
   funlist=[funlist,'       real mat1(1,1)',r];
  end
 end
 funlist=[funlist,'       integer i,j',r]; 
 unique_r=['        mat1=in_1',r];
 unique_c=['        mat1=in_1',r];
 unique_i=['        mat1=in_1',r];
 unique_s=['        mat1(1,1)=in_1',r];
 unique_t=['        mat1(1,1)=in_1',r];
 unique_u=['        mat1(1,1)=in_1',r];
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
 kernel=[kernel,'           if (in_1_m==1) then',r];
 kernel=[kernel,'            if (in_1_n==1) then ! scalar',r];
 kernel=[kernel,'             out(1,1)=dgamma(mat1(1,1))',r];
 kernel=[kernel,'            else                ! row',r];
 kernel=[kernel,'             do i=1,in_1_n',r];
 kernel=[kernel,'              out(1,i)=dgamma(mat1(1,i))',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            endif',r];
 kernel=[kernel,'           else',r];
 kernel=[kernel,'            if (in_1_n==1) then ! column',r];
 kernel=[kernel,'             do i=1,in_1_m',r];
 kernel=[kernel,'              out(i,1)=dgamma(mat1(i,1))',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            else                ! matrix',r];
 kernel=[kernel,'             do i=1,in_1_m',r];
 kernel=[kernel,'              do j=1,in_1_n',r];
 kernel=[kernel,'               out(i,j)=dgamma(mat1(i,j))',r];
 kernel=[kernel,'              enddo',r];
 kernel=[kernel,'             enddo',r];
 kernel=[kernel,'            endif',r];
 kernel=[kernel,'           endif',r];
 
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
