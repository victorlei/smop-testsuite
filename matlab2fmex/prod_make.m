function [modlist,funlist,fun_info]=prod_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='prod';
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
 [modlist,funlist,len]=makeheader('prod',1,typestr,modlist,funlist,r); 
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
 if length(typestr)<2
  funlist=[funlist,'       integer :: dim=0',r]; 
 else
  funlist=[funlist,'       integer dim',r]; 
 end
 if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
  if ~((typestr(1)=='c')|(typestr(1)=='t'))
   funlist=[funlist,'       real, dimension(size(in_1,dim=1),size(in_1,dim=2)) :: mat',r];
  else
   funlist=[funlist,'       complex, dimension(size(in_1,dim=1),size(in_1,dim=2)) :: mat',r];
  end
 else
  if ~((typestr(1)=='c')|(typestr(1)=='t'))
   funlist=[funlist,'       real, dimension(1,1) :: mat',r];
  else
   funlist=[funlist,'       complex, dimension(1,1) :: mat',r];
  end
 end
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
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
   case 2
    if length(typestr)>1
    end    
    switch typestr(ii)
     case 'r'
      funlist=[funlist,['        dim=int(in_2(1,1))',r]];
     case 'c'
      funlist=[funlist,['        dim=int(in_2(1,1))',r]];
     case 'i'
      funlist=[funlist,['        dim=(in_2(1,1))',r]];
     case 's'
      funlist=[funlist,['        dim=int(in_2)',r]];
     case 't'
      funlist=[funlist,['        dim=int(in_2)',r]];
     case 'u'
      funlist=[funlist,['        dim=(in_2)',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 if length(typestr)<3
  kernel=[''];
 else
  kernel=[''];
 end
 kernel=[kernel,'        if (in_1_m==1) then',r];
 kernel=[kernel,'         if (in_1_n==1) then',r];
 kernel=[kernel,'          allocate(out(1,1))',r];
 kernel=[kernel,'          out=mat',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          if ((dim==0).or.(dim==2)) then',r];
 kernel=[kernel,'           allocate(out(1,1))',r];
 kernel=[kernel,'           out=product(mat)',r];
 kernel=[kernel,'          elseif (dim==1) then',r];
 kernel=[kernel,'           allocate(out(in_1_m,in_1_n))',r];
 kernel=[kernel,'           out=mat',r];
 kernel=[kernel,'          endif',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        elseif (in_1_n==1) then',r];
 kernel=[kernel,'         if ((dim==0).or.(dim==1)) then',r];
 kernel=[kernel,'          allocate(out(1,1))',r];
 kernel=[kernel,'          out=product(mat)',r];
 kernel=[kernel,'         elseif (dim==2) then',r];
 kernel=[kernel,'          allocate(out(in_1_m,1))',r];
 kernel=[kernel,'          out=mat',r];
 kernel=[kernel,'         endif',r];
 kernel=[kernel,'        else',r];
 kernel=[kernel,'         if ((dim==0).or.(dim==1)) then',r];
 kernel=[kernel,'          allocate(out(1,in_1_m))',r];
 kernel=[kernel,'          out(1,1:in_1_n)=product(mat,1)',r];
 kernel=[kernel,'         else',r];
 kernel=[kernel,'          allocate(out(in_1_m,1))',r];
 kernel=[kernel,'          out(1:in_1_m,1)=product(mat,2)',r];
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
