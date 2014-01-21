function [modlist,funlist,fun_info]=median_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='median';
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
 [modlist,funlist,len]=makeheader(funname,1,typestr,modlist,funlist,r); 
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
 ii=1;
 if ((typestr(ii)=='r')|(typestr(ii)=='i')|(typestr(ii)=='c'))
  funlist=[funlist,'       real mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
 else
  funlist=[funlist,'       real mat',num2str(ii),'(1,1)',r];
 end
 if any(strcmp(typestr(1),{'r';'c';'i'}))
  funlist=[funlist,'       integer :: i',r]; 
 end
 if length(typestr)==1
 elseif length(typestr)==2
  funlist=[funlist,'       integer :: dim',r]; 
  switch typestr(2)
   case 'r'
    funlist=[funlist,'       dim=int(in_2(1,1))',r];
   case 'c'
    funlist=[funlist,'       dim=int(in_2(1,1))',r];
   case 'i'
    funlist=[funlist,'       dim=in_2(1,1)',r];
   case 's'
    funlist=[funlist,'       dim=int(in_2)',r];
   case 't'
    funlist=[funlist,'       dim=int(in_2)',r];
   case 'u'
    funlist=[funlist,'       dim=in_2',r];
  end  
 end
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,['        mat1=in_1',r]];
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 x{1}=''; x{2}='';
 if any(strcmp(typestr(1),{'c';'t'})),x{1}='abs(' ;x{2}=')';end
 if length(typestr)==1
  switch typestr(1)
   case {'r';'c';'i'}
    kernel=[kernel,'        if ((in_1_m==1).and.(in_1_n==1)) then',r];
    kernel=[kernel,'         allocate(out(1,1))',r];
    if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
     kernel=[kernel,'         out(1,1)=in_1(1,1)',r];
    else
     kernel=[kernel,'         out(1,1)=in_1',r];
    end
    kernel=[kernel,'        elseif (in_1_m==1) then',r];
    kernel=[kernel,'         allocate(out(1,1))',r];
    kernel=[kernel,'         out(1,1:1)=(in_1(1,maxloc(',x{1},'in_1(1,:)'x{2},'))+',...
            'in_1(1,minloc(',x{1},'in_1(1,:)'x{2},')))/2.0',r];
    kernel=[kernel,'        elseif (in_1_n==1) then',r];
    kernel=[kernel,'         allocate(out(1,1))',r];
    kernel=[kernel,'         out(1:1,1)=(in_1(maxloc(',x{1},'in_1(:,1)'x{2},'),1)+',...
            'in_1(minloc(',x{1},'in_1(:,1)'x{2},'),1))/2.0',r];
    kernel=[kernel,'        else',r];
    kernel=[kernel,'         allocate(out(1,in_1_n))',r];
    kernel=[kernel,'         do i=1,in_1_n',r];
    kernel=[kernel,'          out(1,i:i)=(in_1(maxloc(',x{1},'in_1(:,i)'x{2},'),i)+',...
            'in_1(minloc(',x{1},'in_1(:,i)'x{2},'),i))/2.0',r];
    kernel=[kernel,'         enddo',r];
    kernel=[kernel,'        endif',r];
   case {'s';'t';'u'}
    kernel=[kernel,'         allocate(out(1,1))',r];
    kernel=[kernel,'         out(1,1)=in_1',r];
  end  
 elseif length(typestr)==2
  switch typestr(1)
   case {'r';'c';'i'}
    kernel=[kernel,'        if ((in_1_m==1).and.(in_1_n==1)) then',r];
    kernel=[kernel,'         allocate(out(1,1))',r];
    if ((typestr(1)=='r')|(typestr(1)=='c')|(typestr(1)=='i'))
     kernel=[kernel,'         out(1,1)=in_1(1,1)',r];
    else
     kernel=[kernel,'         out(1,1)=in_1',r];
    end
    kernel=[kernel,'        elseif (in_1_m==1) then',r];
    kernel=[kernel,'         if (dim==1) then',r];
    kernel=[kernel,'          allocate(out(1,in_1_n))',r];
    kernel=[kernel,'          out=in_1',r];
    kernel=[kernel,'         elseif (dim==2) then',r];
    kernel=[kernel,'          allocate(out(1,1))',r];
    kernel=[kernel,'          out(1,1:1)=(in_1(1,maxloc(',x{1},'in_1(1,:)'x{2},'))+',...
            'in_1(1,minloc(',x{1},'in_1(1,:)'x{2},')))/2.0',r];
    kernel=[kernel,'         endif',r];
    kernel=[kernel,'        elseif (in_1_n==1) then',r];
    kernel=[kernel,'         if (dim==2) then',r];
    kernel=[kernel,'          allocate(out(in_1_m,1))',r];
    kernel=[kernel,'          out=in_1',r];
    kernel=[kernel,'         elseif (dim==1) then',r];
    kernel=[kernel,'          allocate(out(1,1))',r];
    kernel=[kernel,'          out(1:1,1)=(in_1(maxloc(',x{1},'in_1(:,1)'x{2},'),1)+',...
            'in_1(minloc(',x{1},'in_1(:,1)'x{2},'),1))/2.0',r];
    kernel=[kernel,'         endif',r];
    kernel=[kernel,'        else',r];
    kernel=[kernel,'         if (dim==1) then',r];
    kernel=[kernel,'          allocate(out(1,in_1_n))',r];
    kernel=[kernel,'          do i=1,in_1_n',r];
    kernel=[kernel,'           out(1,i:i)=(in_1(maxloc(',x{1},'in_1(:,i)'x{2},'),i)+',...
            'in_1(minloc(',x{1},'in_1(:,i)'x{2},'),i))/2.0',r];
    kernel=[kernel,'          enddo',r];
    kernel=[kernel,'         elseif (dim==2) then',r];
    kernel=[kernel,'          allocate(out(in_1_m,1))',r];
    kernel=[kernel,'          do i=1,in_1_m',r];
    kernel=[kernel,'           out(i:i,1)=(in_1(i,maxloc(',x{1},'in_1(i,:)'x{2},'))+',...
            'in_1(i,minloc(',x{1},'in_1(i,:)'x{2},')))/2.0',r];
    kernel=[kernel,'          enddo',r];
    kernel=[kernel,'         endif',r];
    kernel=[kernel,'        endif',r];
   case {'s';'t';'u'}
    kernel=[kernel,'         allocate(out(1,1))',r];
    kernel=[kernel,'         out(1,1)=in_1',r];
  end    
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
