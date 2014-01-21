function [modlist,funlist,fun_info]=find_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='find';
modlist='';funlist='';
r=[char(10)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist,...
	 '       interface ',funname,'f',r];
if ~iscell(typestrlist)
 typestrlist={typestrlist};
end
fun_info{2}='i';
for i=1:length(typestrlist)
 typestr=typestrlist{i};
  for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
 [modlist,funlist,len]=makeheader(funname,1,typestr,modlist,funlist,r); 
 arr={'r';'c';'i';'l'}; noi={'r';'c';'s';'t';'v'}; com={'c';'t'}; log={'l';'v'};
 funlist=[funlist,'       integer, pointer, dimension(:,:) :: out',r];
 if any(strcmp(typestr(1),arr))
  funlist=[funlist,'       integer, dimension(size(in_1)) :: temp',r];
 else
  funlist=[funlist,'       integer :: temp',r];
 end
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 funlist=[funlist,'       integer i, j, num',r]; 
 funlist=makesize2(wantsizes,funlist,len,r,typestr);

 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=['        num=0',r];
 if any(strcmp(typestr(1),log)),  temp4=''; else,  temp4='/=0'; end
 switch typestr(1)
  case {'r';'c';'i';'l'}
   kernel=[kernel,'        if ((in_1_m==1).and.(in_1_n==1)) then',r];
   kernel=[kernel,'         if (in_1(1,1)',temp4,') then ',r];
   kernel=[kernel,'          num=1',r];
   kernel=[kernel,'          allocate(out(num,1))',r];
   kernel=[kernel,'          out(1,1)=1',r];
   kernel=[kernel,'         else',r];
   kernel=[kernel,'          allocate(out(num,1))',r];
   kernel=[kernel,'         endif',r];
   kernel=[kernel,'        elseif (in_1_m==1) then',r];
   kernel=[kernel,'         do i=1,in_1_n',r];
   kernel=[kernel,'          if (in_1(1,i)',temp4,') then',r];
   kernel=[kernel,'           num=num+1',r];
   kernel=[kernel,'           temp(num)=i',r];
   kernel=[kernel,'          endif',r];
   kernel=[kernel,'         enddo',r];
   kernel=[kernel,'         allocate(out(1,num))',r];
   kernel=[kernel,'         out(1,1:num)=temp',r];
   kernel=[kernel,'        elseif (in_1_n==1) then',r];
   kernel=[kernel,'         do i=1,in_1_m',r];
   kernel=[kernel,'          if (in_1(i,1)',temp4,') then',r];
   kernel=[kernel,'           num=num+1',r];
   kernel=[kernel,'           temp(num)=i',r];
   kernel=[kernel,'          endif',r];
   kernel=[kernel,'         enddo',r];
   kernel=[kernel,'         allocate(out(num,1))',r];
   kernel=[kernel,'         out(1:num,1)=temp',r];
   kernel=[kernel,'        else',r];
   kernel=[kernel,'         do j=1,in_1_n',r];
   kernel=[kernel,'          do i=1,in_1_m',r];
   kernel=[kernel,'           if (in_1(i,j)',temp4,') then',r];
   kernel=[kernel,'            num=num+1',r];
   kernel=[kernel,'            temp(num)=i+(j-1)*in_1_m',r];
   kernel=[kernel,'           endif',r];
   kernel=[kernel,'          enddo',r];
   kernel=[kernel,'         enddo',r];
   kernel=[kernel,'         allocate(out(num,1))',r];
   kernel=[kernel,'         out(1:num,1)=temp',r];
   kernel=[kernel,'        endif',r];
  case {'s';'t';'u';'v'}
   kernel=[kernel,'        if (in_1',temp4,') then ',r];
   kernel=[kernel,'         num=1',r];
   kernel=[kernel,'         allocate(out(num,1))',r];
   kernel=[kernel,'         out(1,1)=1',r];
   kernel=[kernel,'        else',r];
   kernel=[kernel,'         allocate(out(num,1))',r];
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
