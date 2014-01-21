function [modlist,funlist,fun_info]=dot2_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='dot2';
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
 [modlist,funlist,len]=makeheader(funname,2,typestr,modlist,funlist,r); 
 if any(strcmp(typestr(1),typs{3}))|any(strcmp(typestr(2),typs{3}))
  funlist=[funlist,'       complex, dimension(size(in_1,1),1) :: out',r];
  fun_info{2}='c';
 else
  funlist=[funlist,'       real, dimension(size(in_1,1),1) :: out',r];
  fun_info{2}='r';
 end
 
 wantsizes=1;
 if wantsizes
  funlist=[funlist,'       integer out_m, out_n, '];
  for ii=1:len
   if ii~=len
    funlist=[funlist,'in_',num2str(ii),'_m, in_',num2str(ii),'_n, '];
   else
    funlist=[funlist,'in_',num2str(ii),'_m, in_',num2str(ii),'_n',r];
   end
  end
 end

 %Here we can insert any global vars or preliminary options %%%%%%
 if length(typestr)<3
  funlist=[funlist,'       integer i,j',r]; 
  funlist=[funlist,'       integer :: dim=0',r]; 
 else
  funlist=[funlist,'       integer i,j,dim',r]; 
 end
%%% for ii=1:2
%%%  if typestr(ii)=='c'
%%%   funlist=[funlist,'       complex mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
%%%  elseif typestr(ii)=='t'
%%%   funlist=[funlist,'       complex mat',num2str(ii),'(1,1)',r];
%%%  elseif ((typestr(ii)=='r')|(typestr(ii)=='i'))
%%%   funlist=[funlist,'       real mat',num2str(ii),'(size(in_',num2str(ii),',1),size(in_',num2str(ii),',2))',r];
%%%  elseif ((typestr(ii)=='s')|(typestr(ii)=='u'))
%%%   funlist=[funlist,'       real mat',num2str(ii),'(1,1)',r];
%%%  end
%%% end
%%% funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
%%% for ii=1:length(typestr)
%%%  switch ii
%%%   case 1
%%%    switch typestr(ii)
%%%     case 'r'
%%%      funlist=[funlist,['        mat1=in_1',r]];
%%%     case 'c'
%%%      funlist=[funlist,['        mat1=in_1',r]];
%%%     case 'i'
%%%      funlist=[funlist,['        mat1=in_1',r]];
%%%     case 's'
%%%      funlist=[funlist,['        mat1(1,1)=in_1',r]];
%%%     case 't'
%%%      funlist=[funlist,['        mat1(1,1)=in_1',r]];
%%%     case 'u'
%%%      funlist=[funlist,['        mat1(1,1)=in_1',r]];
%%%    end
%%%   case 2
%%%    switch typestr(ii)
%%%     case 'r'
%%%      funlist=[funlist,['        mat2=in_2',r]];
%%%     case 'c'
%%%      funlist=[funlist,['        mat2=in_2',r]];
%%%     case 'i'
%%%      funlist=[funlist,['        mat2=in_2',r]];
%%%     case 's'
%%%      funlist=[funlist,['        mat2(1,1)=in_2',r]];
%%%     case 't'
%%%      funlist=[funlist,['        mat2(1,1)=in_2',r]];
%%%     case 'u'
%%%      funlist=[funlist,['        mat2(1,1)=in_2',r]];
%%%    end
%%%   case 3
%%%    if length(typestr)>1
%%%    end    
%%%    switch typestr(ii)
%%%     case 'r'
%%%      funlist=[funlist,['        dim=int(in_3(1,1))',r]];
%%%     case 'c'
%%%      funlist=[funlist,['        dim=int(in_3(1,1))',r]];
%%%     case 'i'
%%%      funlist=[funlist,['        dim=(in_3(1,1))',r]];
%%%     case 's'
%%%      funlist=[funlist,['        dim=int(in_3)',r]];
%%%     case 't'
%%%      funlist=[funlist,['        dim=int(in_3)',r]];
%%%     case 'u'
%%%      funlist=[funlist,['        dim=(in_3)',r]];
%%%    end
%%%  end
%%% end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
%%% kernel=[kernel,'        if (in_1_m==1) then',r];
%%% kernel=[kernel,'         if (in_2_m==1) then',r];
%%% kernel=[kernel,'          out(1,1)=dot_product(mat1(1,:),mat2(1,:))',r];
%%% kernel=[kernel,'         elseif (in_2_n==1) then',r];
%%% kernel=[kernel,'          out(1,1)=dot_product(mat1(1,:),mat2(:,1))',r];
%%% kernel=[kernel,'         endif',r];
%%% kernel=[kernel,'        elseif (in_1_n==1) then',r];
%%% kernel=[kernel,'         if (in_2_m==1) then',r];
%%% kernel=[kernel,'          out(1,1)=dot_product(mat1(:,1),mat2(1,:))',r];
%%% kernel=[kernel,'         elseif (in_2_n==1) then',r];
%%% kernel=[kernel,'          out(1,1)=dot_product(mat1(:,1),mat2(:,1))',r];
%%% kernel=[kernel,'         endif',r];
%%% kernel=[kernel,'        else',r];
%%% kernel=[kernel,'         if ((dim==1).or.(dim==0)) then',r];
%%% kernel=[kernel,'          do i=1,in_1_n',r];
%%% kernel=[kernel,'           out(1,i)=dot_product(mat1(:,i),mat2(:,i))',r];
%%% kernel=[kernel,'          enddo',r];
%%% kernel=[kernel,'         elseif (dim==2) then',r];
 kernel=[kernel,'          do i=1,size(in_1,1)',r];
 kernel=[kernel,'           out(i,1)=dot_product(in_1(i,:),in_2(i,:))',r];
 kernel=[kernel,'          enddo',r];
%%% kernel=[kernel,'         endif',r];
%%% kernel=[kernel,'        endif',r];
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
