function [modlist,funlist,fun_info]=size_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='size';
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
 [modlist,funlist,len]=makeheader('size',2,typestr,modlist,funlist,r); 
 if length(typestr)==1
  funlist=[funlist,'       integer, dimension(1,2) :: out',r];
  fun_info{2}='i';
 else
  funlist=[funlist,'       integer :: out',r];
  fun_info{2}='u';
 end
 
 wantsizes=1;
%%% funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 if length(typestr)<2
 else
  funlist=[funlist,'       integer dim',r]; 
 end
%%% funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 for ii=1:length(typestr)
  switch ii
   case 1
    switch typestr(ii)
     case 'r'
      funlist=[funlist,[]];
     case 'c'
      funlist=[funlist,[]];
     case 'i'
      funlist=[funlist,[]];
     case 's'
      funlist=[funlist,[]];
     case 't'
      funlist=[funlist,[]];
     case 'u'
      funlist=[funlist,[]];
    end
   case 2
    if length(typestr)>1
    end    
    switch typestr(ii)
     case typs{14}
      funlist=[funlist,['        dim=int(in_2(1,1))',r]];
     case typs{13}
      funlist=[funlist,['        dim=int(in_2(1,1))',r]];
     case typs{15}
      funlist=[funlist,['        dim=(in_2(1,1))',r]];
     case {'w','x'}
      funlist=[funlist,['        dim=int(in_2(1))',r]];
     case 'y'
      funlist=[funlist,['        dim=(in_2(1))',r]];
     case {'s','t'}
      funlist=[funlist,['        dim=int(in_2)',r]];
     case 'u'
      funlist=[funlist,['        dim=(in_2)',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 if length(typestr)==1
  switch typestr(1)
   case typs{1}
    kernel=[kernel,'        out(1,1)=size(in_1,1)',r];
    kernel=[kernel,'        out(1,2)=size(in_1,2)',r];
   case typs{9}
    kernel=[kernel,'        out(1,1)=1',r];
    kernel=[kernel,'        out(1,2)=size(in_1)',r];
   case typs{10}
    kernel=[kernel,'        out(1,1)=size(in_1)',r];
    kernel=[kernel,'        out(1,2)=1',r];
   case typs{4}
    kernel=[kernel,'        out(1,1)=1',r];
    kernel=[kernel,'        out(1,2)=size(in_1)',r];
   otherwise
    kernel=[kernel,'        out(1,1)=1',r];
    kernel=[kernel,'        out(1,2)=1',r];
  end
 else
  switch typestr(1)
   case typs{1}
    kernel=[kernel,'        out=size(in_1,dim)',r];
   case typs{9}
    kernel=[kernel,'        if (dim==1) then',r];
    kernel=[kernel,'         out=1',r];
    kernel=[kernel,'        else',r];
    kernel=[kernel,'         out=size(in_1)',r];
    kernel=[kernel,'        endif',r];
   case typs{10}
    kernel=[kernel,'        if (dim==1) then',r];
    kernel=[kernel,'         out=size(in_1)',r];
    kernel=[kernel,'        else',r];
    kernel=[kernel,'         out=1',r];
    kernel=[kernel,'        endif',r];
   case typs{4}
    kernel=[kernel,'        out=size(in_1)',r];
   otherwise
    kernel=[kernel,'        out=1',r];
  end
 end
 funlist=[funlist,kernel];
 
 %End of the function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 funlist=[funlist,'       end function ',funname,'f_',typestr2,r];
 if strcmp(typestr,typestrlist{length(typestrlist)})
 else
  funlist=[funlist,r];
 end
end
%And end the module %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
modlist=[modlist...
	 '       end interface ',funname,'f',char(10)];
fun_info{1}=0;
