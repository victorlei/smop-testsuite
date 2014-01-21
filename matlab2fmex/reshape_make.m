function [modlist,funlist,fun_info]=reshape_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='reshape';
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
 [modlist,funlist,len]=makeheader('reshape',2,typestr,modlist,funlist,r); 
 arr={'r';'c';'i';'l'}; noi={'r';'c';'s';'t'}; com={'c';'t'};vecs={'w';'x';'y';'z'};
 temp1='complex';fun_info{2}='c';
 if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
  temp1='real'; fun_info{2}='r';
 end
 temp2{1,1}=''; temp2{1,2}=''; temp2{2,1}=''; temp2{2,2}='';
 temp3{1,1}=''; temp3{1,2}=''; temp3{2,1}=''; temp3{2,2}='';
 if length(typestr)==2
  if ((typestr(2)=='r')|(typestr(2)=='c')),temp3{1,1}='NInt'; temp3{2,1}='NInt'; end
  if typestr(2)=='c',                      temp3{1,2}='real'; temp3{2,1}='real'; end
  temp2{1,1}='2';        temp2{2,1}='2';
  if any(strcmp(typestr(2),arr))  ;        temp2{1,2}='(1,1)';temp2{2,2}='(1,2)';end
  if any(strcmp(typestr(2),vecs)) ;        temp2{1,2}='(1)';  temp2{2,2}='(2)';  end
 elseif length(typestr)==3
  temp2{1,1}='2';        temp2{2,1}='3';
  if any(strcmp(typestr(2),arr)),                             temp2{1,2}='(1,1)';end
  if any(strcmp(typestr(3),arr)),                             temp2{2,2}='(1,1)';end
  if any(strcmp(typestr(2),noi)),          temp3{1,1}='NInt';                    end
  if any(strcmp(typestr(2),com)),          temp3{1,2}='real';                    end
  if any(strcmp(typestr(3),noi)),                             temp3{2,1}='NInt'; end
  if any(strcmp(typestr(3),com)),                             temp3{2,2}='real'; end
 end
 funlist=[funlist,'       ',temp1,', dimension(',temp3{1,1},'(',temp3{1,2},'(in_',temp2{1,1},temp2{1,2},')),',temp3{2,1},'(',temp3{2,2},'(in_',temp2{2,1},temp2{2,2},'))) :: out',r];
 %'wwreshape',kb
 
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);

 %Here we can insert any global vars or preliminary options %%%%%%
 funlist=[funlist,'       integer tempm, tempn, i, j, mm, nn',r]; 
 funlist=makesize2(wantsizes,funlist,len,r,typestr);

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
    if length(typestr)>2
     unique_r=['        tempm=int(in_2(1,1))',r];
     unique_c=[];
     unique_i=['        tempm=in_2(1,1)',r];
     unique_s=['        tempm=int(in_2)',r];
     unique_t=[];
     unique_u=['        tempm=in_2',r];
    else
     unique_r=['        tempm=int(in_2(1,1));tempn=int(in_2(1,2));',r];
     unique_c=[];
     unique_i=['        tempm=in_2(1,1);tempn=in_2(1,2);',r];
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
      funlist=[funlist,['        tempn=int(in_3(1,1))',r]];
     case 'c'
      funlist=[funlist,[]];
     case 'i'
      funlist=[funlist,['        tempn=in_3(1,1)',r]];
     case 's'
      funlist=[funlist,['        tempn=int(in_3)',r]];
     case 't'
      funlist=[funlist,[]];
     case 'u'
      funlist=[funlist,['        tempn=in_3',r]];
    end
  end
 end
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 if length(typestr)<3
  kernel=[''];
 else
  kernel=[''];
 end
 kernel=[kernel,'        m=1; n=1;',r];
 kernel=[kernel,'        do j=1,tempn',r];
 kernel=[kernel,'         do i=1,tempm',r];
 if any(strcmp(typestr(1),arr))
  kernel=[kernel,'          out(i,j)=in_1(m,n)',r];
 elseif any(strcmp(typestr(1),vecs))
  kernel=[kernel,'          out(i,j)=in_1(m)',r];
 else
  kernel=[kernel,'          out(i,j)=in_1',r];
 end
 kernel=[kernel,'          m=m+1',r];
 kernel=[kernel,'          if (m>in_1_m) then',r];
 kernel=[kernel,'           m=1; n=n+1;',r];
 kernel=[kernel,'          end if',r];
 kernel=[kernel,'         end do',r];
 kernel=[kernel,'        end do',r];
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
