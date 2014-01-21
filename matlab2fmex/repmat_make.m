function [modlist,funlist,fun_info]=repmat_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='repmat';
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
 [modlist,funlist,len]=makeheader('repmat',2,typestr,modlist,funlist,r);
 arr={'r';'c';'i'}; noi={'r';'c';'s';'t'}; com={'c';'t'};
 temp1='complex';fun_info{2}='c';
 if ~any(strcmp(typestr(1),typs{3}))
  temp1='real'; fun_info{2}='r';
 end
 temp2{1,1}=''; temp2{1,2}=''; temp2{2,1}=''; temp2{2,2}='';
 temp3{1,1}=''; temp3{1,2}=''; temp3{2,1}=''; temp3{2,2}='';
 temp4{1,1}=''; temp4{1,2}='';
 if any(strcmp(typestr(1),typs{11})),temp4{1,1}='size(in_1,1)*';temp4{1,2}='size(in_1,2)*';end
 if any(strcmp(typestr(1),typs{4})), temp4{1,1}='';             temp4{1,2}='size(in_1)*';  end
 if length(typestr)==2
  if ~any(strcmp(typestr(2),typs{7})),temp3{1,1}='NInt';         temp3{2,1}='NInt';         end
  if any(strcmp(typestr(2),typs{3})), temp3{1,2}='real';         temp3{2,2}='real';         end
  if ~any(strcmp(typestr(2),typs{4})),temp2{1,2}='(1,1)';        temp2{2,2}='(1,2)';
  else                                temp2{1,2}='(1)';          temp2{2,2}='(2)';          end
  temp2{1,1}='2';        temp2{2,1}='2';
 elseif length(typestr)==3
  temp2{1,1}='2';        temp2{2,1}='3';
  if any(strcmp(typestr(2),typs{11})),                        temp2{1,2}='(1,1)';end
  if any(strcmp(typestr(2),typs{4})),                         temp2{1,2}='(1)';  end
  if any(strcmp(typestr(3),typs{11})),                        temp2{2,2}='(1,1)';end
  if any(strcmp(typestr(2),typs{4})),                         temp2{2,2}='(1)';  end
  if ~any(strcmp(typestr(2),typs{7})),     temp3{1,1}='NInt';                    end
  if any(strcmp(typestr(2),typs{3})),      temp3{1,2}='real';                    end
  if ~any(strcmp(typestr(3),typs{7})),                        temp3{2,1}='NInt'; end
  if any(strcmp(typestr(3),typs{3})),                         temp3{2,2}='real'; end
 end
 funlist=[funlist,'       ',temp1,', dimension(',temp4{1,1},temp3{1,1},'(',temp3{1,2},'(in_',temp2{1,1},temp2{1,2},')),',temp4{1,2},temp3{2,1},'(',temp3{2,2},'(in_',temp2{2,1},temp2{2,2},'))) :: out',r];
 %'ww',kb
 
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 %Here we can insert any global vars or preliminary options %%%%%%
 funlist=[funlist,'       integer tempm, tempn, i, j, m, n',r];
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%

 funlist=[funlist,['        tempm=',temp3{1,2},'(',temp3{1,1},'(in_',temp2{1,1},temp2{1,2},'))',r]];
 funlist=[funlist,['        tempn=',temp3{2,2},'(',temp3{2,1},'(in_',temp2{2,1},temp2{2,2},'))',r]];
 
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=[''];
 kernel=[kernel,'        do m=1,tempm',r];
 kernel=[kernel,'         do n=1,tempn',r];
 if length(find(strcmp(typestr(1),{'r' 'c' 'i'})))>0
  kernel=[kernel,'          do i=1,in_1_m',r];
  kernel=[kernel,'           do j=1,in_1_n',r];
  kernel=[kernel,'            out(i+in_1_m*(m-1),j+in_1_n*(n-1))=in_1(i,j)',r];
  kernel=[kernel,'           end do',r];
  kernel=[kernel,'          end do',r];
 else
  kernel=[kernel,'            out(1+in_1_m*(m-1),1+in_1_n*(n-1))=in_1',r];
 end
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
