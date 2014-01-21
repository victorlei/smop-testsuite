function [modlist,funlist,fun_info]=ss2in_make(typestrlist)
% fun_info is a 1x2 cell. fun_info{1}=>1 result is pointer, 0=>not
%                         fun_info{2}=>typestr ('r' or 'c' usually)
declare_globals
funname='ss2in';
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
 arr={'r';'c';'i';'l'}; noi={'r';'c';'s';'t';'v'}; com={'c';'t'}; log={'l';'v'};
 fun_info{2}=typestr(1);
 %String for output type and result type
 switch typestr(1)
  case 'r'
   fun_info{2}=typestr(1);temp1{1}='real';
  case 'c'
   fun_info{2}=typestr(1);temp1{1}='complex';
  case 'i'
   fun_info{2}=typestr(1);temp1{1}='integer';
  case 'l'
   fun_info{2}='i';       temp1{1}='integer';
  case 's'
   fun_info{2}='r';       temp1{1}='real';
  case 't'
   fun_info{2}='c';       temp1{1}='complex';
  case 'u'
   fun_info{2}='i';       temp1{1}='integer';
  case 'v'
   fun_info{2}='i';       temp1{1}='integer';
 end  

 funlist=[funlist,'       ',temp1{1},', pointer, dimension(:,:) :: out',r];
 if any(strcmp(typestr(1),arr))
  funlist=[funlist,'       ',temp1{1},', dimension(size(in_1)) :: temp',r];
  %funlist=[funlist,'       ',temp1{1},', dimension(size(in_2)) :: temp2',r];
  funlist=[funlist,'       integer, dimension(size(in_2)) :: temp2',r];
 else
  funlist=[funlist,'       ',temp1{1},' :: temp',r];
 end
 wantsizes=1;
 funlist=makesize1(wantsizes,funlist,len,r);
 
 %Here we can insert any global vars or preliminary options %%%%%%
 funlist=[funlist,'       integer i, j, num, bb',r]; 
 funlist=makesize2(wantsizes,funlist,len,r,typestr);
 
 %Here we can insert type dependant things %%%%%%%%%%%%%%%%%%%%%%%
 %And now the kernel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 kernel=['        num=0',r];
 if any(strcmp(typestr(1),log)),  temp4=''; else,  temp4='/=0'; end
 switch typestr(1)
  case {'r';'c';'i';'l'}
   kernel=[kernel,'        if (in_1_m==1) then !row vector',r];
   if strcmp(typestr(2),'l') %Logical indexing
    kernel=[kernel,'         bb=size(in_2);num=count(in_2);allocate(out(1,num))',r];
    kernel=[kernel,'         out(1,1:num)=pack(in_1(1:1,1:bb),reshape(in_2,(/1,bb/)))',r];   
   elseif strcmp(typestr(2),'r')|strcmp(typestr(2),'i') %Either real # or logical indexing w/ 0,1
    if strcmp(typestr(2),'r'), temp3{1}='Int(';temp3{2}=')';else temp3{1}='';temp3{2}=''; end
    temp2{1}='==1';
    kernel=[kernel,'         if (all(in_2==0.or.in_2==1)) then !logical w/ 0,1',r];
    kernel=[kernel,'          bb=size(in_2);num=count(in_2==1);allocate(out(1,num))',r];
    kernel=[kernel,'          out(1,1:num)=pack(in_1(1:1,1:bb),reshape(in_2,(/1,bb/))==1)',r];
    kernel=[kernel,'         else',r]; %Here we index with numbers
    kernel=[kernel,'          if (in_2_m==1) then',r];
    kernel=[kernel,'           bb=size(in_2);allocate(out(1,bb))',r];
    kernel=[kernel,'           out(1,:)=in_1(1,',temp3{1},'in_2(1,:)',temp3{2},')',r];
    kernel=[kernel,'          elseif (in_2_n==1) then',r];
    kernel=[kernel,'           bb=size(in_2);allocate(out(1,bb))',r];
    kernel=[kernel,'           out(1,:)=in_1(1,',temp3{1},'in_2(:,1)',temp3{2},')',r];
    kernel=[kernel,'          else',r];
    kernel=[kernel,'           bb=size(in_2,1);num=size(in_2,2);allocate(out(bb,num))',r];
    kernel=[kernel,'           temp=reshape(in_1,(/size(in_1)/))',r];
    kernel=[kernel,'           temp2=reshape(in_2,(/bb*num/))',r];
    kernel=[kernel,'           out=reshape(temp(temp2),(/bb,num/))',r];
    kernel=[kernel,'          endif',r];
    kernel=[kernel,'         endif',r];
   end
   kernel=[kernel,'        elseif (in_1_n==1) then !column vector',r];
   if strcmp(typestr(2),'l') %Logical indexing
    kernel=[kernel,'         bb=size(in_2);num=count(in_2);allocate(out(num,1))',r];
    kernel=[kernel,'         out(1:num,1)=pack(in_1(1:bb,1:1),reshape(in_2,(/bb,1/)))',r];    
   elseif strcmp(typestr(2),'r')|strcmp(typestr(2),'i') %Either real # or logical indexing w/ 0,1
    if strcmp(typestr(2),'r'), temp3{1}='Int(';temp3{2}=')';else temp3{1}='';temp3{2}=''; end
    kernel=[kernel,'         if (all(in_2==0.or.in_2==1)) then !logical w/ 0,1',r];
    kernel=[kernel,'          bb=size(in_2);num=count(in_2==1);allocate(out(num,1))',r];
    kernel=[kernel,'          out(1:num,1)=pack(in_1(1:bb,1:1),reshape(in_2,(/bb,1/))==1)',r];
    kernel=[kernel,'         else',r];
    kernel=[kernel,'          if (in_2_m==1) then',r];
    kernel=[kernel,'           bb=size(in_2);allocate(out(bb,1))',r];
    kernel=[kernel,'           out(:,1)=in_1(',temp3{1},'in_2(1,:)',temp3{2},',1)',r];
    kernel=[kernel,'          elseif (in_2_n==1) then',r];
    kernel=[kernel,'           bb=size(in_2);allocate(out(bb,1))',r];
    kernel=[kernel,'           out(:,1)=in_1(',temp3{1},'in_2(:,1)',temp3{2},',1)',r];
    kernel=[kernel,'          else',r];
    kernel=[kernel,'           bb=size(in_2,1);num=size(in_2,2);allocate(out(bb,num))',r];
    kernel=[kernel,'           temp=reshape(in_1,(/size(in_1)/))',r];
    kernel=[kernel,'           temp2=reshape(in_2,(/bb*num/))',r];
    kernel=[kernel,'           out=reshape(temp(temp2),(/bb,num/))',r];
    kernel=[kernel,'          endif',r];
    kernel=[kernel,'         endif',r]; 
   end
   kernel=[kernel,'        else',r]; %Here we have a full matrix
   if strcmp(typestr(2),'l') %Logical indexing
    kernel=[kernel,'         if (in_2_m==1) then !row vector index',r];
    kernel=[kernel,'          bb=size(in_2);num=count(in_2);allocate(out(1,num))',r];
    kernel=[kernel,'          out(1,1:num)=pack(reshape(in_1,(/1,bb/)),in_2)',r];
    kernel=[kernel,'         else',r];
    kernel=[kernel,'          bb=size(in_2);num=count(in_2);allocate(out(num,1))',r];
    kernel=[kernel,'          out(1:num,1)=pack(reshape(in_1,(/1,bb/)),reshape(in_2,(/1,bb/)))',r];
    kernel=[kernel,'         endif',r];
   elseif strcmp(typestr(2),'r')|strcmp(typestr(2),'i') %Either real # or logical indexing w/ 0,1
    kernel=[kernel,'         if (all(in_2==0.or.in_2==1)) then !logical w/ 0,1',r];
    kernel=[kernel,'          if (in_2_m==1) then !row vector index',r];
    kernel=[kernel,'           bb=size(in_2);num=count(in_2==1);allocate(out(1,num))',r];
    kernel=[kernel,'           out(1,1:num)=pack(reshape(in_1,(/1,bb/)),in_2==1)',r];
    kernel=[kernel,'          else',r];
    kernel=[kernel,'           bb=size(in_2);num=count(in_2==1);allocate(out(num,1))',r];
    kernel=[kernel,'           out(1:num,1)=pack(reshape(in_1,(/1,bb/)),reshape(in_2,(/1,bb/))==1)',r];
    kernel=[kernel,'          endif',r];
    kernel=[kernel,'         else',r];
    kernel=[kernel,'          bb=size(in_2,1);num=size(in_2,2);allocate(out(bb,num))',r];
    kernel=[kernel,'          temp=reshape(in_1,(/size(in_1)/))',r];
    kernel=[kernel,'          temp2=reshape(in_2,(/bb*num/))',r];
    kernel=[kernel,'          out=reshape(temp(temp2),(/bb,num/))',r];
    kernel=[kernel,'         endif',r];
   end
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
