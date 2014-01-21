function tempstr=makemultioutput(i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,inoutother,keywords,localvartype,needed_interfaces,filename_all,make_words,cw,multinum,howmany,subscripts,centercomma,parens,howmany2,subscripts2,centercomma2,parens2);
tempstr=cell(2,1);r=char(10);
typestr{1}=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
typestr{2}=makeMATLABcallstring(howmany2,subscripts2,centercomma2,parens2,i,j);
howmany3=zeros(1,howmany2);
for k=1:howmany2
 if k==1,leftside=parens2(1);rightside=centercomma2(1);
 elseif k==howmany2,leftside=centercomma2(k-1);rightside=parens2(2);
 else leftside=centercomma2(k-1);rightside=centercomma2(k);
 end
 temp=find((funstrwords_b{i}<rightside)&(funstrwords_b{i}>leftside));
 howmany3(k)=hassubscript(i,temp(1));
end
tempstr{1}=['       interface mlcallm',num2str(multinum),r...
    '        module procedure callbackm',num2str(multinum),r,...
    '       end interface mlcallm',num2str(multinum),r];
tempstr{2}=['       subroutine callbackm',num2str(multinum),'('];
for k=1:howmany
 if any(strcmp(typestr{1}(k),{'i';'u'}))
  tempstr{2}=[tempstr{2},'in_i_',num2str(k),','];
 else
  tempstr{2}=[tempstr{2},'in_',num2str(k),','];
 end
end
for k=1:howmany2
 tempstr{2}=[tempstr{2},'out_',num2str(k),',']; 
 if howmany3(k)==2
  for ii=1:howmany3(k)
   tempstr{2}=[tempstr{2},'out_',num2str(k),'_',num2str(ii),',']; 
  end
 end
end
tempstr{2}=[tempstr{2},'funstr)',r];
i=length(typestr{1});
for k=1:i
 switch typestr{1}(k)
  case 'r'
   tempstr{2}=[tempstr{2},'       real, dimension(:,:) :: in_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_ptr',r];
  case 'c'
   tempstr{2}=[tempstr{2},'       complex, dimension(:,:) :: in_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_r_ptr',r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_i_ptr',r];
  case 'i'
   tempstr{2}=[tempstr{2},'       integer, dimension(:,:) :: in_i_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_ptr',r];
   tempstr{2}=[tempstr{2},'       real, allocatable, dimension(:,:) :: in_',num2str(k),r];
  case 's'
   tempstr{2}=[tempstr{2},'       real :: in_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_ptr',r];
  case 't'
   tempstr{2}=[tempstr{2},'       complex :: in_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_r_ptr',r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_i_ptr',r];
  case 'u'
   tempstr{2}=[tempstr{2},'       integer :: in_i_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: in_',num2str(k),'_ptr',r];
   tempstr{2}=[tempstr{2},'       real :: in_',num2str(k),r];
 end
end
i=length(typestr{2});
for k=1:i
 switch typestr{2}(k)
  case 'r'
   tempstr{2}=[tempstr{2},'       real, pointer, dimension(:,:) :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_ptr',r];
  case 'c'
   tempstr{2}=[tempstr{2},'       complex, pointer, dimension(:,:) :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_r_ptr',r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_i_ptr',r];
  case 'i'
   tempstr{2}=[tempstr{2},'       integer, pointer, dimension(:,:) :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_ptr',r];
  case 's'
   tempstr{2}=[tempstr{2},'       real :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_ptr',r];
  case 't'
   tempstr{2}=[tempstr{2},'       complex :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_r_ptr',r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_i_ptr',r];
  case 'u'
   tempstr{2}=[tempstr{2},'       integer :: out_',num2str(k),r];
   tempstr{2}=[tempstr{2},'       integer, pointer :: out_',num2str(k),'_ptr',r];
 end
 if howmany3(k)==2
  for ii=1:howmany3(k)
   tempstr{2}=[tempstr{2},'       integer, dimension(:) :: out_',num2str(k),'_',num2str(ii),r];
  end
 end  
end
tempstr{2}=[tempstr{2},'       character (len=*) :: funstr',r];
tempstr{2}=[tempstr{2},'       integer lhs(50), rhs(50)',r];
tempstr{2}=[tempstr{2},'       integer '];
for k=1:howmany
 if k~=howmany
  tempstr{2}=[tempstr{2},'in_',num2str(k),'_m, in_',num2str(k),'_n, '];
 else
  tempstr{2}=[tempstr{2},'in_',num2str(k),'_m, in_',num2str(k),'_n',r];
 end
end
tempstr{2}=[tempstr{2},'       integer '];
for k=1:howmany2
 if k~=howmany2
  tempstr{2}=[tempstr{2},'out_',num2str(k),'_m, out_',num2str(k),'_n, '];
 else
  tempstr{2}=[tempstr{2},'out_',num2str(k),'_m, out_',num2str(k),'_n',r];
 end
end
for k=1:howmany
 if strcmp(typestr{1}(k),'i')
  tempstr{2}=[tempstr{2},'        allocate(in_',num2str(k),'(size(in_i_',num2str(k),',dim=1),size(in_i_',num2str(k),',dim=2)))',r]; 
 end
 if ((strcmp(typestr{1}(k),'i'))|(strcmp(typestr{1}(k),'u')))
  tempstr{2}=[tempstr{2},'        in_',num2str(k),'=in_i_',num2str(k),r]; 
 end
 if ((strcmp(typestr{1}(k),'r'))|(strcmp(typestr{1}(k),'c'))|(strcmp(typestr{1}(k),'i')))
    tempstr{2}=[tempstr{2},'        in_',num2str(k),'_m=size(in_',num2str(k),',dim=1); in_',num2str(k),'_n=size(in_',num2str(k),',dim=2)',r];
 else
  tempstr{2}=[tempstr{2},'        in_',num2str(k),'_m=1; in_',num2str(k),'_n=1',r];
 end
end
% for k=1:howmany2
%  if howmany3(k)==2
%   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_m=size(out_',num2str(k),'_1);out_',num2str(k),'_n=size(out_',num2str(k),'_2)',r];
%  else
%   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_m=size(out_',num2str(k),',dim=1);out_',num2str(k),'_n=size(out_',num2str(k),',dim=2)',r];
%  end
% end
%do the out cars allocating
for k=1:howmany
 if ((strcmp(typestr{1}(k),'r'))|(strcmp(typestr{1}(k),'s'))|(strcmp(typestr{1}(k),'i'))|(strcmp(typestr{1}(k),'u')))
  tempstr{2}=[tempstr{2},'        rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,0)',r];
  tempstr{2}=[tempstr{2},'        in_',num2str(k),'_ptr=>mxGetPr(rhs(',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        call fill(in_',num2str(k),'_ptr,in_',num2str(k),'_m,in_',num2str(k),'_n,in_',num2str(k),')',r];
  tempstr{2}=[tempstr{2},'        call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
 else
  tempstr{2}=[tempstr{2},'        rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,1)',r];
  tempstr{2}=[tempstr{2},'        in_',num2str(k),'_r_ptr=>mxGetPr(rhs(',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        in_',num2str(k),'_i_ptr=>mxGetPi(rhs(',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        call fill(in_',num2str(k),'_r_ptr,in_',num2str(k),'_m,in_',num2str(k),'_n,real(in_',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        call fill(in_',num2str(k),'_i_ptr,in_',num2str(k),'_m,in_',num2str(k),'_n,aimag(in_',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
 end
end
tempstr{2}=[tempstr{2},...
	    '        call mexCallMATLAB(',num2str(howmany2),',lhs(1:',num2str(howmany2),'),',num2str(howmany),',rhs(1:',num2str(howmany),'),funstr)',r];
for k=1:howmany2
 if ((strcmp(typestr{2}(k),'r'))|(strcmp(typestr{2}(k),'c'))|(strcmp(typestr{2}(k),'i')))
  tempstr{2}=[tempstr{2},'        out_',num2str(k),'_m=mxGetM(lhs(',num2str(k),'));out_',num2str(k),'_n=mxGetN(lhs(',num2str(k),'))',r];
  tempstr{2}=[tempstr{2},'        allocate(out_',num2str(k),'(out_',num2str(k),'_m,out_',num2str(k),'_n))',r];
 end
 switch typestr{2}(k)
  case 'r'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call fill(out_',num2str(k),',out_',num2str(k),'_m,out_',num2str(k),'_n,out_',num2str(k),'_ptr)',r];
  case 'c'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_r_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_i_ptr=>mxGetPi(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call fillc(out_',num2str(k),',out_',num2str(k),'_m,out_',num2str(k),'_n,out_',num2str(k),'_r_ptr,out_',num2str(k),'_i_ptr)',r];
  case 'i'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call filli(out_',num2str(k),',out_',num2str(k),'_m,out_',num2str(k),'_n,out_',num2str(k),'_ptr)',r];
  case 's'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call fill(out_',num2str(k),',1,1,out_',num2str(k),'_ptr)',r];
  case 't'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_r_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_i_ptr=>mxGetPi(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call fillc(out_',num2str(k),',1,1,out_',num2str(k),'_r_ptr,out_',num2str(k),'_i_ptr)',r];
  case 'i'
   tempstr{2}=[tempstr{2},'        out_',num2str(k),'_ptr=>mxGetPr(lhs(',num2str(k),'))',r];
   tempstr{2}=[tempstr{2},'        call filli(out_',num2str(k),',1,1,out_',num2str(k),'_ptr)',r];
 end
end
tempstr{2}=[tempstr{2},...
	    '       end subroutine callbackm',num2str(multinum),r];
