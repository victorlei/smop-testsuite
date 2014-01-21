function outstr=makeMexcallbackmodule(maxmxinputs,maxmxinputs0,alpha)
beginstr='';middlestr='';endstr='';
modlist='';funlist='';
r=char(10);
beginstr=[beginstr,...
	  'module mexcallback',r];
middlestr=[middlestr,...
	   'contains',r];
endstr=[endstr,...
	'end module mexcallback',r];
modlist=[modlist,...
	 ' interface mlcall',r];
for j=1:length(maxmxinputs)
 typestr=maxmxinputs{j};
 for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
 i=length(typestr);
 modlist=[modlist,...
          '  module procedure callback_',typestr,r];
 %Now for the function definitions
 funlist=[funlist,...
          ' subroutine callback_',typestr,'('];
 temp1='';if alpha, temp1='*8'; end
 funlist=[funlist,'out,'];
 for k=1:i
  if k~=i
   funlist=[funlist,'in_',num2str(k),','];
  else
   funlist=[funlist,'in_',num2str(k),',funstr)',r];
  end
 end
 for k=1:i
  temp2{1,k}='';  temp2{2,k}='';
  switch typestr(k)
   case 'r'
    funlist=[funlist,' real, dimension(:,:) :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
   case 'c'
    funlist=[funlist,' complex, dimension(:,:) :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_r_ptr',r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_i_ptr',r];
   case 'i'
    funlist=[funlist,' integer, dimension(:,:) :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
    temp2{1,k}='real(';    temp2{2,k}=')';
   case 's'
    funlist=[funlist,' real :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
   case 't'
    funlist=[funlist,' complex :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_r_ptr',r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_i_ptr',r];
   case 'u'
    funlist=[funlist,' integer :: in_',num2str(k),r];
    funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
    temp2{1,k}='real(';    temp2{2,k}=')';
  end
 end
 if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
  funlist=[funlist,' real, pointer, dimension(:,:) :: out',r];
  funlist=[funlist,' integer',temp1,' :: out_ptr',r];
 else
  funlist=[funlist,' complex, pointer, dimension(:,:) :: out',r];
  funlist=[funlist,' integer',temp1,' :: out_r_ptr',r];
  funlist=[funlist,' integer',temp1,' :: out_i_ptr',r];
 end
 funlist=[funlist,' character (len=*) :: funstr',r];
 funlist=[funlist,' integer',temp1,' lhs(50), rhs(50)',r];
 funlist=[funlist,' integer out_m, out_n, '];
 for k=1:i
  if k~=i
   funlist=[funlist,'in_',num2str(k),'_m, in_',num2str(k),'_n, '];
  else
   funlist=[funlist,'in_',num2str(k),'_m, in_',num2str(k),'_n',r];
  end
 end
 %Get size of all inputs
 for k=1:i
  if ((typestr(k)=='r')|(typestr(k)=='c')|(typestr(k)=='i'))
   funlist=[funlist,'  in_',num2str(k),'_m=size(in_',num2str(k),',dim=1); in_',num2str(k),'_n=size(in_',num2str(k),',dim=2)',r];
  else
   funlist=[funlist,'  in_',num2str(k),'_m=1; in_',num2str(k),'_n=1',r];
  end
 end
 %Create input matrices for matlab call
 for k=1:i
  if ((typestr(k)=='r')|(typestr(k)=='s')|(typestr(k)=='i')|(typestr(k)=='u'))
   funlist=[funlist,'  rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,0)',r];
   funlist=[funlist,'  in_',num2str(k),'_ptr=mxGetPr(rhs(',num2str(k),'))',r];
   funlist=[funlist,'  call mxCopyReal8ToPtr(',temp2{1,k},'in_',num2str(k),temp2{2,k},',in_',num2str(k),'_ptr,in_',num2str(k),'_m*in_',num2str(k),'_n)',r];
   funlist=[funlist,'  call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
  else
   funlist=[funlist,'  rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,1)',r];
   funlist=[funlist,'  in_',num2str(k),'_r_ptr=mxGetPr(rhs(',num2str(k),'))',r];
   funlist=[funlist,'  in_',num2str(k),'_i_ptr=mxGetPi(rhs(',num2str(k),'))',r];
   funlist=[funlist,'  call mxCopyComplex16ToPtr(in_',num2str(k),',in_',num2str(k),'_r_ptr,in_',num2str(k),'_i_ptr,in_',num2str(k),'_m*in_',num2str(k),'_n)',r];
   funlist=[funlist,'  call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
  end
 end
 funlist=[funlist,'  call mexCallMATLAB(1,lhs(1),',num2str(k),',rhs(1:',num2str(k),'),funstr)',r];
 funlist=[funlist,'  out_m=mxGetM(lhs(1));out_n=mxGetN(lhs(1))',r];
 funlist=[funlist,'  allocate(out(out_m,out_n))',r];
 if (length(findstr(typestr,'c'))+length(findstr(typestr,'t')))==0
  funlist=[funlist,'  out_ptr=mxGetPr(lhs(1))',r];
  funlist=[funlist,'  call mxCopyPtrToReal8(out_ptr,out,out_m*out_n)',r];
 else
  funlist=[funlist,'  out_r_ptr=mxGetPr(lhs(1))',r];
  funlist=[funlist,'  out_i_ptr=mxGetPi(lhs(1))',r];
  funlist=[funlist,'  call mxCopyPtrToComplex16(out_r_ptr,out_i_ptr,out,out_m*out_n)',r];
 end
 funlist=[funlist,' end subroutine callback_',typestr,r];  
 funlist=[funlist,r];
end
modlist=[modlist...
	 ' end interface mlcall',r,r];


if length(maxmxinputs0)>0
 modlist=[modlist,...
	  ' interface mlcall0',r];
 for j=1:length(maxmxinputs0)
  typestr=maxmxinputs0{j};
  for m=1:length(typestr)
   temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
   temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
   temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
   temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
  end
  i=length(typestr);
  modlist=[modlist,...
           '  module procedure callback0_',typestr,r];
  %Now for the function definitions
  funlist=[funlist,...
           ' subroutine callback0_',typestr,'('];
  for k=1:i, 
   funlist=[funlist,'in_',num2str(k),','];
  end
  funlist=[funlist,'funstr)',r];
  for k=1:i
   switch typestr(k)
    case 'r'
     funlist=[funlist,' real, dimension(:,:) :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
    case 'c'
     funlist=[funlist,' complex, dimension(:,:) :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_r_ptr',r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_i_ptr',r];
    case 'i'
     funlist=[funlist,' integer, dimension(:,:) :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
    case 's'
     funlist=[funlist,' real :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
    case 't'
     funlist=[funlist,' complex :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_r_ptr',r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_i_ptr',r];
    case 'u'
     funlist=[funlist,' integer :: in_',num2str(k),r];
     funlist=[funlist,' integer',temp1,' :: in_',num2str(k),'_ptr',r];
   end
  end
  funlist=[funlist,' character (len=*) :: funstr',r];
  funlist=[funlist,' integer',temp1,' lhs(50), rhs(50)',r];
  if i~=0, funlist=[funlist,' integer ']; end
  for k=1:i
   if k~=i
    funlist=[funlist,'in_',num2str(k),'_m, in_',num2str(k),'_n, '];
   else
    funlist=[funlist,'in_',num2str(k),'_m, in_',num2str(k),'_n',r];
   end
  end
  for k=1:i
   if ((typestr(k)=='r')|(typestr(k)=='c')|(typestr(k)=='i'))
    funlist=[funlist,'  in_',num2str(k),'_m=size(in_',num2str(k),',dim=1); in_',num2str(k),'_n=size(in_',num2str(k),',dim=2)',r];
   else
    funlist=[funlist,'  in_',num2str(k),'_m=1; in_',num2str(k),'_n=1',r];
   end
  end
  for k=1:i
   if ((typestr(k)=='r')|(typestr(k)=='s')|(typestr(k)=='i')|(typestr(k)=='u'))
    funlist=[funlist,'  rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,0)',r];
    funlist=[funlist,'  in_',num2str(k),'_ptr=mxGetPr(rhs(',num2str(k),'))',r];
    funlist=[funlist,'  call mxCopyReal8ToPtr(in_',num2str(k),',in_',num2str(k),'_ptr,in_',num2str(k),'_m*in_',num2str(k),'_n)',r];
    funlist=[funlist,'  call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
   else
    funlist=[funlist,'  rhs(',num2str(k),')=mxCreateFull(in_',num2str(k),'_m,in_',num2str(k),'_n,1)',r];
    funlist=[funlist,'  in_',num2str(k),'_r_ptr=mxGetPr(rhs(',num2str(k),'))',r];
    funlist=[funlist,'  in_',num2str(k),'_i_ptr=mxGetPi(rhs(',num2str(k),'))',r];
    funlist=[funlist,'  call mxCopyComplex16ToPtr(in_',num2str(k),',in_',num2str(k),'_r_ptr,in_',num2str(k),'_i_ptr,in_',num2str(k),'_m*in_',num2str(k),'_n)',r];
    funlist=[funlist,'  call mxSetM(rhs(',num2str(k),'),in_',num2str(k),'_m);call mxSetN(rhs(',num2str(k),'),in_',num2str(k),'_n)',r];
   end
  end
  if i==0
   funlist=[funlist,'  call mexCallMATLAB(0,NULL,0,NULL',num2str(k),',funstr)',r];
  else
   funlist=[funlist,'  call mexCallMATLAB(0,NULL,',num2str(k),',rhs(1:',num2str(k),'),funstr)',r];
  end
  funlist=[funlist,' end subroutine callback0_',typestr,r];  
  funlist=[funlist,r];
  %end
 end
 modlist=[modlist...
	  ' end interface mlcall0',r];
end 
outstr=[beginstr,modlist,middlestr,funlist,endstr];
