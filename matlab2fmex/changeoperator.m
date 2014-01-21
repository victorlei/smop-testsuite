function [changedflag,nif]=changeoperator(i,operator,dummy);
declare_globals
nif=cell(2,1);neednif=0;r=char(10);
changedflag=0;
temp=find(~isspace(funstr{i}));
temp=temp(temp>(dummy+length(operator)-1));temp=temp(1);
temp1=strcmp('''',operator(length(operator)));
%First the right side.
if ~temp1
 if isnameletter(funstr{i}(temp))
  temp2=find(funstrwords_b{i}==temp);
  [howmany,subscripts,centercomma,parens]=hassubscript(i,temp2);
  if howmany>0
   rightarg_b=temp;
   rightarg_e=parens(2);
  else
   rightarg_b=temp;
   rightarg_e=funstrwords_e{i}(temp2);
  end
 elseif isnumber(funstr{i}(temp))
  temp2=find(funstrnumbers_b{i}==temp);
  rightarg_b=temp;
  rightarg_e=funstrnumbers_e{i}(temp2);
 elseif ((strcmp(funstr{i}(temp),'('))|(strcmp(funstr{i}(temp),'[')))
  rightarg_b=temp;
  rightarg_e=findrights(temp,funstr{i});
 end
end
%Next the left side.
temp=find(~isspace(funstr{i}));temp=temp(temp<dummy);temp=temp(length(temp));
if length(find(temp==funstrnumbers_e{i}))>0
 temp2=find(funstrnumbers_e{i}==temp);
 leftarg_e=temp;
 leftarg_b=funstrnumbers_b{i}(temp2);
elseif length(find(temp==funstrwords_e{i}))>0
 temp2=find(funstrwords_e{i}==temp);
 leftarg_e=temp;
 leftarg_b=funstrwords_b{i}(temp2);
elseif ((strcmp(funstr{i}(temp),')'))|(strcmp(funstr{i}(temp),']')))
 leftarg_e=temp;
 leftparen=findlefts(temp,funstr{i});
 if length(find(funstrwords_e{i}==(leftparen-1)))>0
  temp2=find(funstrwords_e{i}==(leftparen-1));
  leftarg_b=funstrwords_b{i}(temp2);
 else
  leftarg_b=leftparen;
 end
end
temp4=find(strcmp(operator,{operators{:,1}}));
if ~temp1
 %So we have anything but a transpose operator
 if want_op==1
  funstr{i}=[funstr{i}(1:(leftarg_b-1)),operators{temp4,2},'(',funstr{i}(leftarg_b:leftarg_e),',',funstr{i}(rightarg_b:rightarg_e),')',funstr{i}((rightarg_e+1):length(funstr{i}))];
  neednif=1;
  changedflag=1;
 else
  if strcmp(operator,'*')|strcmp(operator,'^')|strcmp(operator,'.^')
   centercomma=[];
   howmany=1;
   subscripts=[];
   subscripts{1}=funstr{i}([leftarg_b:leftarg_e]);
   parens(1)=leftarg_b;   parens(2)=leftarg_e;
   tempstr(1)=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,inf);
   howmany=1;
   subscripts=[];
   subscripts{1}=funstr{i}([rightarg_b:rightarg_e]);
   parens(1)=rightarg_b;   parens(2)=rightarg_e;
   tempstr(2)=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,inf);
   if any(strcmp(tempstr(1),typs{12})),temp3(1)=0;else temp3(1)=1;end
   if any(strcmp(tempstr(2),typs{12})),temp3(2)=0;else temp3(2)=1;end
  end   
  switch operator
   case '+'
    %Nothing ever!
   case '-'
    %Nothing ever!
   case '/'
    %Nothing ever!
   case './'
    funstr{i}=[funstr{i}(1:dummy-1),funstr{i}(dummy+1:length(funstr{i}))];
    changedflag=1;
   case '*'
    temp5=cell(2,4);
    temp5{1,1}='';         temp5{1,2}='';         temp5{1,3}='';         temp5{1,4}='';
    temp5{2,1}='';         temp5{2,2}='';         temp5{2,3}='';         temp5{2,4}='';
    if want_op~=-1
     if ((temp3(1)==0)&(temp3(2)==0))
      for k=1:2
       if any(strcmp(tempstr(k),typs{4}))
        temp5{k,1}='spread(';    temp5{k,2}=',1,1)';
       elseif any(strcmp(tempstr(2),typs{5}))
        temp5{k,3}='[';         temp5{k,4}=']';
       end
      end
      %funstr{i},funstr{i}(leftarg_b:leftarg_e),funstr{i}(rightarg_b:rightarg_e),temp3,kb
      funstr{i}=[funstr{i}(1:(leftarg_b-1)),operators{temp4,2},'(',temp5{1,1},temp5{1,3},funstr{i}(leftarg_b:leftarg_e),temp5{1,4},temp5{1,2},',',temp5{2,1},temp5{2,3},funstr{i}(rightarg_b:rightarg_e),temp5{2,4},temp5{2,2},')',funstr{i}((rightarg_e+1):length(funstr{i}))];
      %funstr{i},temp3,kb
      neednif=1;
      changedflag=1;
     end
    end
   case '.*'
    funstr{i}=[funstr{i}(1:dummy-1),funstr{i}(dummy+1:length(funstr{i}))];
    changedflag=1;
   case '^'
    if want_op~=-1
     if ((temp3(1)==1)&(temp3(2)==1))|((temp3(1)==1)&(temp3(2)==0))|((temp3(1)==0)&(temp3(2)==0))
      funstr{i}=[funstr{i}(1:dummy-1),'**',funstr{i}(dummy+1:length(funstr{i}))];
      changedflag=1;
     end
     if ((temp3(1)==0)&(temp3(2)==1))
      funstr{i}=[funstr{i}(1:(leftarg_b-1)),operators{temp4,2},'(',funstr{i}(leftarg_b:leftarg_e),',',funstr{i}(rightarg_b:rightarg_e),')',funstr{i}((rightarg_e+1):length(funstr{i}))];
      neednif=1;
      changedflag=1;
     end     
    else
     funstr{i}=[funstr{i}(1:dummy-1),'**',funstr{i}(dummy+1:length(funstr{i}))];
     changedflag=1;     
    end
   case '.^'   %Nothing ever! Just replace with **
    funstr{i}=[funstr{i}(1:dummy-1),'**',funstr{i}(dummy+2:length(funstr{i}))];
    changedflag=1;     
  end
 end
else
%%% funstr{i},funstr{i}(leftarg_b:leftarg_e)
 howmany=1;
%%% try
 subscripts{1}=funstr{i}([leftarg_b:leftarg_e]);
%%% catch
%%%  'pooooou',funstr{i},kb,funstr{i}([leftarg_b:leftarg_e])
%%% end
 parens(1)=leftarg_b;   parens(2)=leftarg_e;
 tempstr=makeMATLABcallstring(howmany,subscripts,[leftarg_b leftarg_e],parens,i,inf);
 if any(strcmp(tempstr(1),typs{11})),temp3(1)=0;else temp3(1)=1;end
 %tempstr=makeMATLABcallstring(1,{funstr{i}(leftarg_b:leftarg_e)},[],[leftarg_b,leftarg_e],i,j);
 if any(strcmp(tempstr(1),typs{3}))       %Complex argument
  temp5{1}='conjg(';  temp5{2}=')';
 else                                     %Real argument
  temp5{1}='';        temp5{2}='';
 end
 if strcmp('.''',operators{temp4,1})     %Regular transpose
  if temp3(1)
   funstr{i}=[funstr{i}(1:leftarg_e),funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
  else 
   funstr{i}=[funstr{i}(1:(leftarg_b-1)),'transpose(',funstr{i}(leftarg_b:leftarg_e),')',funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
  end
 else                                    %Conjugate transpose
  if temp3(1)
   funstr{i}=[funstr{i}(1:(leftarg_b-1)),temp5{1},funstr{i}(leftarg_b:leftarg_e),temp5{2},funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
  else 
   funstr{i}=[funstr{i}(1:(leftarg_b-1)),'transpose(',temp5{1},funstr{i}(leftarg_b:leftarg_e),temp5{2},')',funstr{i}((leftarg_e+1+length(operator)):length(funstr{i}))];
  end
 end
 changedflag=1;
%%% funstr{i},kb
end
if changedflag
 updatefunstr(i);
end
if neednif
 switch operator
  case '.'''
   %Never needed!
  case ''''
   %Never needed!
  case '^'
   nif{1}=[];
   nif{1}=[nif{1},'       interface mxpow',r];
   nif{1}=[nif{1},'        module procedure mxpow_r2r',r];
   nif{1}=[nif{1},'        module procedure mxpow_r2i',r];
   nif{1}=[nif{1},'        module procedure mxpow_c2r',r];
   nif{1}=[nif{1},'        module procedure mxpow_c2i',r];
   nif{1}=[nif{1},'        module procedure mxpow_r2r2',r];
   nif{1}=[nif{1},'        module procedure mxpow_r2i2',r];
   nif{1}=[nif{1},'        module procedure mxpow_c2r2',r];
   nif{1}=[nif{1},'        module procedure mxpow_c2i2',r];
   nif{1}=[nif{1},'        module procedure mxpow_i2r',r];
   nif{1}=[nif{1},'        module procedure mxpow_i2i',r];
   nif{1}=[nif{1},'        module procedure mxpow_i2r2',r];
   nif{1}=[nif{1},'        module procedure mxpow_i2i2',r];
   nif{1}=[nif{1},'       end interface mxpow',r];
   nif{2}=[];
   nif{2}=[nif{2},'',r];
   nif{2}=[nif{2},'c      --- mxpow ==> Matlab power !! This can only be called with a=>square, b=>1x1 (integer)',r];
   nif{2}=[nif{2},'       function mxpow_r2r(a,b) result(out)',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0.0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_r2r',r];
   nif{2}=[nif{2},'       function mxpow_r2i(a,b) result(out)',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_r2i',r];
   nif{2}=[nif{2},'       function mxpow_c2r(a,b) result(out)',r];
   nif{2}=[nif{2},'        complex, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real :: b',r];
   nif{2}=[nif{2},'        complex, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0.0) then',r];
   nif{2}=[nif{2},'         out=(1.0,0.0)',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_c2r',r];
   nif{2}=[nif{2},'       function mxpow_c2i(a,b) result(out)',r];
   nif{2}=[nif{2},'        complex, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer :: b',r];
   nif{2}=[nif{2},'        complex, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0) then',r];
   nif{2}=[nif{2},'         out=(1.0,0.0)',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_c2i',r];
   nif{2}=[nif{2},'       function mxpow_r2r2(a,b) result(out)',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0.0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_r2r2',r];
   nif{2}=[nif{2},'       function mxpow_r2i2(a,b) result(out)',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_r2i2',r];
   nif{2}=[nif{2},'       function mxpow_c2r2(a,b) result(out)',r];
   nif{2}=[nif{2},'        complex, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        complex, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0.0) then',r];
   nif{2}=[nif{2},'         out=(1.0,0.0)',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_c2r2',r];
   nif{2}=[nif{2},'       function mxpow_c2i2(a,b) result(out)',r];
   nif{2}=[nif{2},'        complex, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        complex, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0) then',r];
   nif{2}=[nif{2},'         out=(1.0,0.0)',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_c2i2',r];
   nif{2}=[nif{2},'       function mxpow_i2r(a,b) result(out)',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0.0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_i2r',r];
   nif{2}=[nif{2},'       function mxpow_i2i(a,b) result(out)',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer :: b',r];
   nif{2}=[nif{2},'        integer, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b==0) then',r];
   nif{2}=[nif{2},'         out=1',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_i2i',r];
   nif{2}=[nif{2},'       function mxpow_i2r2(a,b) result(out)',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        real, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        real, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m, a_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0.0) then',r];
   nif{2}=[nif{2},'         out=1.0',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_i2r2',r];
   nif{2}=[nif{2},'       function mxpow_i2i2(a,b) result(out)',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: a',r];
   nif{2}=[nif{2},'        integer, dimension(:,:) :: b',r];
   nif{2}=[nif{2},'        integer, dimension(size(a,dim=1),size(a,dim=2)) :: out',r];
   nif{2}=[nif{2},'        integer a_m,ia_n, b_m, b_n, i, j',r];
   nif{2}=[nif{2},'        a_m=size(a,dim=1);a_n=size(a,dim=2);',r];
   nif{2}=[nif{2},'        if (b(1,1)==0) then',r];
   nif{2}=[nif{2},'         out=1',r];
   nif{2}=[nif{2},'        else',r];
   nif{2}=[nif{2},'         out=a',r];
   nif{2}=[nif{2},'         do i=1,int(b(1,1)-1)',r];
   nif{2}=[nif{2},'          out=matmul(out,a)',r];
   nif{2}=[nif{2},'         enddo',r];
   nif{2}=[nif{2},'        endif',r];
   nif{2}=[nif{2},'       end function mxpow_i2i2',r];
  case '.^'
   %Never needed!
  case '*'
   %Already taken care of by matmul
  case '/'
   %Never needed!
  case '.*'
   %Already correct
  case './'
   %Already correct
  case '+'
   %Never needed!
  case '-'
   %Never needed!
  case '\'
   %Should we do this?
 end
end
