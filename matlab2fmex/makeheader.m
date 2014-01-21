function [modlist,funlist,len,typestr]=makeheader(funname,suborfun,typestr,modlist,funlist,r,w1d);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin<7 w1d=0; end
if w1d
 for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='x';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='w';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='y';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='z';end
 end 
else
 for m=1:length(typestr)
  temp=any(strcmp(typestr(m),{'d','m'})); if temp,typestr(m)='c';end
  temp=any(strcmp(typestr(m),{'e','n'})); if temp,typestr(m)='r';end
  temp=any(strcmp(typestr(m),{'f','o'})); if temp,typestr(m)='i';end
  temp=any(strcmp(typestr(m),{'g','p'})); if temp,typestr(m)='l';end
 end
end
len=length(typestr);
modlist=[modlist,...
	 '    module procedure ',funname,'f_',typestr,r];
if suborfun==2
 funlist=[funlist,...
	  ' function ',funname,'f_',typestr,'('];
 for ii=1:len 
  if ii<len
   funlist=[funlist,'in_',num2str(ii),','];
  else
   funlist=[funlist,'in_',num2str(ii),') result(out)',r];
  end
 end
else
 funlist=[funlist,...
	  ' subroutine ',funname,'f_',typestr,'(out,'];
 for ii=1:len 
  if ii<len
   funlist=[funlist,'in_',num2str(ii),','];
  else
   funlist=[funlist,'in_',num2str(ii),')',r];
  end
 end
end
for ii=1:len
 switch typestr(ii)
  case 'r'
   funlist=[funlist,'  real, dimension(:,:) :: in_',num2str(ii),r];
  case 'c'
   funlist=[funlist,'  complex, dimension(:,:) :: in_',num2str(ii),r];
  case 'i'
   funlist=[funlist,'  integer, dimension(:,:) :: in_',num2str(ii),r];
  case 'l'
   funlist=[funlist,'  logical, dimension(:,:) :: in_',num2str(ii),r];
  case 'w'
   funlist=[funlist,'  real, dimension(:) :: in_',num2str(ii),r];
  case 'x'
   funlist=[funlist,'  complex, dimension(:) :: in_',num2str(ii),r];
  case 'y'
   funlist=[funlist,'  integer, dimension(:) :: in_',num2str(ii),r];
  case 'z'
   funlist=[funlist,'  logical, dimension(:) :: in_',num2str(ii),r];
  case 's'
   funlist=[funlist,'  real :: in_',num2str(ii),r];
  case 't'
   funlist=[funlist,'  complex :: in_',num2str(ii),r];
  case 'u'
   funlist=[funlist,'  integer :: in_',num2str(ii),r];
  case 'v'
   funlist=[funlist,'  logical :: in_',num2str(ii),r];
 end
end
