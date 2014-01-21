clall

dd=builtin('dir');
ddc=struct2cell(dd);
ext='f90';
r=char(10);
funstr='';
tt=cputime;
num_files=0;
firstone='.'; started=0;

for ii=1:length(dd)
 t1=findstr(dd(ii).name,ext);
 if ~isempty(t1)
  foundf=0;
  temp=strncmp({ddc{1,:}},dd(ii).name,length(dd(ii).name)-2);
  if nnz(temp)>1
   foundf=1;
  end
  if ~isempty(strfind(dd(ii).name,firstone))
   started=1;
  end
  if foundf
   disp(dd(ii).name)
   if started
    f2matlab(dd(ii).name);
    num_files=num_files+1;
   end
  end
 end
end

filename='d1mach.m';
disp(['Converting constants in ',filename,' to Matlab equivalents.'])
fid=fopen(filename); filestr=fscanf(fid,'%c'); fclose(fid);
r=char(10);
%%%str1={'dmach(1)=[','dmach(1)=i1mach(10)^(i1mach(15)-1);';...
%%%      'dmach(2)=[','dmach(2)=i1mach(10)^(i1mach(16))*(1-i1mach(10)^(-i1mach(14)));';...
%%%      'dmach(3)=[','dmach(3)=i1mach(10)^(-i1mach(14));'
%%%      'dmach(4)=[','dmach(4)=i1mach(10)^(1-i1mach(14));'
%%%      'dmach(5)=[','dmach(5)=log10(i1mach(10));'};
str1={'dmach(1)=[','dmach(1)=realmin;';...
      'dmach(2)=[','dmach(2)=realmax;';...
      'dmach(3)=[','dmach(3)=eps/i1mach(10);';...
      'dmach(4)=[','dmach(4)=eps;';...
      'dmach(5)=[','dmach(5)=log10(i1mach(10));'};
for ii=1:length(str1)
 rets=findstr(r,filestr); rets=[0 rets];
 temp=strfind(filestr,str1{ii,1});
 temp2=rets(rets>temp);  temp2=temp2(1);
 filestr=[filestr(1:temp-1),str1{ii,2},filestr(temp2:end)];
end
fid=fopen(filename,'w');  fprintf(fid,'%c',filestr);   fclose(fid);

filename='r1mach.m';
disp(['Converting constants in ',filename,' to Matlab equivalents.'])
fid=fopen(filename); filestr=fscanf(fid,'%c'); fclose(fid);
r=char(10);
%%%str1={'dmach(1)=[','dmach(1)=i1mach(10)^(i1mach(15)-1);';...
%%%      'dmach(2)=[','dmach(2)=i1mach(10)^(i1mach(16))*(1-i1mach(10)^(-i1mach(14)));';...
%%%      'dmach(3)=[','dmach(3)=i1mach(10)^(-i1mach(14));'
%%%      'dmach(4)=[','dmach(4)=i1mach(10)^(1-i1mach(14));'
%%%      'dmach(5)=[','dmach(5)=log10(i1mach(10));'};
str1={'rmach(1)=[','rmach(1)=realmin;';...
      'rmach(2)=[','rmach(2)=realmax;';...
      'rmach(3)=[','rmach(3)=eps/i1mach(10);';...
      'rmach(4)=[','rmach(4)=eps;';...
      'rmach(5)=[','rmach(5)=log10(i1mach(10));'};
for ii=1:length(str1)
 rets=findstr(r,filestr); rets=[0 rets];
 temp=strfind(filestr,str1{ii,1});
 temp2=rets(rets>temp);  temp2=temp2(1);
 filestr=[filestr(1:temp-1),str1{ii,2},filestr(temp2:end)];
end
fid=fopen(filename,'w');  fprintf(fid,'%c',filestr);   fclose(fid);




disp(['Total number of files => ',num2str(num_files)])
disp(['Total time            => ',num2str(cputime-tt)])
