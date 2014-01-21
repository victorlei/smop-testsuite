function filestr=make_mex_subroutine(filename,inoutother,cw,vararginout,localvartype,suborfun,want_fb,want_kb,recursive)
%Suborfun==0 then subroutine, 1 then function
innum=length(inoutother{1});
outnum=length(inoutother{2});
othernum=length(inoutother{3});
fid=fopen([filename,'.f90'],'w');
r=char(10);ret=1;
temp=''; if recursive, temp='recursive '; end
filestr='';
if suborfun==0
 filestr=[filestr,'',temp,'subroutine ',filename,'('];
else
 filestr=[filestr,'',temp,'function ',filename,'('];
end
if suborfun==0
 for i=1:length(inoutother{2})
  filestr=[filestr,inoutother{2}{i},','];
 end
end
for i=1:length(inoutother{1})-1
 filestr=[filestr,inoutother{1}{i},','];
end
filestr=[filestr,inoutother{1}{end},')'];
if suborfun==1
 filestr=[filestr,' result(',inoutother{2}{1},')',r];
else
 filestr=[filestr,r];
end
% Create calling variables
filestr=[filestr,'!     size variables',r];
%filestr=[filestr,'!     First create all the calling variables. ',r];
%filestr=[filestr,'!     REM*** The changes are returned to the caller.',r];
for i=1:length(inoutother{1})
 if prod(size(getfield(cw,inoutother{1}{i})))==1
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,'real ',inoutother{1}{i},r];
  else
   filestr=[filestr,'complex ',inoutother{1}{i},r];
  end
 else
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,'real :: ',inoutother{1}{i},'(:,:)',r];
  else
   filestr=[filestr,'complex :: ',inoutother{1}{i},'(:,:)',r];
  end
 end
end
%Let's assign a vararginout for those outputs who have no association
for i=(length(vararginout)+1):outnum
  temp3=[0 0];
 for j=1:length(inoutother{1})
  if ((size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1))&(size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)))
   temp3(1)=j;temp3(2)=j;temp5{i}{1}='1';temp5{i}{2}='2';
   if want_fb|want_kb
    if i<11
     disp(['  Setting the size of output var ',inoutother{2}{i},' equal to the input var ',inoutother{1}{j},'.']);
    end
   end
   break
  end
  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1)
   temp3(1)=j;temp5{i}{1}='1';
  end
  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),2)
   temp3(1)=j;temp5{i}{1}='2';
  end
  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),1)
   temp3(2)=j;temp5{i}{2}='1';
  end
  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)
   temp3(2)=j;temp5{i}{2}='2';
  end
 end
 if all(temp3)
  vararginout{i}{1}{1}=inoutother{1}{temp3(1)};
  if ~isreal(getfield(cw,vararginout{i}{1}{1}))
   temp4{i}{1}='';else,temp4{i}{1}='';
  end
  vararginout{i}{1}{2}=inoutother{1}{temp3(2)};
  if ~isreal(getfield(cw,vararginout{i}{1}{2}))
   temp4{i}{2}='';else,temp4{i}{2}='';
  end
  vararginout{i}{2}=~isreal(getfield(cw,inoutother{2}{i}));
 else
  error(['output variable ',inoutother{2}{i},'''s size does not match any input variable.',r,'Make an input variable the same size as ',inoutother{2}{i},' (in this case ',num2str(size(getfield(cw,inoutother{2}{i}),1)),',',num2str(size(getfield(cw,inoutother{2}{i}),2)),').']);
 end
end
for i=1:length(inoutother{2})
 if prod(size(getfield(cw,inoutother{2}{i})))>1
  if ((size(getfield(cw,inoutother{2}{i}),1)>1)&(size(getfield(cw,inoutother{2}{i}),2)>1))
   if isreal(getfield(cw,inoutother{2}{i}))
    filestr=[filestr,'real ',inoutother{2}{i},'(size(',vararginout{i}{1}{1},',',temp5{i}{1},'),size(',vararginout{i}{1}{2},',',temp5{i}{2},'))',r];
   else
    filestr=[filestr,'complex ',inoutother{2}{i},'(size(',vararginout{i}{1}{1},',',temp5{i}{1},'),size(',vararginout{i}{1}{2},',',temp5{i}{2},'))',r];
   end
  elseif size(getfield(cw,inoutother{2}{i}),1)>1
   if isreal(getfield(cw,inoutother{2}{i}))
    filestr=[filestr,'real ',inoutother{2}{i},'(size(',vararginout{i}{1}{1},',',temp5{i}{1},'),1)',r];
   else
    filestr=[filestr,'complex ',inoutother{2}{i},'(size(',vararginout{i}{1}{1},',',temp5{i}{1},'),1)',r];
   end
  elseif size(getfield(cw,inoutother{2}{i}),2)>1
   if isreal(getfield(cw,inoutother{2}{i}))
    filestr=[filestr,'real ',inoutother{2}{i},'(1,size(',vararginout{i}{1}{2},',',temp5{i}{2},'))',r];
   else
    filestr=[filestr,'complex ',inoutother{2}{i},'(1,size(',vararginout{i}{1}{2},',',temp5{i}{2},'))',r];
   end
  end
 else
  if isreal(getfield(cw,inoutother{2}{i}))
   filestr=[filestr,'real ',inoutother{2}{i},r];
  else
   filestr=[filestr,'complex ',inoutother{2}{i},r];
  end
 end
end
filestr=[filestr,'!     All other local variables',r];
% Create local mirrors
filestr=[filestr,'!     Input/Output local mirrors',r];
filestr=[filestr,'!     Fill in vars going in and out',r];
filestr=[filestr,r];
filestr=[filestr,'! --- Main computational routine. ---------------------------------!',r];
filestr=[filestr,r];
filestr=[filestr,'return',r];
if suborfun==0
 filestr=[filestr,'end subroutine ',filename,r];
else
 filestr=[filestr,'end function ',filename,r];
end
filestr=[filestr,'!---------------------------------------------------------------------',r];
fprintf(fid,'%s',filestr);
fclose(fid);
