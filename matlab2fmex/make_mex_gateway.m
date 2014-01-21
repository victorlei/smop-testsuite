function filestr=make_mex_gateway(filename,inoutother,cw,vararginout,localvartype,want_fb,want_kb,alpha)
% This function will write filename to contain the appropriate mex gateway.
%   filestr=make_mex_gateway(filename,inoutother,cw,vararginout,localvartype,varargin)
%
%   filename should be without the .f extension
innum=length(inoutother{1});
outnum=length(inoutother{2});
othernum=length(inoutother{3});
fid=fopen([filename,'.f90'],'w');
r=char(10);
inttype='(4)';
% Header comes first
filestr='';
filestr=[filestr,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',r];
filestr=[filestr,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Gateway!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',r];
filestr=[filestr,'subroutine mexfunction(nlhs, plhs, nrhs, prhs)',r];
if ~alpha
 filestr=[filestr,'integer',inttype,' plhs(*), prhs(*)',r];
else
 filestr=[filestr,'integer*8 plhs(*), prhs(*)',r];
end
filestr=[filestr,'integer',inttype,' nlhs, nrhs      ',r,...
	 '! input and output pointers',r];
% Now declare all the pointers
if ~alpha
 filestr=[filestr,'integer',inttype,' :: '];
else
 filestr=[filestr,'integer*8 :: '];
end
countimag=0;imags=[];reals=[];
for i=1:innum
 if i~=innum
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,inoutother{1}{i},'_ptr,'];
   reals=[reals i];
  else
   filestr=[filestr,inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i, '];
   countimag=countimag+1;imags=[imags i];
  end
 else
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,inoutother{1}{i},'_ptr',r];
   reals=[reals i];
  else
   filestr=[filestr,inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i',r];
   countimag=countimag+1;imags=[imags i];
  end
 end
end
if ~isempty(reals)
 filestr=[filestr,'real, allocatable :: '];
 for i=reals
  if i~=reals(end)
   filestr=[filestr,inoutother{1}{i},'(:,:),'];
  else
   filestr=[filestr,inoutother{1}{i},'(:,:)',r];
  end
 end
end
if countimag>0
 filestr=[filestr,'complex, allocatable :: '];
 for i=imags
  if i~=imags(end)
   filestr=[filestr,inoutother{1}{i},'(:,:),'];
  else
   filestr=[filestr,inoutother{1}{i},'(:,:)',r];
  end
 end
end
%Let's assign a vararginout for those outputs who have no association
for i=(length(vararginout)+1):outnum
 temp3=[0 0];
 for j=1:length(inoutother{1})
  if ((size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1))&(size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)))
   temp3(1)=j;temp3(2)=j;temp5{i}{1}='_m';temp5{i}{2}='_n';temp5{i}{3}=',1';temp5{i}{4}=',2';
   if want_fb|want_kb
    if i<11
     disp(['  Setting the size of output var ',inoutother{2}{i},' equal to the input var ',inoutother{1}{j},'.']);
    end
   end
   break
  end
  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1)
   temp3(1)=j;temp5{i}{1}='_m';
  end
  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),2)
   temp3(1)=j;temp5{i}{1}='_n';
  end
  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),1)
   temp3(2)=j;temp5{i}{2}='_m';
  end
  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)
   temp3(2)=j;temp5{i}{2}='_n';
  end
 end
 if all(temp3)
  vararginout{i}{1}{1}=inoutother{1}{temp3(1)};
  if ~isreal(getfield(cw,vararginout{i}{1}{1}))
   temp4{i}{1}='_r';else,temp4{i}{1}='';
  end
  vararginout{i}{1}{2}=inoutother{1}{temp3(2)};
  if ~isreal(getfield(cw,vararginout{i}{1}{2}))
   temp4{i}{2}='_r';else,temp4{i}{2}='';
  end
  vararginout{i}{2}=~isreal(getfield(cw,inoutother{2}{i}));
 else
  error(['output variable ',inoutother{2}{i},'''s size does not match any input variable.',r,'Make an input variable the same size as ',inoutother{2}{i},' (in this case ',num2str(size(getfield(cw,inoutother{2}{i}),1)),',',num2str(size(getfield(cw,inoutother{2}{i}),2)),').']);
 end
end
%Now for the outputs
if ~alpha
 filestr=[filestr,'integer',inttype,' :: '];
else
 filestr=[filestr,'integer*8 :: '];
end
countimag=0;imags=[];reals=[];
for i=1:outnum
 if i~=outnum
  if vararginout{i}{2}==0
   filestr=[filestr,inoutother{2}{i},'_ptr,'];
   reals=[reals i];
  else
   filestr=[filestr,inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i,'];
   countimag=countimag+1;imags=[imags i];
  end
 else
  if vararginout{i}{2}==0
   filestr=[filestr,inoutother{2}{i},'_ptr',r];
   reals=[reals i];
  else
   filestr=[filestr,inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i',r];
   countimag=countimag+1;imags=[imags i];
  end
 end
end
if ~isempty(reals)
 filestr=[filestr,'real, allocatable :: '];
 for i=reals
  if i~=reals(end)
   filestr=[filestr,inoutother{2}{i},'(:,:),'];
  else
   filestr=[filestr,inoutother{2}{i},'(:,:)',r];
  end
 end
end
if countimag>0
 filestr=[filestr,'complex, allocatable :: '];
 for i=imags
  if i~=imags(end)
   filestr=[filestr,inoutother{2}{i},'(:,:),'];
  else
   filestr=[filestr,inoutother{2}{i},'(:,:)',r];
  end
 end
end
% Now let's pass the sizes of all the input vars.
filestr=[filestr,'!     Any other variables needed',r];
filestr=[filestr,'integer '];
for i=1:innum
 if i~=innum
  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n,'];
 else
  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n',r];
 end
end
% Check for the proper number of inputs and outputs.
%filestr=[filestr,'write(*,*)',r];
filestr=[filestr,...
	 '!     CHECK FOR PROPER NUMBER OF ARGUMENTS',r,...
	 'if (nrhs .ne. ',num2str(innum),') then',r,...
	 '  print *,''',filename,' requires ',num2str(innum),' input arguments',char(39),';return',r,...
	 'elseif (nlhs .ne. ',num2str(outnum),') then',r,...
	 '  print *,''',filename,' requires ',num2str(outnum),' output arguments',char(39),';return',r,...
	 'endif',r];
% mexerrmsgtxt is broken as of now with ifc
%%%filestr=[filestr,...
%%%	 '!     CHECK FOR PROPER NUMBER OF ARGUMENTS',r,...
%%%	 'if (nrhs .ne. ',num2str(innum),') then',r,...
%%%	 '  call mexerrmsgtxt(''',filename,' requires ',num2str(innum),' input arguments',char(39),')',r,...
%%%	 'elseif (nlhs .ne. ',num2str(outnum),') then',r,...
%%%	 '  call mexerrmsgtxt(''',filename,' requires ',num2str(outnum),' output arguments',char(39),')',r,...
%%%	 'endif',r];
% OK, let's get the sizes of all the inputs
%filestr=[filestr,'!---------------------------------------------------------------------',r];
filestr=[filestr,'!     Get the sizes of all the input variables',r];
count=1;
for i=1:innum
 filestr=[filestr,'',inoutother{1}{i},'_m=mxGetm(prhs(',num2str(count),'));',inoutother{1}{i},'_n=mxGetn(prhs(',num2str(count),'))',r];
 count=count+1;
end
% Return argumnets, allocation, and sizing.
filestr=[filestr,'!     Create matrices for the return argument',r];
count=1;
for i=1:outnum
 if vararginout{i}{2}==0
  filestr=[filestr,'plhs(',num2str(count),')=mxCreateDoubleMatrix(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},',0)',r];
 else
  filestr=[filestr,'plhs(',num2str(count),')=mxCreateDoubleMatrix(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},',1)',r];
  end
 count=count+1;
end
count=1;
for i=1:outnum
 if vararginout{i}{2}==0
  filestr=[filestr,'',inoutother{2}{i},'_ptr=mxGetPr(plhs(',num2str(count),'))',r];
 else
  filestr=[filestr,'',inoutother{2}{i},'_ptr_r=mxGetPr(plhs(',num2str(count),'));'];
  filestr=[filestr,'',inoutother{2}{i},'_ptr_i=mxGetPi(plhs(',num2str(count),'))',r];
  end
 count=count+1;
end
% Right hand side arguments
filestr=[filestr,'!     Copy right hand arguments to local arrays',r];
count=1;
for i=1:innum
 if isreal(getfield(cw,inoutother{1}{i}))
  filestr=[filestr,'',inoutother{1}{i},'_ptr=mxGetPr(prhs(',num2str(count),'))',r];
 else
  filestr=[filestr,'',inoutother{1}{i},'_ptr_r=mxGetPr(prhs(',num2str(count),'));'];
  filestr=[filestr,'',inoutother{1}{i},'_ptr_i=mxGetPi(prhs(',num2str(count),'))',r];
 end
 count=count+1;  
end
% Allocate arrays
filestr=[filestr,'!     Allocate and copy data to arrays',r];
filestr=[filestr,'allocate('];
for i=1:innum
 if i~=innum
  filestr=[filestr,inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n),'];
 else
  filestr=[filestr,inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n))',r];
 end
end
filestr=[filestr,'allocate('];
for i=1:outnum
 if i~=outnum
  filestr=[filestr,inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},'),'];
 else
  filestr=[filestr,inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},'))',r];
  end
end
for i=1:innum
 if isreal(getfield(cw,inoutother{1}{i}))
  filestr=[filestr,'call mxCopyPtrToReal8(',inoutother{1}{i},'_ptr,',inoutother{1}{i},',',inoutother{1}{i},'_m*',inoutother{1}{i},'_n)',r];
 else
  filestr=[filestr,'call mxCopyPtrToComplex16(',inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i,',inoutother{1}{i},',',inoutother{1}{i},'_m*',inoutother{1}{i},'_n)',r];
 end
end
% Call the actual computational subroutine
filestr=[filestr,'!     Do the actual computations in a subroutine',r];
filestr=[filestr,'call ',filename,'('];
for i=1:outnum
 filestr=[filestr,inoutother{2}{i},','];
end
for i=1:innum
 if i~=innum
  filestr=[filestr,inoutother{1}{i},','];
 else
  filestr=[filestr,inoutother{1}{i},')',r];
 end
end
% End up the gateway
for i=1:outnum
 if vararginout{i}{2}==0
  filestr=[filestr,'call mxCopyReal8ToPtr(',inoutother{2}{i},',',inoutother{2}{i},'_ptr,',vararginout{i}{1}{1},temp5{i}{1},'*',vararginout{i}{1}{2},temp5{i}{2},')',r];
 else
  filestr=[filestr,'call mxCopyComplex16ToPtr(',inoutother{2}{i},',',inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i,',vararginout{i}{1}{1},temp5{i}{1},'*',vararginout{i}{1}{2},temp5{i}{2},')',r];
  end
end
% Deallocate complex array mirrors
filestr=[filestr,'deallocate('];
for i=1:outnum
 filestr=[filestr,inoutother{2}{i},','];
end
for i=1:innum
 if i~=innum
  filestr=[filestr,inoutother{1}{i},','];
 else
  filestr=[filestr,inoutother{1}{i},')',r];
 end
end
filestr=[filestr,'return',r];
filestr=[filestr,r,'contains',r];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now move on to the computational routine.
filestr=[filestr,r,r];
filestr=[filestr,'subroutine ',filename,'('];
for i=1:outnum
 filestr=[filestr,inoutother{2}{i},','];
end
for i=1:innum
 if i~=innum
  filestr=[filestr,inoutother{1}{i},','];
 else
  filestr=[filestr,inoutother{1}{i},')',r];
 end
end
filestr=[filestr,'!     COMPUTATIONAL SUBROUTINE',r];
% Create calling variables
filestr=[filestr,'!     size variables',r];
% Create local mirrors
filestr=[filestr,'!     Input/Output local mirrors',r];
for i=1:length(inoutother{1})
 if prod(size(getfield(cw,inoutother{1}{i})))==1
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,'real ',inoutother{1}{i},'',r];
  else
   filestr=[filestr,'complex ',inoutother{1}{i},'',r];
  end
 else
  if isreal(getfield(cw,inoutother{1}{i}))
   filestr=[filestr,'real ',inoutother{1}{i},'(:,:)',r];
  else
   filestr=[filestr,'complex ',inoutother{1}{i},'(:,:)',r];
  end
 end
end
for i=1:length(inoutother{2})
 if isreal(getfield(cw,inoutother{2}{i}))
  filestr=[filestr,'real ',inoutother{2}{i},'(:,:)',r];
 else
  filestr=[filestr,'complex ',inoutother{2}{i},'(:,:)',r];
 end
end
filestr=[filestr,'!     All other local variables',r];
filestr=[filestr,'!     Fill in vars going in and out',r];
filestr=[filestr,'! --- Main computational routine. ---------------------------------!',r];
filestr=[filestr,r];
filestr=[filestr,'return',r];
filestr=[filestr,'end subroutine ',filename,r];
%filestr=[filestr,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',r];
filestr=[filestr,r,'end subroutine mexfunction',r];
%filestr=[filestr,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',r];
%filestr=[filestr,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',r];
fprintf(fid,'%s',filestr);
fclose(fid);
























%%%% This function will write filename to contain the appropriate mex gateway.
%%%%   filestr=make_mex_gateway(filename,inoutother,cw,vararginout,localvartype,varargin)
%%%%
%%%%   filename should be without the .f extension
%%%innum=length(inoutother{1});
%%%outnum=length(inoutother{2});
%%%othernum=length(inoutother{3});
%%%fid=fopen([filename,'.f90'],'w');
%%%r=char(10);
%%%% Header comes first
%%%filestr=['subroutine mexfunction(nlhs, plhs, nrhs, prhs)',r,...
%%%	 '!--------------------------------------------------------------------',r];
%%%if ~alpha
%%% filestr=[filestr,'integer plhs(*), prhs(*)',r];
%%%else
%%% filestr=[filestr,'integer*8 plhs(*), prhs(*)',r];
%%%end
%%%filestr=[filestr,'integer nlhs, nrhs      ',r,...
%%%	 '!--------------------------------------------------------------------',r,...
%%%	 '!     Create pointers for calling the computational subroutine.',r];
%%%% Now declare all the pointers
%%%filestr=[filestr,'!     inputs',r];
%%%if ~alpha
%%% filestr=[filestr,'integer :: '];
%%%else
%%% filestr=[filestr,'integer*8 :: '];
%%%end
%%%countimag=0;imags=[];reals=[];
%%%for i=1:innum
%%% if i~=innum
%%%  if isreal(getfield(cw,inoutother{1}{i}))
%%%   filestr=[filestr,inoutother{1}{i},'_ptr,'];
%%%   reals=[reals i];
%%%  else
%%%   filestr=[filestr,inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i, '];
%%%   countimag=countimag+1;imags=[imags i];
%%%  end
%%% else
%%%  if isreal(getfield(cw,inoutother{1}{i}))
%%%   filestr=[filestr,inoutother{1}{i},'_ptr',r];
%%%   reals=[reals i];
%%%  else
%%%   filestr=[filestr,inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i',r];
%%%   countimag=countimag+1;imags=[imags i];
%%%  end
%%% end
%%%end
%%%if ~isempty(reals)
%%% filestr=[filestr,'real, allocatable :: '];
%%% for i=reals
%%%  if i~=reals(end)
%%%   filestr=[filestr,inoutother{1}{i},'(:,:),'];
%%%  else
%%%   filestr=[filestr,inoutother{1}{i},'(:,:)',r];
%%%  end
%%% end
%%%end
%%%if countimag>0
%%% filestr=[filestr,'complex, allocatable :: '];
%%% for i=imags
%%%  if i~=imags(end)
%%%   filestr=[filestr,inoutother{1}{i},'(:,:),'];
%%%  else
%%%   filestr=[filestr,inoutother{1}{i},'(:,:)',r];
%%%  end
%%% end
%%%end
%%%%Let's assign a vararginout for those outputs who have no association
%%%for i=(length(vararginout)+1):outnum
%%% temp3=[0 0];
%%% for j=1:length(inoutother{1})
%%%  if ((size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1))&(size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)))
%%%   temp3(1)=j;temp3(2)=j;temp5{i}{1}='_m';temp5{i}{2}='_n';
%%%   if want_fb|want_kb
%%%    if i<11
%%%     disp(['  Setting the size of output var ',inoutother{2}{i},' equal to the input var ',inoutother{1}{j},'.']);
%%%    end
%%%   end
%%%   break
%%%  end
%%%  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),1)
%%%   temp3(1)=j;temp5{i}{1}='_m';
%%%  end
%%%  if size(getfield(cw,inoutother{2}{i}),1)==size(getfield(cw,inoutother{1}{j}),2)
%%%   temp3(1)=j;temp5{i}{1}='_n';
%%%  end
%%%  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),1)
%%%   temp3(2)=j;temp5{i}{2}='_m';
%%%  end
%%%  if size(getfield(cw,inoutother{2}{i}),2)==size(getfield(cw,inoutother{1}{j}),2)
%%%   temp3(2)=j;temp5{i}{2}='_n';
%%%  end
%%% end
%%% if all(temp3)
%%%  vararginout{i}{1}{1}=inoutother{1}{temp3(1)};
%%%  if ~isreal(getfield(cw,vararginout{i}{1}{1}))
%%%   temp4{i}{1}='_r';else,temp4{i}{1}='';
%%%  end
%%%  vararginout{i}{1}{2}=inoutother{1}{temp3(2)};
%%%  if ~isreal(getfield(cw,vararginout{i}{1}{2}))
%%%   temp4{i}{2}='_r';else,temp4{i}{2}='';
%%%  end
%%%  vararginout{i}{2}=~isreal(getfield(cw,inoutother{2}{i}));
%%% else
%%%  error(['output variable ',inoutother{2}{i},'''s size does not match any input variable.',r,'Make an input variable the same size as ',inoutother{2}{i},' (in this case ',num2str(size(getfield(cw,inoutother{2}{i}),1)),',',num2str(size(getfield(cw,inoutother{2}{i}),2)),').']);
%%% end
%%%end
%%%%Now for the outputs
%%%filestr=[filestr,'!     outputs',r];
%%%if ~alpha
%%% filestr=[filestr,'integer :: '];
%%%else
%%% filestr=[filestr,'integer*8 :: '];
%%%end
%%%countimag=0;imags=[];reals=[];
%%%for i=1:outnum
%%% if i~=outnum
%%%  if vararginout{i}{2}==0
%%%   filestr=[filestr,inoutother{2}{i},'_ptr,'];
%%%   reals=[reals i];
%%%  else
%%%   filestr=[filestr,inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i,'];
%%%   countimag=countimag+1;imags=[imags i];
%%%  end
%%% else
%%%  if vararginout{i}{2}==0
%%%   filestr=[filestr,inoutother{2}{i},'_ptr',r];
%%%   reals=[reals i];
%%%  else
%%%   filestr=[filestr,inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i',r];
%%%   countimag=countimag+1;imags=[imags i];
%%%  end
%%% end
%%%end
%%%if ~isempty(reals)
%%% filestr=[filestr,'real, allocatable :: '];
%%% for i=reals
%%%  if i~=reals(end)
%%%   filestr=[filestr,inoutother{2}{i},'(:,:),'];
%%%  else
%%%   filestr=[filestr,inoutother{2}{i},'(:,:)',r];
%%%  end
%%% end
%%%end
%%%if countimag>0
%%% filestr=[filestr,'complex, allocatable :: '];
%%% for i=imags
%%%  if i~=imags(end)
%%%   filestr=[filestr,inoutother{2}{i},'(:,:),'];
%%%  else
%%%   filestr=[filestr,inoutother{2}{i},'(:,:)',r];
%%%  end
%%% end
%%%end
%%%% Now let's pass the sizes of all the input vars.
%%%filestr=[filestr,'!     Any other variables needed',r];
%%%filestr=[filestr,'integer '];
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n,'];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n',r];
%%% end
%%%end
%%%% Check for the proper number of inputs and outputs.
%%%filestr=[filestr,...
%%%	 '!---------------------------------------------------------------------',r,...
%%%	 '!     CHECK FOR PROPER NUMBER OF ARGUMENTS',r,...
%%%	 'if (nrhs .ne. ',num2str(innum),') then',r,...
%%%	 '  call mexerrmsgtxt(''',filename,' requires ',num2str(innum),' input arguments',char(39),')',r,...
%%%	 'elseif (nlhs .ne. ',num2str(outnum),') then',r,...
%%%	 '  call mexerrmsgtxt(''',filename,' requires ',num2str(outnum),' output arguments',char(39),')',r,...
%%%	 'endif',r];
%%%% OK, let's get the sizes of all the inputs
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%filestr=[filestr,'!     Get the sizes of all the input variables',r];
%%%count=1;
%%%for i=1:innum
%%% filestr=[filestr,'',inoutother{1}{i},'_m=mxGetm(prhs(',num2str(count),'));',inoutother{1}{i},'_n=mxGetn(prhs(',num2str(count),'))',r];
%%% count=count+1;
%%%end
%%%% Return argumnets, allocation, and sizing.
%%%filestr=[filestr,'!     Create matrices for the return argument',r];
%%%count=1;
%%%for i=1:outnum
%%% if vararginout{i}{2}==0
%%%  filestr=[filestr,'plhs(',num2str(count),')=mxCreateFull(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},',0)',r];
%%% else
%%%  filestr=[filestr,'plhs(',num2str(count),')=mxCreateFull(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},',1)',r];
%%%  end
%%% count=count+1;
%%%end
%%%count=1;
%%%for i=1:outnum
%%% if vararginout{i}{2}==0
%%%  filestr=[filestr,'',inoutother{2}{i},'_ptr=mxGetPr(plhs(',num2str(count),'))',r];
%%% else
%%%  filestr=[filestr,'',inoutother{2}{i},'_ptr_r=mxGetPr(plhs(',num2str(count),'));'];
%%%  filestr=[filestr,'',inoutother{2}{i},'_ptr_i=mxGetPi(plhs(',num2str(count),'))',r];
%%%  end
%%% count=count+1;
%%%end
%%%% Right hand side arguments
%%%filestr=[filestr,'!     Copy right hand arguments to local arrays',r];
%%%count=1;
%%%for i=1:innum
%%% if isreal(getfield(cw,inoutother{1}{i}))
%%%  filestr=[filestr,'',inoutother{1}{i},'_ptr=mxGetPr(prhs(',num2str(count),'))',r];
%%% else
%%%  filestr=[filestr,'',inoutother{1}{i},'_ptr_r=mxGetPr(prhs(',num2str(count),'));'];
%%%  filestr=[filestr,'',inoutother{1}{i},'_ptr_i=mxGetPi(prhs(',num2str(count),'))',r];
%%% end
%%% count=count+1;  
%%%end
%%%% Allocate arrays
%%%filestr=[filestr,'!     Allocate and copy data to arrays',r];
%%%filestr=[filestr,'allocate('];
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n),'];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n))',r];
%%% end
%%%end
%%%filestr=[filestr,'allocate('];
%%%for i=1:outnum
%%% if i~=outnum
%%%  filestr=[filestr,inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},'),'];
%%% else
%%%  filestr=[filestr,inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},'))',r];
%%%  end
%%%end
%%%for i=1:innum
%%% if isreal(getfield(cw,inoutother{1}{i}))
%%%  filestr=[filestr,'call mxCopyPtrToReal8(',inoutother{1}{i},'_ptr,',inoutother{1}{i},',',inoutother{1}{i},'_m*',inoutother{1}{i},'_n)',r];
%%% else
%%%  filestr=[filestr,'call mxCopyPtrToComplex16(',inoutother{1}{i},'_ptr_r,',inoutother{1}{i},'_ptr_i,',inoutother{1}{i},',',inoutother{1}{i},'_m*',inoutother{1}{i},'_n)',r];
%%% end
%%%end
%%%% Call the actual computational subroutine
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%filestr=[filestr,'!     Do the actual computations in a subroutine',r];
%%%filestr=[filestr,'call ',filename,'('];
%%%for i=1:outnum
%%% filestr=[filestr,inoutother{2}{i},','];
%%%end
%%%for i=1:innum
%%% filestr=[filestr,inoutother{1}{i},','];
%%%end
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n,'];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n)',r];
%%% end
%%%end
%%%% End up the gateway
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%for i=1:outnum
%%% if vararginout{i}{2}==0
%%%  filestr=[filestr,'call mxCopyReal8ToPtr(',inoutother{2}{i},',',inoutother{2}{i},'_ptr,',vararginout{i}{1}{1},temp5{i}{1},'*',vararginout{i}{1}{2},temp5{i}{2},')',r];
%%% else
%%%  filestr=[filestr,'call mxCopyComplex16ToPtr(',inoutother{2}{i},',',inoutother{2}{i},'_ptr_r,',inoutother{2}{i},'_ptr_i,',vararginout{i}{1}{1},temp5{i}{1},'*',vararginout{i}{1}{2},temp5{i}{2},')',r];
%%%  end
%%%end
%%%
%%%% Deallocate complex array mirrors
%%%filestr=[filestr,'deallocate('];
%%%for i=1:outnum
%%% filestr=[filestr,inoutother{2}{i},','];
%%%end
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},','];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},')',r];
%%% end
%%%end
%%%filestr=[filestr,'return',r];
%%%filestr=[filestr,'end subroutine mexfunction',r];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Now move on to the computational routine.
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%filestr=[filestr,r];
%%%filestr=[filestr,r];
%%%filestr=[filestr,'!     COMPUTATIONAL SUBROUTINE',r];
%%%filestr=[filestr,'subroutine ',filename,'('];
%%%for i=1:outnum
%%% filestr=[filestr,inoutother{2}{i},','];
%%%end
%%%for i=1:innum
%%% filestr=[filestr,inoutother{1}{i},','];
%%%end
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n,'];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n)',r];
%%% end
%%%end
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%% Create calling variables
%%%filestr=[filestr,'!     size variables',r];
%%%filestr=[filestr,'integer '];
%%%for i=1:innum
%%% if i~=innum
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n,'];
%%% else
%%%  filestr=[filestr,inoutother{1}{i},'_m,',inoutother{1}{i},'_n',r];
%%% end
%%%end
%%%%filestr=[filestr,'!     First create all the calling variables. ',r];
%%%%filestr=[filestr,'!     REM*** The changes are returned to the caller.',r];
%%%filestr=[filestr,'!     All other local variables',r];
%%%% Create local mirrors
%%%filestr=[filestr,'!     Input/Output local mirrors',r];
%%%for i=1:length(inoutother{1})
%%% if prod(size(getfield(cw,inoutother{1}{i})))==1
%%%  if isreal(getfield(cw,inoutother{1}{i}))
%%%   filestr=[filestr,'real ',inoutother{1}{i},'',r];
%%%  else
%%%   filestr=[filestr,'complex ',inoutother{1}{i},'',r];
%%%  end
%%% else
%%%  if isreal(getfield(cw,inoutother{1}{i}))
%%%   filestr=[filestr,'real ',inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n)',r];
%%%  else
%%%   filestr=[filestr,'complex ',inoutother{1}{i},'(',inoutother{1}{i},'_m,',inoutother{1}{i},'_n)',r];
%%%  end
%%% end
%%%end
%%%for i=1:length(inoutother{2})
%%% if isreal(getfield(cw,inoutother{2}{i}))
%%%  filestr=[filestr,'real ',inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},')',r];
%%% else
%%%  filestr=[filestr,'complex ',inoutother{2}{i},'(',vararginout{i}{1}{1},temp5{i}{1},',',vararginout{i}{1}{2},temp5{i}{2},')',r];
%%% end
%%%end
%%%filestr=[filestr,'!     Fill in vars going in and out',r];
%%%filestr=[filestr,'! --- Main computational routine. --------------------------------------',r];
%%%filestr=[filestr,r];
%%%filestr=[filestr,'return',r];
%%%filestr=[filestr,'end subroutine ',filename,r];
%%%filestr=[filestr,'!---------------------------------------------------------------------',r];
%%%fprintf(fid,'%s',filestr);
%%%fclose(fid);
