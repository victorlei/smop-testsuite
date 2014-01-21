clall

dd=builtin('dir');
ext='f90';
r=char(10);
funstr='';

for ii=1:length(dd)
 t1=findstr(dd(ii).name,ext);
 if ~isempty(t1)
  disp(dd(ii).name)
  fid=fopen(dd(ii).name); filestr=fscanf(fid,'%c'); fclose(fid);
  rets=findstr(r,filestr);
  rets=[0 rets];
  filestr=strrep(filestr,'GOTO 99999','RETURN');
  filestr=strrep(filestr,'99999 END SUBROUTINE','END SUBROUTINE');
  filestr=strrep(filestr,'99999 END FUNCTION','END FUNCTION');
  if strcmp(dd(ii).name,'d1mach.f90') | strcmp(dd(ii).name,'r1mach.f90')
   filestr=strrep(filestr,'DATA iflag/0/','DATA iflag/1/');
  end
  fid=fopen(dd(ii).name,'w');  fprintf(fid,'%c',filestr);   fclose(fid);
 end
end


unix(['rm *.smb'])