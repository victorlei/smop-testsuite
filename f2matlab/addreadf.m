if ~isempty(regexp(filestr,'readf'))
 fid=fopen(fullfile(fileparts(which('f2matlab')),'readf.m'));
 temp1=fscanf(fid,'%c'); fclose(fid);
 filestr=[filestr,r,r,r,temp1];
end % if ~isempty(regexp(filestr,