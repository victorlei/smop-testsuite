function [dir_mex] = TEarch(cfg)

arch = computer('arch');
if strcmp(arch(1:3),'win')
    ps = '\';
    md = 'mexw';
else 
    ps = '/';
    md = 'mexa';
end
    
    
    
if strcmp(arch(end-1:end), '64')
    if strcmp(cfg.Path2TSTOOL(end), ps)
        dir_mex = strcat(cfg.Path2TSTOOL,'tstoolbox',ps,'mex',ps,md,'64');
    else
        dir_mex = strcat(cfg.Path2TSTOOL,ps,'tstoolbox',ps,'mex',ps,md,'64');
    end
else
% old code 
%     if strcmp(cfg.Path2TSTOOL(end), ps)
%         dir_mex = strcat(cfg.Path2TSTOOL,'tstoolbox',ps,'mex');
%     else
%         dir_mex = strcat(cfg.Path2TSTOOL,ps,'tstoolbox',ps,'mex');
%     end

% bugfix by Nicu pampu / Coneural 2011-08-23; inserted by MW
  if strcmp(cfg.Path2TSTOOL(end), ps)
        dir_mex = strcat(cfg.Path2TSTOOL,'tstoolbox',ps,'mex',ps,md,'32');
    else
        dir_mex = strcat(cfg.Path2TSTOOL,ps,'tstoolbox',ps,'mex',ps,md,'32');
    end
end