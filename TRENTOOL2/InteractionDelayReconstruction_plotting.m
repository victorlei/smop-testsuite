% function PlotTE
function []=InteractionDelayReconstruction_plotting(cfg)

% InteractionDelayReconstruction_plotting
% provides a detailed plot of TE and (1-p) versus perdictiontime u
% created from the intermediate results of InteractionDelayReconstruction_calculate
% which are stored in files with names that follow patterns like
% 'ORIGINALDATASETNAME_FILEIDOUT_u_*_TIMEINFO_TEpermtest_output.mat'
% (when you check the directory where your output is stored look for the
% files that end in 'TEpermtest_output.mat' it shoud be pretty clear how to set up the pattern)
%
% INPUTS:
%
%   cfg                   configuration structure  with fields
%
%   cfg.pattern           the Filename pattern with MATLAB compatible 
%                         wildcards (such as ?,*)
%
%   cfg.directory         The directory where the files conatining the
%                         results of TEpermtest are stored
%   cfg.graphics    =     'pp_ready' or 'view'
%                         The configuration for paper ready/ testview,
%                         default 'view'
%   cfg.scaletype   =     'log' or 'lin' - the predictiontime u scale
%                          (default 'log')
%
% 2012 Michael Wibral based on code by Viola Priesemann
% changelog:
% 2012.02.27 NP: plot for multiple channel combination;
%
% 2012.07.05 NP: adding scaletype for ploting

%set defaluts
if  ~isfield(cfg,'directory')
    cfg.directory=pwd;
end;

if ~isfield(cfg,'scaletype')
    cfg.scaletype = 'log';
end;

if ~strcmp(cfg.directory(end),'/')
    cfg.directory=strcat(cfg.directory,'/')
end;

if ~isfield(cfg, 'graphics' )
    cfg.graphics = 'view';
end;

ddir = dir(strcat(cfg.directory,cfg.pattern));

for k=1:length(ddir)
   load(ddir(k).name), u(k)=TEpermtest.cfg.predicttime_u; TE{k}=squeeze(TEpermtest.TEpermvalues); %squeeze not necessary in TRENTOOL2
end

[u,idx] = sort(u,'ascend');

sgs = TEpermtest.sgncmb;
ch_nr = length(sgs);j = 0;
for k=1:length(sgs), ii=1;
    for ku = idx
        pval(k,ii)  = TE{ku}(1*ch_nr-(ch_nr-1)+j); % was (k,1)
        TEval(k,ii) = TE{ku}(4*ch_nr-(ch_nr-1)+j); % ...
        TEsig(k,ii) = TE{ku}(2*ch_nr-(ch_nr-1)+j);
        TEsig2(k,ii)= TE{ku}(3*ch_nr-(ch_nr-1)+j);        
        TEvolC(k,ii)= TE{ku}(5*ch_nr-(ch_nr-1)+j); 
        ii=ii+1;
    end
    j = j+1;
end

if strcmp(cfg.graphics,'view')
    
    cmap = jet(7);
    cc=3; dy = 1.3; df=0;
    % for k=1:6, figure(k+df), hold off, end

    for k=1:length(sgs)
        figure(k+df), %  hold off
        if strcmp(cfg.scaletype,'log') 
            semilogx(u,1-pval(k,:),'--','Color',cmap(cc,:)), hold all
        elseif strcmp(cfg.scaletype,'lin')
            plot(u,1-pval(k,:),'--','Color',cmap(cc,:)), hold all
        else error('TRENTOOL error: chose a correct plot scaletype');
        end
        
        plot(u,TEval(k,:)/max(abs(TEval(k,:))),'.-','Color',cmap(cc,:))
        plot(u,ones(length(u),1)*dy,'.','Color',[0.4,0.4,0.4])
        plot(u,1./TEsig(k,:)*dy,'*k')
        plot(u,1./TEsig2(k,:)*dy,'*g')
        plot(u,1./TEvolC(k,:)*dy,'*r')
    %    title (['signal comb ', sgs(k,:), 'maxTE ', num2str(max(TEval(k,:)))])
        h = legend('1-p','TE/max(TE)','calculated','sign','signB','Volume Conduct');
        set(h,'box','off','Color', 'none','Location','SouthEast')
        xlabel('pred time u in ms')
end
elseif strcmp(cfg.graphics,'pp_ready')

    cc=3; dy = 1.3; df=0;
    for k=1:length(sgs)
        figure(k+df), %  hold off
        if strcmp(cfg.scaletype,'log') 
            semilogx(u,TEval(k,:)/max(abs(TEval(k,:))),'o-','Color','k','LineWidth',2,'MarkerSize',6), hold all
        elseif strcmp(cfg.scaletype,'lin')
            plot(u,TEval(k,:)/max(abs(TEval(k,:))),'o-','Color','k','LineWidth',2,'MarkerSize',6), hold all
        else error('TRENTOOL error: chose a correct plot scaletype');
        end
       
        plot(u,ones(length(u),1)*dy,'.','Color','k','MarkerSize',4)
        plot(u,1./TEsig(k,:)*dy,'*k','MarkerSize',5.5)
        plot(u,1./TEsig2(k,:)*dy,'dk','MarkerSize',6.5)
        plot(u,1./TEvolC(k,:)*dy,'xk','MarkerSize',5.5)
        
%    title (['signal comb ', sgs(k,:), 'maxTE ', num2str(max(TEval(k,:)))])
        %h = legend('1-p','TE/max(TE)','calculated','sign','signB','Volume Conduct');
        %set(h,'box','off','Color', 'none','Location','SouthEast')
        ylabel('TE/max(TE)')
        xlabel('pred time u in ms')
        %grid(gca,'minor');
        %set(gca,'XGrid','on');
    end
else fprintf('no method %s is implemented\n',cfg.graphics);
end