# Autogenerated with SMOP version 0.20
import numpy,sys
from copy import copy as _copy
from smop.runtime import *
numpy.random.seed(0)
def getDensity(N,E,**kwargs):
    if nargin()==1:
        E=sum(sum(N>0))
        N=size(N,1)
    dens=E/(N*(N-1))
    return dens
def TEbacktracking(solution_TEdyn,k,threshold,**kwargs):
    solution_tree=cell(2*threshold+1,2)
    count=0
    lower_limit=k-2*threshold
    if lower_limit<1:
        lower_limit=1
    visited=zeros(1,size(solution_TEdyn,2))
    visited[int(1-1)]=1
    for i in arange(lower_limit,k+1).reshape(1,-1).flat:
        s=m_array([i+1,size(solution_TEdyn,2)]).reshape(1,-1)
        if  not isempty(solution_TEdyn[s[int(1-1)]-1,s[int(2-1)]-1]):
            paths=TEbacktracking_rec(s,solution_TEdyn,[],m_array(),visited,0)
            if  not isempty(paths):
                for j in arange(1,length(paths)+1).reshape(1,-1).flat:
                    paths[j-1]=cat(2,paths[j-1],1)
                    paths[j-1]=fliplr(paths[j-1])
                count=count+1
                solution_tree[count-1,1-1]=paths
                solution_tree[count-1,2-1]=i
    solution_tree=solution_tree[1-1:count+1-1,:]
    if isempty(solution_tree):
        solution_tree=m_array()
    return solution_tree
def TEbacktracking_rec(source,solution,paths,current_path,visited,depth,**kwargs):
    visited=_copy(visited)
    if (source==m_array([1,1]).reshape(1,-1)):
        paths=cat(1,paths,current_path)
        return paths,current_path
    else:
        if (visited[int(source[int(2-1)]-1)]==1):
            return paths,current_path
        else:
            if length(paths)>20000:
                disp('Too many paths. Return.')
                return paths,current_path
    for i in arange(1,size(solution[source[int(1-1)]-1,source[int(2-1)]-1],2)+1).reshape(1,-1).flat:
        current_path=m_array([current_path,source[int(2-1)]]).reshape(1,-1)
        visited[int(source[int(2-1)]-1)]=1
        depth=depth+1
        new_source=m_array([solution[source[int(1-1)]-1,source[int(2-1)]-1][2-1,i-1]+1,solution[source[int(1-1)]-1,source[int(2-1)]-1][1-1,i-1]]).reshape(1,-1)
        paths,current_path=TEbacktracking_rec(new_source,solution,paths,current_path,visited,depth,nargout=2)
        depth=depth-1
        visited[int(source-1)]=0
        current_path[int(current_path.shape[0]-1)]=m_array()
    return paths,current_path
def TEdfs(adjacency_list,**kwargs):
    source=length(adjacency_list)
    target=1
    bool=TEdfs_rec(source,target,adjacency_list,zeros(1,length(adjacency_list)),0)
    bool_connected=logical(bool)
    return bool_connected
def TEdfs_rec(source,target,adjacency_list,visited,bool,**kwargs):
    visited=_copy(visited)
    if source==target:
        bool=1
        return bool
    else:
        for i in arange(1,size(adjacency_list[source-1],2)+1).reshape(1,-1).flat:
            if visited[int(adjacency_list[source-1][1-1,i-1]-1)]==0:
                new_source=adjacency_list[source-1][1-1,i-1]
                visited[int(new_source-1)]=1
                bool=TEdfs_rec(new_source,target,adjacency_list,visited,bool)
            if logical(bool):
                return bool
    return bool
def TEdyn(adjacency_list,k,**kwargs):
    n_vertices=size(adjacency_list,1)
    solution=cell(k+1,n_vertices)
    solution[1-1,1-1][int(1-1)]=0
    for i in arange(1,k+1).reshape(1,-1).flat:
        for v in arange(1,n_vertices+1).reshape(1,-1).flat:
            for e in arange(1,size(adjacency_list[v-1],2)+1).reshape(1,-1).flat:
                u=adjacency_list[v-1][1-1,e-1]
                w=round(adjacency_list[v-1][2-1,e-1])
                if (i-w)>=0:
                    try: 
                        if ( not isempty(solution[i-w+1-1,u-1])):
                            solution[i+1-1,v-1]=cat(2,solution[i+1-1,v-1],m_array([[u],[i-w]]).reshape(1,-1))
                    except: 
                        disp('error')
    return solution
def TEflagedges(data,altpaths,edges_orig,sgncmb_enum,**kwargs):
    data=_copy(data)
    N=size(altpaths,1)
    flag_list=m_array([cell2mat(altpaths[:,1-1:3+1-1]),2*ones(N,1)]).reshape(1,-1)
    for i in arange(1,N+1).reshape(1,-1).flat:
        new_edge_ind=edges_orig[altpaths[i-1,1-1]-1,:]
        flag_list[i-1,1-1]=find(sgncmb_enum[:,1-1]==new_edge_ind[int(1-1)]&sgncmb_enum[:,2-1]==new_edge_ind[int(2-1)])
        no_triangles=0
        for j in arange(1,length(altpaths[i-1,5-1][1-1])+1).reshape(1,-1).flat:
            if (length(altpaths[i-1,5-1][1-1][j-1])==3):
                no_triangles=no_triangles+1
                new_edge=m_array([altpaths[i-1,5-1][1-1][j-1][int(altpaths[i-1,5-1][1-1][j-1].shape[0]-1-1)],altpaths[i-1,5-1][1-1][j-1][int(altpaths[i-1,5-1][1-1][j-1].shape[0]-1)]]).reshape(1,-1)
                new_edge_ind=edges_orig[edges_orig[:,1-1]==new_edge[int(1-1)]&edges_orig[:,2-1]==new_edge[int(2-1)],:]
                new_edge_ind=find(sgncmb_enum[:,1-1]==new_edge_ind[int(1-1)]&sgncmb_enum[:,2-1]==new_edge_ind[int(2-1)])
                flag_list=cat(1,flag_list,m_array([new_edge_ind,new_edge,4]).reshape(1,-1))
                flag_list[i-1,4-1]=3
    disp(m_array([num2str(no_triangles),' triangle(s) were found by TEflagedges.']).reshape(1,-1))
    duplicates=m_array()
    for i in arange(1,size(flag_list,1)+1).reshape(1,-1).flat:
        ind=flag_list[i-1,1-1]==flag_list[i+1-1:flag_list.shape[0]+1-1,1-1]
        if (sum(ind)>0):
            ind=find(ind)+i
            duplicates=m_array([[duplicates],[ind]]).reshape(1,-1)
    flag_list[duplicates-1,:]=m_array()
    data.n_spuriousedges=size(flag_list,1)
    for i in arange(1,size(flag_list,1)+1).reshape(1,-1).flat:
        if isnan(data.TEpermvalues[flag_list[i-1,1-1]-1,4-1]):
            warning('This edge has already been flagged!')
        ind=flag_list[i-1,1-1]
        data.TEpermvalues[ind-1,:]=m_array([1,0,0,NaN(),flag_list[i-1,4-1],0]).reshape(1,-1)
    return data
def TEgraphanalysis(cfg,data,**kwargs):
    if isfield(cfg,'threshold'):
        threshold=cfg.threshold
    else:
        error('No threshold defined')
    if exist('ft_progress','file')==0:
        error('You have no current fieldtrip version in your path, that provides the function ft_progress. Please update to a version fieldtrip-201201xx or higher.')
    if sum(mod(data.TEpermvalues[:,6-1],1))>0:
        error('Delay times have to be integer values!')
    labels_vertices=unique(cat(1,data.sgncmb[:,1-1],data.sgncmb[:,2-1]))
    weights=data.TEpermvalues
    edges_temp=data.sgncmb[weights[:,2-1]==1,:]
    weights=weights[weights[:,2-1]==1,6-1]
    edges=zeros(size(edges_temp))
    sgncmb_enum=zeros(size(data.sgncmb))
    for i in arange(1,size(labels_vertices,1)+1).reshape(1,-1).flat:
        edges[strcmp(edges_temp[:,1-1],labels_vertices[int(i-1)])-1,1-1]=i
        edges[strcmp(edges_temp[:,2-1],labels_vertices[int(i-1)])-1,2-1]=i
        sgncmb_enum[strcmp(data.sgncmb[:,1-1],labels_vertices[int(i-1)])-1,1-1]=i
        sgncmb_enum[strcmp(data.sgncmb[:,2-1],labels_vertices[int(i-1)])-1,2-1]=i
    clear('edges_temp')
    n_vertices=length(labels_vertices)
    n_edges=length(edges)
    if n_vertices<3:
        disp('Graphanalysis does not work for graphs with less than 3 nodes! Return...')
        return data_paths(),cfg
    disp(m_array(['no of edges: ',num2str(n_edges),', no of vertices: ',num2str(n_vertices)]).reshape(1,-1))
    graphanalysis=m_array()
    graphanalysis.edges=n_edges
    graphanalysis.vertices=n_vertices
    graphanalysis.density=getDensity(n_edges,n_vertices)
    graphanalysis.threshold=cfg.threshold
    all_paths=[]
    data_paths=data
    ft_progress('init','text','Starting graph analysis...')
    no_nopath_TEdyn=0
    no_nopath_TEbacktracking=0
    for i in arange(1,n_edges+1).reshape(1,-1).flat:
        ft_progress(i/n_edges,'Processing edge %d of %d ...',i,n_edges)
        k=weights[int(i-1)]+threshold
        s=edges[i-1,1-1]
        t=edges[i-1,2-1]
        if k<=0:
            error('Something is wrong with your threshold!')
        edges_temp=_copy(edges)
        edges_temp[i-1,:]=m_array()
        weights_temp=_copy(weights)
        weights_temp[int(i-1)]=m_array()
        labels_vertices_temp=arange(1,n_vertices+1).reshape(1,-1)
        labels_vertices_temp[int(labels_vertices_temp==s)]=m_array()
        labels_vertices_temp[int(labels_vertices_temp==t)]=m_array()
        labels_vertices_temp=m_array([[s],[labels_vertices_temp.T],[t]]).reshape(1,-1)
        mask=ones(size(edges_temp))
        for j in arange(1,length(labels_vertices_temp)+1).reshape(1,-1).flat:
            mask_temp=edges_temp==labels_vertices_temp[int(j-1)]&mask
            edges_temp[int(edges_temp==labels_vertices_temp[int(j-1)]&mask)]=j
            mask[int(mask_temp-1)]=0
        clear('mask','mask_temp')
        adjacency_list=cell(n_vertices,1)
        for j in arange(1,n_vertices+1).reshape(1,-1).flat:
            adjacency_list[j-1]=edges_temp[edges_temp[:,2-1]==j,1-1].T
            adjacency_list[j-1]=cat(1,adjacency_list[j-1],weights_temp[int(edges_temp[:,2-1]==j)].T)
        if (TEdfs(adjacency_list)):
            solution=TEdyn(adjacency_list,k)
            alt_paths=0
            for j in arange(k-2*threshold,k+1).reshape(1,-1).flat:
                if j<1:
                    continue
                if  not isempty(solution[j-1,solution.shape[1]-1]):
                    alt_paths=1
                    break
            if logical(alt_paths):
                path_tree=TEbacktracking(solution,k,threshold)
                if  not isempty(path_tree):
                    path_count=0
                    for j in arange(1,size(path_tree,1)+1).reshape(1,-1).flat:
                        path_count=size(path_tree[j-1],1)+path_count
                        for jj in arange(1,size(path_tree[j-1],1)+1).reshape(1,-1).flat:
                            for jjj in arange(1,length(path_tree[j-1][jj-1])+1).reshape(1,-1).flat:
                                path_tree[j-1][jj-1][int(jjj-1)]=labels_vertices_temp[int(path_tree[j-1][jj-1][int(jjj-1)]-1)]
                    all_paths=m_array([[all_paths],[[i,s,t,path_count,path_tree]]]).reshape(1,-1)
                else:
                    no_nopath_TEbacktracking=no_nopath_TEbacktracking+1
            else:
                no_nopath_TEdyn=no_nopath_TEdyn+1
    ft_progress('close')
    if  not isempty(all_paths):
        disp(m_array(['For ',num2str(m_array([n_edges-no_nopath_TEdyn-no_nopath_TEbacktracking]).reshape(1,-1)),' of ',num2str(n_edges),' edges alternative paths were found.']).reshape(1,-1))
        disp(m_array(['     In ',num2str(no_nopath_TEdyn),' of ',num2str(n_edges),' cases TEdyn did not detect alternative paths.']).reshape(1,-1))
        disp(m_array(['     In ',num2str(no_nopath_TEbacktracking),' of ',num2str(n_edges),' cases TEbacktracking did not return valid alternative paths.']).reshape(1,-1))
        disp(' ')
        data_paths=TEflagedges(data,all_paths,edges,sgncmb_enum)
        data_paths.altpaths_thresh=cfg.threshold
        data_paths.graphanalysis=graphanalysis
        if  not isfield(data,'TEsteps'):
            data_paths.TEsteps='GA'
        else:
            data_paths.TEsteps=strcat(data.TEsteps,'_GA')
    else:
        disp('No alternative paths were found!')
    ft_progress('close')
    return data_paths,cfg
def TEgroup_calculate(filename,**kwargs):
    working_directory1=pwd()
    fprintf('\\nLoad data')
    varinfile=who('-file',filename)
    load(filename)
    x=strcat('data=',varinfile[1-1],';')
    eval(x)
    fprintf(' - ok')
    fprintf('\\nCheck data')
    if isfield(data(),'TEprepare')==0:
        error('TRENTOOL ERROR: The functions TEprepare and TEgroup_prepare must be performed on all datasets first, see help!')
    if isfield(data(),'TEgroupprepare')==0:
        error('TRENTOOL ERROR: The function TEgroup_prepare must be performed on all datasets first, see help!')
    fprintf(' - ok')
    cfg=data().TEgroupprepare.cfg
    cfg.calctime='yes'
    fprintf('\\nStart calculating transfer entropy for unshuffled data')
    cfg.shuffle='no'
    TEresult=transferentropy(cfg,data(),nargout=1)
    TEresult.TEprepare=data().TEprepare
    if strcmp(cfg.shifttest,'yes'):
        fprintf('\\nStart calculating transfer entropy for shifted data')
        cfg.calctime='no'
        cfg.numpermutation=190100
        cfg.shuffle='no'
        TEshift=transferentropy(cfg,data(),'shifttest',nargout=1)
        fprintf('\\nStart permutation tests for shift test')
        cfg.permstatstype='indepsamplesT'
        cfg.tail=1
        if strcmp(cfg.shifttesttype,'TE>TEshift'):
            cfg.alpha=0.05
            cfg.correctm='FDR'
            TEpermshift=TEperm(cfg,TEresult,TEshift)
            cfg=rmfield(cfg,'alpha')
            cfg=rmfield(cfg,'correctm')
        else:
            if strcmp(cfg.shifttesttype,'TEshift>TE'):
                cfg.alpha=0.05/length(data().TEgroupprepare.files)
                cfg.correctm='FDR'
                TEpermshift=TEperm(cfg,TEshift,TEresult)
                cfg=rmfield(cfg,'alpha')
                cfg=rmfield(cfg,'correctm')
        fprintf('\\nanalyze shift test\\n')
        if strcmp(cfg.shifttesttype,'TE>TEshift'):
            indexinstmix=find(TEpermshift.TEpermvalues[:,:,2-1]==0)
            if size(indexinstmix,1)==0:
                fprintf('No instantaneous mixing found!\\n')
            else:
                fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found!\\nFor these cases TEvalues of all trials are set to NaN!\\n'))
                mask=repmat((TEpermshift.TEpermvalues[:,:,2-1]-1)*-1,m_array([1,1,size(TEresult.TEmat,3)]).reshape(1,-1))
                TEresult.TEmat[int(mask==1)]=NaN()
                TEresult.MImat[int(mask==1)]=NaN()
                clear('mask')
                TEresult.instantaneousmixing=(TEpermshift.TEpermvalues[:,:,2-1]-1)*-1
        else:
            if strcmp(cfg.shifttesttype,'TEshift>TE'):
                indexinstmix=find(TEpermshift.TEpermvalues[:,:,2-1]==1)
                if size(indexinstmix,1)==0:
                    fprintf('No instantaneous mixing found!\\n')
                else:
                    fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found!\\nFor these cases TEvalues of all trials are set to NaN!\\n'))
                    mask=repmat(TEpermshift.TEpermvalues[:,:,2-1],m_array([1,1,size(TEresult.TEmat,3)]).reshape(1,-1))
                    TEresult.TEmat[int(mask==1)]=NaN()
                    TEresult.MImat[int(mask==1)]=NaN()
                    clear('mask')
                    TEresult.instantaneousmixing=TEpermshift.TEpermvalues[:,:,2-1]
        clear('TEpermshift')
        cfg=rmfield(cfg,'permstatstype')
        cfg=rmfield(cfg,'numpermutation')
        cfg=rmfield(cfg,'tail')
        cfg=rmfield(cfg,'calctime')
    TEresult.TEgroupprepare=data().TEgroupprepare
    TEresult.TEprepare=data().TEprepare
    TEresult.cfg=cfg
    savename=strcat(filename[1-1:filename.shape[0]-25+1-1],'TE_output.mat')
    save(savename,'TEresult')
    cd(working_directory1)
    return 
def prepare_data(TMPdata,**kwargs):
    if ischar(TMPdata[1-1,1-1]):
        state=1
    else:
        if isfield(TMPdata[1-1,1-1],'TEpermvalues'):
            state=2
        else:
            error('TRENTOOL error:wrong data input, see help')
    if size(TMPdata,1)>1:
        TMPdata=TMPdata.T
        if size(TMPdata,1)>1:
            error('TRENTOOL error:data input must be in 1xN or Nx1 format')
    working_directory1=pwd()
    if state==1:
        DataCell=[]
        for ll in arange(1,length(TMPdata)+1).reshape(1,-1).flat:
            varinfile=who('-file',TMPdata[ll-1])
            load(TMPdata[ll-1])
            x=strcat('DataCell{ll}=',varinfile[1-1],';')
            eval(x)
            y=strcat(m_array(['clear ',varinfile[1-1]]).reshape(1,-1))
            eval(y)
            clear('x','y','varinfile')
        clear('ll')
        nrdata=length(DataCell)
        if nrdata!=length(TMPdata):
            error('TRENTOOL error: unequal number of loaded Data and entries in FileCell')
        check_data(DataCell)
        data=DataCell
    if state==2:
        check_data(TMPdata)
        data=TMPdata
    cd(working_directory1)
    return data
def check_data(data,**kwargs):
    nr_comb=size(data[1-1,1-1].TEpermvalues,1)
    binomial_q=data[1-1,1-1].cfg.alpha
    for i in arange(1,size(data,2)+1).reshape(1,-1).flat:
        if ( not isfield(data[1-1,i-1],'sgncmb')) or (size(data[1-1,i-1].sgncmb,1)!=nr_comb) or (size(data[1-1,i-1].sgncmb,2)!=2):
            error(strcat('TRENTOOL error: There are errors in datafile. Check sgncmb field:',num2str(i)))
    for i in arange(2,size(data,2)+1).reshape(1,-1).flat:
        aux=size(data[1-1,i-1].TEpermvalues)!=size(data[1-1,i-1-1].TEpermvalues)
        if aux[int(1-1)] or aux[int(2-1)]:
            error(strcat('TRENTOOL error: There are errors in datafile. Check TEpermvalues field:',num2str(i)))
        if data[1-1,i-1].cfg.alpha!=binomial_q:
            error(strcat('TRENTOOL error: There are errors in datafile. Check cfg.alpha'))
    for i in arange(2,size(data,2)+1).reshape(1,-1).flat:
        for j in arange(1,nr_comb+1).reshape(1,-1).flat:
            if  not strcmp(data[1-1,i-1].sgncmb[j-1,1-1],data[1-1,i-1-1].sgncmb[j-1,1-1]) or  not strcmp(data[1-1,i-1].sgncmb[j-1,2-1],data[1-1,i-1-1].sgncmb[j-1,2-1]):
                error(strcat('TRENTOOL error: Mismatch sgncmb label in entry:',num2str(i)))
    return 
def binomtest(occur,tot_nr,alpha,**kwargs):
    bprob=1-binocdf(occur-1,tot_nr,alpha)
    return bprob
def TEsurrogatestats(cfg,data,**kwargs):
    cfg=_copy(cfg)
    working_directory1=pwd()
    fprintf('\\nCheck data and config')
    if  not isfield(data,'TEprepare'):
        fprintf('\\n')
        error('TRENTOOL ERROR: The function TEprepare must be performed on the data, see help!')
    data=ft_checkdata(data,'datatype','raw',nargout=1)
    if  not isfield(data,'trial'):
        fprintf('\\n')
        error("TRENTOOL ERROR: data must be in '.trial'-structure, see help!")
    if  not isfield(data,'time'):
        fprintf('\\n')
        error("TRENTOOL ERROR: data contains no '.time'-structure, see help!")
    if  not isfield(data,'label'):
        fprintf('\\n')
        error("TRENTOOL ERROR: data contains no '.label'-structure, see help!")
    if  not isfield(data,'fsample'):
        fprintf('\\n')
        error("TRENTOOL ERROR: data contains no '.fsample'-structure, see help!")
    if size(data.time,1)>size(data.time,2):
        data.time=data.time.T
    doublefields=0
    cfgTEprepare=data.TEprepare.cfg
    if isfield(cfg,'Path2TSTOOL') and isfield(cfgTEprepare,'Path2TSTOOL'):
        cfgTEprepare=rmfield(cfgTEprepare,'Path2TSTOOL')
    cfgfields=fieldnames(cfgTEprepare)
    cfgfields2=fieldnames(cfg)
    for ii in arange(1,size(cfgfields,1)+1).reshape(1,-1).flat:
        for jj in arange(1,size(cfgfields2,1)+1).reshape(1,-1).flat:
            if strcmp(cfgfields[ii-1],cfgfields2[jj-1]):
                doublefields=doublefields+1
    clear('cfgTEprepare')
    if doublefields>0:
        fprintf('\\n')
        error('TRENTOOL ERROR: Illegal attempt to overwrite entry generated by or used for TEprepare! Change cfg or rerun TEprepare. (see help)')
    names1=fieldnames(data.TEprepare.cfg)
    nr1=size(names1,1)
    for ii in arange(1,nr1+1).reshape(1,-1).flat:
        eval(strcat('cfg.',names1[ii-1],' = getfield(data.TEprepare.cfg, {1}, names1{ii});'))
    if  not isfield(cfg,'alpha'):
        cfg.alpha=0.05
    if  not isfield(cfg,'correctm'):
        cfg.correctm='FDR'
    if  not isfield(cfg,'tail'):
        cfg.tail=1
    if  not isfield(cfg,'permstatstype'):
        cfg.permstatstype='mean'
    if strcmp(cfg.permstatstype,'mean')==0 and strcmp(cfg.permstatstype,'indepsamplesT')==0 and strcmp(cfg.permstatstype,'depsamplesT')==0:
        fprintf('\\n')
        error("TRENTOOL ERROR: wrong cfg.permstatstype - use 'mean' 'depsamplesT' or 'indepsamplesT', see help!")
    if  not isfield(cfg,'shifttest'):
        cfg.shifttest='yes'
    if strcmp(cfg.shifttest,'yes')==0 and strcmp(cfg.shifttest,'no')==0:
        fprintf('\\n')
        error("TRENTOOL ERROR: wrong cfg.shifttest - use 'yes' or 'no', see help!")
    if strcmp(cfg.shifttest,'yes'):
        if  not isfield(cfg,'shifttype'):
            cfg.shifttype='predicttime'
        if  not isfield(cfg,'shifttesttype'):
            cfg.shifttesttype='TE>TEshift'
        if strcmp(cfg.shifttesttype,'TE>TEshift')==0 and strcmp(cfg.shifttesttype,'TEshift>TE')==0:
            fprintf('\\n')
            error("TRENTOOL ERROR: wrong cfg.shifttesttype - use 'TE>TEshift' or 'TEshift>TE', see help!")
    if  not isfield(cfg,'fileidout'):
        fprintf('\\n')
        error('TRENTOOL ERROR: cfg.fileidout must be defined, see help!')
    if  not isfield(cfg,'optdimusage'):
        fprintf('\\n')
        error('TRENTOOL ERROR: cfg.optdimusage is not defined, see help!')
    else:
        if strcmp(cfg.optdimusage,'maxdim')==0 and strcmp(cfg.optdimusage,'indivdim')==0:
            fprintf('\\n')
            error(m_array(['TRENTOOL ERROR: ',cfg.optdimusage,' is a wrong input for cfg.optdimusage , see help!']).reshape(1,-1))
    if  not isfield(cfg,'dim'):
        if strcmp(cfg.optdimusage,'indivdim'):
            cfg.dim=data.TEprepare.optdimmat
        else:
            cfg.dim[1-1:size(data.TEprepare.optdimmat,1)+1-1,1-1]=data.TEprepare.optdim
    else:
        if strcmp(cfg.optdimusage,'indivdim'):
            if size(cfg.dim,1)!=size(data.TEprepare.channelcombi,1):
                fprintf('\\n')
                error('TRENTOOL ERROR: cfg.dim has to be in that size: (channelcombi x 1), see help!')
            else:
                if size(cfg.dim,2)>1:
                    fprintf('\\n')
                    error('TRENTOOL ERROR: cfg.dim has to be in that size: (channelcombi x 1), see help!')
        else:
            if size(cfg.dim,1)>1 and size(cfg.dim,2)>1:
                fprintf('\\n')
                error('TRENTOOL ERROR: cfg.dim must include a scalar, see help!')
            if cfg.dim<data.TEprepare.optdim:
                fprintf('\\n')
                fprintf('TRENTOOL WARNING: specified embedding dimension (cfg.dim) is smaller then the optimal dimension from TEprepare.')
            else:
                if cfg.dim>data.TEprepare.optdim:
                    fprintf('\\n')
                    fprintf('TRENTOOL WARNING: specified embedding dimension (cfg.dim) is bigger then the optimal dimension from TEprepare.')
    if  not isfield(cfg,'tau'):
        if strcmp(data.TEprepare.cfg.optimizemethod,'ragwitz'):
            if strcmp(cfg.optdimusage,'indivdim'):
                cfg.tau=data.TEprepare.opttaumat
            else:
                cfg.tau[1-1:size(data.TEprepare.channelcombi,1)+1-1]=data.TEprepare.opttau
        else:
            if strcmp(data.TEprepare.cfg.optimizemethod,'cao'):
                cfg.tau[1-1:size(data.TEprepare.channelcombi,1)+1-1]=data.TEprepare.cfg.caotau
    else:
        if strcmp(cfg.optdimusage,'indivdim') and strcmp(data.TEprepare.cfg.optimizemethod,'ragwitz'):
            if size(cfg.tau,1)!=size(data.TEprepare.channelcombi,1):
                fprintf('\\n')
                error('TRENTOOL ERROR: cfg.tau has to be in that size: (channelconmbi x 1), see help!')
            else:
                if size(cfg.tau,2)>1:
                    fprintf('\\n')
                    error('TRENTOOL ERROR: cfg.tau has to be in that size: (channelconmbi x 1), see help!')
        else:
            if size(cfg.tau,1)>1 and size(cfg.tau,2)>1:
                fprintf('\\n')
                error('TRENTOOL ERROR: cfg.tau must include a scalar, see help!')
    if isempty(cfg.predicttime_u):
        error('TRENTOOL ERROR: specify cfg.predicttime_u, see help!')
    if  not isfield(cfg,'kth_neighbors'):
        cfg.kth_neighbors=4
    if  not isfield(cfg,'TheilerT'):
        cfg.TheilerT='ACT'
    if  not strcmp(cfg.TheilerT,'ACT'):
        if size(cfg.TheilerT,1)>1 or size(cfg.TheilerT,2)>1:
            fprintf('\\n')
            error('TRENTOOL ERROR: cfg.TheilerT must include a scalar, see help!')
    if size(cfg.toi,1)>size(cfg.toi,2):
        cfg.toi=cfg.toi.T
    else:
        if size(cfg.predicttime_u,1)>size(cfg.predicttime_u,2):
            cfg.predicttime_u=cfg.predicttime_u.T
        else:
            if size(cfg.kth_neighbors,1)>1 or size(cfg.kth_neighbors,2)>1:
                fprintf('\\n')
                error('TRENTOOL ERROR: cfg.dim must include a scalar, see help!')
    fprintf(' - ok')
    cfg.permtest.channelcombi=data.TEprepare.channelcombi
    cfg.permtest.channelcombilabel=data.TEprepare.channelcombilabel
    cfg.permtest.ACT=data.TEprepare.ACT
    trials=data.TEprepare.trials
    nrtrials=data.TEprepare.nrtrials
    cfg.permtest.trials=trials
    cfg.permtest.nrtrials=nrtrials
    fprintf('\\n\\nChecking number of permutations')
    nr2cmc=size(data.TEprepare.channelcombilabel,1)*size(cfg.predicttime_u,2)
    if  not isfield(cfg,'numpermutation'):
        cfg.numpermutation=190100
    else:
        if cfg.numpermutation<ceil(1/cfg.alpha):
            fprintf('\\n')
            error('TRENTOOL ERROR: cfg.numpermutation too small!')
        else:
            if nrtrials>31:
                if cfg.numpermutation>2**31:
                    fprintf('\\n')
                    error('TRENTOOL ERROR: cfg.numpermutation too huge!')
            else:
                if cfg.numpermutation>2**min(nrtrials):
                    fprintf('\\n')
                    error('TRENTOOL ERROR: cfg.numpermutation too huge!')
            if cfg.numpermutation<ceil(1/(cfg.alpha/nr2cmc)):
                fprintf('\\n#######################################################################################\\n# WARNING: Nr of permutations not sufficient for correction for multiple comparisons! #\\n#######################################################################################\\n')
    fprintf(' - ok\\n')
    cfg.calctime='yes'
    fprintf('\\nStart calculating transfer entropy for unshuffled data')
    cfg.shuffle='no'
    TEresult=transferentropy(cfg,data,nargout=1)
    TEresult.TEprepare=data.TEprepare
    cfg.calctime='no'
    if strcmp(cfg.shifttest,'yes'):
        fprintf('\\nStart calculating transfer entropy for shifted data')
        cfg.shuffle='no'
        TEshift=transferentropy(cfg,data,'shifttest',nargout=1)
        fprintf('\\nStart permutation tests for shift test')
        permstatstype=cfg.permstatstype
        cfg.permstatstype='indepsamplesT'
        tailtype=cfg.tail
        cfg.tail=1
        if strcmp(cfg.shifttesttype,'TE>TEshift'):
            alpha=cfg.alpha
            cfg.alpha=0.05
            TEpermshift=TEperm(cfg,TEresult,TEshift)
            cfg.alpha=alpha
        else:
            if strcmp(cfg.shifttesttype,'TEshift>TE'):
                alpha=cfg.alpha
                cfg.alpha=0.1
                TEpermshift=TEperm(cfg,TEshift,TEresult)
                cfg.alpha=alpha
        cfg.permstatstype=permstatstype
        cfg.tail=tailtype
        fprintf('\\nanalyze shift test\\n')
        NaNidx=find(isnan(TEresult.TEmat))
        if  not isempty(NaNidx):
            disp('Found NaN in TEresult.TEmat! Aborting')
            return TEpermtest()
        if strcmp(cfg.shifttesttype,'TE>TEshift'):
            indexinstmix=find(TEpermshift.TEpermvalues[:,2-1]==0)
            if size(indexinstmix,1)==0:
                fprintf('No instantaneous mixing found!\\n')
            else:
                fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found by strict shifttest!\\nFor these cases TEvalues of all trials are set to NaN!\\n'))
                mask=repmat((TEpermshift.TEpermvalues[:,2-1]-1)*-1,m_array([1,1,size(TEresult.TEmat,2)]).reshape(1,-1))
                TEresult.TEmat[int(mask==1)]=NaN()
                TEresult.MImat[int(mask==1)]=NaN()
                clear('mask')
                TEresult.instantaneousmixing=(TEpermshift.TEpermvalues[:,2-1]-1)*-1
        else:
            if strcmp(cfg.shifttesttype,'TEshift>TE'):
                indexinstmix=find(TEpermshift.TEpermvalues[:,2-1]==1)
                if size(indexinstmix,1)==0:
                    fprintf('No instantaneous mixing found!\\n')
                else:
                    fprintf(strcat(num2str(size(indexinstmix,1)),' instantaneous mixings found by non-strict shifttest!\\nFor these cases TEvalues of all trials are set to NaN!\\n'))
                    mask=repmat(TEpermshift.TEpermvalues[:,2-1],m_array([1,1,size(TEresult.TEmat,2)]).reshape(1,-1))
                    TEresult.TEmat[int(mask==1)]=NaN()
                    TEresult.MImat[int(mask==1)]=NaN()
                    clear('mask')
                    TEresult.instantaneousmixing=TEpermshift.TEpermvalues[:,2-1]
        clear('TEpermshift')
    fprintf('\\nStart calculating transfer entropy for shuffled data')
    cfg.shuffle='yes'
    TEshuffle=transferentropy(cfg,data,nargout=1)
    cfg=rmfield(cfg,'shuffle')
    cfg=rmfield(cfg,'calctime')
    fprintf('\\nStart permutation tests')
    TEpermtest=TEperm(cfg,TEresult,TEshuffle)
    TEpermtest.dimord='chanpair_value'
    TEpermtest.cfg=cfg
    TEpermtest.ACT.actvalue=data.TEprepare.ACT
    TEpermtest.sgncmb=TEresult.sgncmb
    TEpermtest.numpermutation=cfg.numpermutation
    TEpermtest.TEprepare=data.TEprepare
    TEpermtest.nr2cmc=nr2cmc
    fprintf('\\nCalculation ready\\n')
    fprintf('\\nSaving ...')
    fprintf('\\nResults of TE')
    save(strcat(cfg.fileidout,'_time',num2str(cfg.toi[int(1-1)]),'-',num2str(cfg.toi[int(2-1)]),'s_TE_output.mat'),'TEresult','-v7.3')
    fprintf(' - ok')
    fprintf('\\nResults of permutation test')
    save(strcat(cfg.fileidout,'_time',num2str(cfg.toi[int(1-1)]),'-',num2str(cfg.toi[int(2-1)]),'s_TEpermtest_output.mat'),'TEpermtest','-v7.3')
    fprintf(' - ok')
    cd(working_directory1)
    fprintf('\\n\\nThank you for using this transfer entropy tool!\\n')
    return TEpermtest