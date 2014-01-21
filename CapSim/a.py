# Autogenerated with SMOP version 0.20
import numpy,sys
from copy import copy as _copy
from smop.runtime import *
numpy.random.seed(0)
def do_LCP(M,Ja,Jc,Jf,q,x0_index,E,**kwargs):
    persistent('full_x0')
    if isempty(full_x0()) or size(full_x0(),1)!=E.n_total or any(abs(full_x0())>100000.0) or any(isnan(full_x0())):
        full_x0=ones(E.n_total,1)
    nm=size(M,1)
    if (E.k_fric==0) or isempty(Jf):
        J=m_array([Ja,Jc]).reshape(1,-1)
        nJ=size(J,2)
        H=m_array([m_array([[M],[J.T]]).reshape(1,-1),m_array([[-J],[sparse(nJ,nJ)]]).reshape(1,-1)]).reshape(1,-1)
        ix=x0_index
    else:
        nc=size(Jc,2)
        if E.friction_model==1:
            J=m_array([Ja,Jc+E.k_fric*Jf,Jc-E.k_fric*Jf]).reshape(1,-1)
            nJ=size(J,2)
            H=m_array([m_array([[M],[J.T]]).reshape(1,-1),m_array([[-J],[sparse(nJ,nJ)]]).reshape(1,-1)]).reshape(1,-1)
            q=m_array([[q],[q[q.shape[0]-nc+1-1:q.shape[0]+1-1,1-1]]]).reshape(1,-1)
            ixc=x0_index[x0_index.shape[0]-nc+1-1:x0_index.shape[0]+1-1]
            ix=m_array([[x0_index],[ixc+nc]]).reshape(1,-1)
        else:
            EE=sparse(arange(1,2*nc+1).reshape(1,-1),m_array([[arange(1,nc+1).reshape(1,-1)],[arange(1,nc+1).reshape(1,-1)]]).reshape(1,-1),ones(1,2*nc),2*nc,nc)
            Mu=E.k_fric*speye(nc)
            Jf=m_array([Jf,-Jf]).reshape(1,-1)
            J=m_array([Ja,Jc,Jf[:,m_array([[arange(1,nc+1).reshape(1,-1)],[arange(nc+1,2*nc+1).reshape(1,-1)]]).reshape(1,-1)-1]]).reshape(1,-1)
            na=size(Ja,2)
            nc=size(Jc,2)
            nj=size(J,2)
            H=m_array([m_array([[M],[J.T],[sparse(nc,nm)]]).reshape(1,-1),m_array([[-J],[sparse(nj,nj)],[m_array([sparse(nc,na),Mu,-EE.T]).reshape(1,-1)]]).reshape(1,-1),m_array([[sparse(nm+na+nc,nc)],[EE],[sparse(nc,nc)]]).reshape(1,-1)]).reshape(1,-1)
            J=m_array([J,sparse(nm,nc)]).reshape(1,-1)
            q=m_array([[q],[zeros(3*nc,1)]]).reshape(1,-1)
            ixc=x0_index[x0_index.shape[0]-nc+1-1:x0_index.shape[0]+1-1]
            ix=m_array([[x0_index],[ixc+nc],[ixc+2*nc],[ixc+3*nc]]).reshape(1,-1)
    x0=full_x0[int(ix-1)]
    nh=size(H,1)
    if nm==nh:
        x=numpy.linalg.solve(-H,q)
    else:
        if nh-nm==1:
            x=numpy.linalg.solve(-H,q)
            if E.skin>0:
                x[int(x.shape[0]-1)]=E.skin*log(1+exp(x[int(x.shape[0]-1)]/E.skin))
            else:
                x[int(x.shape[0]-1)]=max(x[int(x.shape[0]-1)],0)
        else:
            if E.solver==1:
                lb=m_array([[-inf(nm,1)],[zeros(nh-nm,1)]]).reshape(1,-1)
                ub=inf(nh,1)
                x=LCP(H,q,lb,ub,x0,0)
            else:
                lb=m_array([[-1e+20*ones(nm,1)],[zeros(nh-nm,1)]]).reshape(1,-1)
                ub=1e+20*ones(nh,1)
                x=x0
                status,tt=lcppath(length(q),nnz(H),x,lb,ub,H,q,nargout=2)
    full_x0[int(ix-1)]=x
    return x,J,ix
def dynamics(x,v,u,E,**kwargs):
    xnew,vnew,lambda_,cp=dyn(x,v,u,E,nargout=4)
    if E.RK:
        x1=xnew-x
        v1=vnew-v
        x2,v2,l2=dyn(x+0.5*x1,v+0.5*v1,u,E,nargout=3)
        x2=x2-(x+0.5*x1)
        v2=v2-(v+0.5*v1)
        x3,v3,l3=dyn(x+0.5*x2,v+0.5*v2,u,E,nargout=3)
        x3=x3-(x+0.5*x2)
        v3=v3-(v+0.5*v2)
        x4,v4,l4=dyn(x+x3,v+v3,u,E,nargout=3)
        x4=x4-(x+x3)
        v4=v4-(v+v3)
        xnew=x+(x1+x4)/6+(x2+x3)/3
        vnew=v+(v1+v4)/6+(v2+v3)/3
        lambda_=(lambda_+l4)/6+(l2+l3)/3
    return xnew,vnew,lambda_,cp
def dyn(x,v,u,E,**kwargs):
    dt=E.stretch*E.dt
    n=E.n
    nm=E.nm
    w_offset=E.w_offset
    c_offset=E.c_offset
    xc,T,Tx,Txx=m2c(x,E,nargout=4)
    Mx=T.T*E.M*Tx
    c=-Mx*(v**2)
    cv=-2*v[:,ones(nm,1)-1].T*Mx
    cx=-v[:,ones(nm,1)-1].T**2*(T.T*E.M*Txx)
    cx=cx-diag(sum(cx,2))
    cs=m_array([[cos(x[int(E.ang_m-1)].T)],[sin(x[int(E.ang_m-1)].T)]]).reshape(1,-1)
    dr_i=bsxfun(plus(),m_array([1,2,1,2,3]).reshape(1,-1).T,3*(arange(0,n-1+1).reshape(1,-1)))
    dr_j=bsxfun(plus(),m_array([1,1,2,2,3]).reshape(1,-1).T,3*(arange(0,n-1+1).reshape(1,-1)))
    er=m_array([[1,1],[0,1]]).reshape(1,-1)*E.radii.T
    dr_v=-2*E.k_drag*m_array([[sum(er[m_array([2,1]).reshape(1,-1)-1,:]*cs**2)],[m_array([[1],[1]]).reshape(1,-1)*((m_array([-1,1]).reshape(1,-1)*er)*prod(cs))],[sum(er*cs**2)],[er[1-1,:]**3/6]]).reshape(1,-1)
    dragx=T.T*sparse(dr_i,dr_j,dr_v)*T
    M=T.T*E.M*T
    Mi=M-dt*cv-dt**2*cx-dt*dragx
    gravity=E.k_grav.T*E.masses.T
    F=c-cv*v+T.T*(u+gravity[:])
    q=M*v+F*dt
    margins=reshape(T*v,size(xc))*dt
    margins=sqrt(sum(abs(margins*m_array([[ones(2,n)],[sum(E.radii,2).T]]).reshape(1,-1))**2))+E.margin
    ka,Ja=angle_constraints(x,E.Ja,E.a_lims,nargout=2)
    if E.k_fric>0:
        kw,Jw,ixw,Jwf=wall_constraints(xc,E.radii,E.walls,margins,nargout=4)
        kc,Jc,ixc,cp,Jcf=collision_constraints(xc,E.radii,E.collidable,margins,nargout=5)
        Jf=m_array([Jwf,Jcf]).reshape(1,-1)
    else:
        kw,Jw,ixw=wall_constraints(xc,E.radii,E.walls,margins,nargout=3)
        kc,Jc,ixc,cp=collision_constraints(xc,E.radii,E.collidable,margins,nargout=4)
        Jf=zeros(3*n,0)
    Jw=T.T*Jw
    Jf=T.T*Jf
    Jc=T.T*Jc
    ixw=ixw+w_offset
    ixc=ixc+c_offset
    ix=m_array([[(arange(1,nm+size(Ja,2)+1).reshape(1,-1)).T],[ixw],[ixc]]).reshape(1,-1)
    vlambda,J,ix=do_LCP(Mi,Ja,m_array([Jw,Jc]).reshape(1,-1),Jf,m_array([[-q],[ka/dt],[kw/dt],[kc/dt]]).reshape(1,-1),ix,E,nargout=3)
    lambda_=vlambda[nm+1-1:vlambda.shape[0]+1-1]
    cskin=1
    vnew=numpy.linalg.solve(Mi,(q+J*(cskin*lambda_)))
    xnew=x+E.dt*vnew
    vnew=v+(vnew-v)/E.stretch
    vl_s=sparse(E.n_total,1)
    vl_s[int(ix-1)]=vlambda
    lambda_=full(vl_s[nm+1-1:vl_s.shape[0]+1-1])
    return xnew,vnew,lambda_,cp
def angle_constraints(x,Ja,a_lims,**kwargs):
    n_a=size(Ja,2)
    phi=wrap(Ja.T*x)
    dum,thresh=min(abs(wrap(m_array([phi,phi]).reshape(1,-1)-a_lims)),m_array(),2,nargout=2)
    thresh=3-2*thresh
    J=Ja*sparse(arange(1,n_a+1).reshape(1,-1),arange(1,n_a+1).reshape(1,-1),thresh,n_a,n_a)
    k=wrap(phi-a_lims[int((arange(1,n_a+1).reshape(1,-1)).T-n_a*(thresh-1)/2-1)])*thresh
    return k,J
def wall_constraints(xc,radii,walls,margins,**kwargs):
    n=size(xc,2)
    nw=size(walls,1)
    if nw>0:
        Jv=zeros(3,2*n,nw)
        x=xc[1-1:2+1-1,:]
        a=xc[3-1,:]
        av=m_array([[cos(a)],[sin(a)]]).reshape(1,-1)
        rp=radii[:,m_array([1,1]).reshape(1,-1)-1].T*av
        p=m_array([x+rp,x-rp]).reshape(1,-1)
        k=walls*m_array([[p],[-ones(1,2*n)]]).reshape(1,-1)-ones(nw,1)*radii[m_array([arange(1,n+1).reshape(1,-1),arange(1,n+1).reshape(1,-1)]).reshape(1,-1)-1,2-1].T
        rw=bsxfun(times(),p23(-walls[:,1-1:2+1-1].T),radii[m_array([arange(1,n+1).reshape(1,-1),arange(1,n+1).reshape(1,-1)]).reshape(1,-1)-1,2-1].T)
        rel=bsxfun(plus(),m_array([rp,-rp]).reshape(1,-1),rw)
        N=permute(walls[:,1-1:2+1-1,ones(1,2*n)-1],m_array([2,3,1]).reshape(1,-1))
        Jv[1-1:2+1-1,:,:]=N
        Jv[3-1,:,:]=sum(m_array([[-rel[2-1,:,:]],[rel[1-1,:,:]]]).reshape(1,-1)*Jv[1-1:2+1-1,:,:],1)
        Jv=reshape(p23(Jv),m_array([3*nw,2*n]).reshape(1,-1))
        close=k<margins[ones(nw,1)-1,m_array([arange(1,n+1).reshape(1,-1),arange(1,n+1).reshape(1,-1)]).reshape(1,-1)-1]
        ac=find(close)
        ac=ac[:]
        aci,acj=find(close,nargout=2)
        acj=acj-n*(acj>n)
        na=length(ac)
        k=k[int(ac-1)]
        k=k[:]
        cols=m_array([[1],[1],[1]]).reshape(1,-1)*(arange(1,na+1).reshape(1,-1))
        rows=bsxfun(plus(),m_array([1,2,3]).reshape(1,-1).T,(acj.T-1)*3)
        Jvix=bsxfun(plus(),m_array([1,2,3]).reshape(1,-1).T,(ac.T-1)*3)
        J=sparse(rows[:],cols[:],Jv[int(Jvix-1)],3*n,na)
        ixw=ac
        if nargout()==4:
            Jfv=zeros(3,2*n,nw)
            Jfv[1-1:2+1-1,:,:]=m_array([[-N[2-1,:,:]],[N[1-1,:,:]]]).reshape(1,-1)
            Jfv[3-1,:,:]=sum(rel*N,1)
            Jfv=reshape(p23(Jfv),m_array([3*nw,2*n]).reshape(1,-1))
            Jf=sparse(rows[:],cols[:],Jfv[int(Jvix-1)],3*n,na)
    else:
        J=sparse(m_array(),m_array(),m_array(),3*n,0)
        Jf=sparse(m_array(),m_array(),m_array(),3*n,0)
        k=m_array()
        ixw=m_array()
    return k,J,ixw,Jf
def collision_constraints(xc,radii,collidable,margins,**kwargs):
    nc=nnz(collidable)
    e1,e2=find(collidable,nargout=2)
    if nc>0:
        cl_pairs=sum((xc[m_array([1,2]).reshape(1,-1)-1,e1-1]-xc[m_array([1,2]).reshape(1,-1)-1,e2-1])**2,1).T<(margins[int(e1-1)].T+margins[int(e2-1)].T+sum(radii[e1-1,:],2)+sum(radii[e2-1,:],2))**2
        na=sum(cl_pairs)
        e1=e1[int(cl_pairs-1)]
        e2=e2[int(cl_pairs-1)]
    else:
        na=0
    if na>0:
        z=zeros(8,4)
        z[int(m_array([[arange(1,8+1,2).reshape(1,-1)],[arange(2,8+1,2).reshape(1,-1)]]).reshape(1,-1)+8*m_array([[arange(0,3+1).reshape(1,-1)],[arange(0,3+1).reshape(1,-1)]]).reshape(1,-1)-1)]=1
        z=z.T
        r=radii.T
        x=xc[1-1:2+1-1,:]
        a=xc[3-1,:]
        v=m_array([[cos(a)],[sin(a)]]).reshape(1,-1)
        p=m_array([[x+r[m_array([1,1]).reshape(1,-1)-1,:]*v],[x-r[m_array([1,1]).reshape(1,-1)-1,:]*v]]).reshape(1,-1)
        pp=m_array([[p[:,e1-1]],[p[:,e2-1]]]).reshape(1,-1)
        xx=m_array([[x[m_array([1,2,1,2]).reshape(1,-1)-1,e2-1]],[x[m_array([1,2,1,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
        vv=m_array([[v[m_array([1,2,1,2]).reshape(1,-1)-1,e2-1]],[v[m_array([1,2,1,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
        rr1=m_array([[r[m_array([1,1]).reshape(1,-1)-1,e2-1]],[r[m_array([1,1]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
        rr2=m_array([[r[m_array([2,2]).reshape(1,-1)-1,e2-1]],[r[m_array([2,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
        t=z*((pp-xx)*vv)
        t=min(rr1,max(-rr1,t))
        uu=xx+vv*t[m_array([1,1,2,2,3,3,4,4]).reshape(1,-1)-1,:]
        d=z*((uu-pp)**2)
        l,i=min(d,m_array(),1,nargout=2)
        ax=i+4*(arange(0,na-1+1).reshape(1,-1))
        bx=5-i+4*(arange(0,na-1+1).reshape(1,-1))
        ix=m_array([[2*i-1],[2*i]]).reshape(1,-1)+m_array([[8],[8]]).reshape(1,-1)*(arange(0,na-1+1).reshape(1,-1))
        jx=m_array([[2*(5-i)-1],[2*(5-i)]]).reshape(1,-1)+m_array([[8],[8]]).reshape(1,-1)*(arange(0,na-1+1).reshape(1,-1))
        s=m_array([[pp[int(ix-1)]],[uu[int(ix-1)]]]).reshape(1,-1)
        sr=uu[int(ix-1)]-pp[int(ix-1)]
        sr=sr*(m_array([1,1]).reshape(1,-1).T*sum(sr**2,1)**(-0.5))
        dir=sign(sum(sr*(xx[int(ix-1)]-xx[int(jx-1)]),1))
        sr=sr*dir[m_array([1,1]).reshape(1,-1)-1,:]
        rr3=m_array([[m_array([1,1]).reshape(1,-1).T*rr2[int(bx-1)]],[m_array([1,1]).reshape(1,-1).T*rr2[int(ax-1)]]]).reshape(1,-1)
        sc=s+m_array([[sr],[-sr]]).reshape(1,-1)*rr3
        scr=sc[m_array([3,4]).reshape(1,-1)-1,:]-sc[m_array([1,2]).reshape(1,-1)-1,:]
        k=sum(scr*sr,1).T
        rel=sc-m_array([[xx[int(jx-1)]],[xx[int(ix-1)]]]).reshape(1,-1)
        torques=m_array([[1,1,0,0],[0,0,1,1]]).reshape(1,-1)*(m_array([[m_array([[0,-1],[1,0]]).reshape(1,-1)*sr],[m_array([[0,1],[-1,0]]).reshape(1,-1)*sr]]).reshape(1,-1)*rel)
        Jv=m_array([[-sr],[torques[1-1,:]],[sr],[torques[2-1,:]]]).reshape(1,-1)
        Jv[:,i>2-1]=Jv[m_array([arange(4,6+1).reshape(1,-1),arange(1,3+1).reshape(1,-1)]).reshape(1,-1).T-1,i>2-1]
        rows=m_array([[bsxfun(plus(),e1*3-2,m_array([0,1,2]).reshape(1,-1)).T],[bsxfun(plus(),e2*3-2,m_array([0,1,2]).reshape(1,-1)).T]]).reshape(1,-1)
        cols=m_array([[1],[1],[1],[1],[1],[1]]).reshape(1,-1)*(arange(1,na+1).reshape(1,-1))
        J=sparse(rows[:],cols[:],Jv,numel(xc),na)
        ixc=collidable[int(e1+size(collidable,1)*(e2-1)-1)]
        x=m_array([sc[1-1:2+1-1,:],sc[3-1:4+1-1,:]]).reshape(1,-1).T
        if nargout()==5:
            torques=m_array([[1,1,0,0],[0,0,1,1]]).reshape(1,-1)*(m_array([[sr],[-sr]]).reshape(1,-1)*rel)
            Jfv=m_array([[m_array([[0,-1],[1,0]]).reshape(1,-1)*sr],[torques[1-1,:]],[m_array([[0,1],[-1,0]]).reshape(1,-1)*sr],[torques[2-1,:]]]).reshape(1,-1)
            Jfv[:,i>2-1]=Jfv[m_array([arange(4,6+1).reshape(1,-1),arange(1,3+1).reshape(1,-1)]).reshape(1,-1).T-1,i>2-1]
            Jf=sparse(rows[:],cols[:],Jfv,numel(xc),na)
    else:
        J=sparse(m_array(),m_array(),m_array(),numel(xc),0)
        Jf=sparse(m_array(),m_array(),m_array(),numel(xc),0)
        k=m_array()
        ixc=m_array()
        x=zeros(0,2)
    return k,J,ixc,x,Jf
def wrap(x,**kwargs):
    Z=(x>pi())*(x-2*pi())+(x<-pi())*(x+2*pi())
    d=Z+x* not Z
    return d
def m2c(x,E,**kwargs):
    nm=E.nm
    n=E.n
    np=size(E.ang_p,1)
    nf=sum( not E.ang_p)
    cm=x[int( not E.ang_m-1)]
    theta=x[int(E.ang_m-1)]
    ct=cos(theta)
    st=sin(theta)
    z=zeros(np,2)
    z[E.ang_p-1,:]=m_array([ct,st]).reshape(1,-1)
    z[ not E.ang_p-1,:]=reshape(cm,m_array([2,nf]).reshape(1,-1)).T
    xc=m_array([E.P*z+E.offsets,theta]).reshape(1,-1).T
    if nargout()>1:
        if E.use_sparse:
            Ct=E.P[:,E.ang_p-1]*sparse(arange(1,n+1).reshape(1,-1),arange(1,n+1).reshape(1,-1),ct,n,n)
            St=E.P[:,E.ang_p-1]*sparse(arange(1,n+1).reshape(1,-1),arange(1,n+1).reshape(1,-1),st,n,n)
            Pc=E.P
            Pc[:,E.ang_p-1]=Ct
            pc=Pc[int(E.P!=0)]
            Ps=E.P
            Ps[:,E.ang_p-1]=-St
            ps=Ps[int(E.P!=0)]
            vT=m_array([[ps],[pc],[ones(n,1)]]).reshape(1,-1)
            T=sparse(E.ijvT[:,1-1],E.ijvT[:,2-1],vT[int(E.ijvT[:,3-1]-1)],3*n,nm)
            if nargout()>2:
                pc=Ct[int(E.P[:,E.ang_p-1]!=0)]
                ps=St[int(E.P[:,E.ang_p-1]!=0)]
                vT=m_array([[-pc],[-ps]]).reshape(1,-1)
                Tx=sparse(E.ijvTx[:,1-1],E.ijvTx[:,2-1],vT[int(E.ijvTx[:,3-1]-1)],3*n,nm)
                if nargout()>3:
                    vT=m_array([[ps],[-pc]]).reshape(1,-1)
                    Txx=sparse(E.ijvTx[:,1-1],E.ijvTx[:,2-1],vT[int(E.ijvTx[:,3-1]-1)],3*n,nm)
        else:
            Ct=E.P[:,E.ang_p-1]*ct[:,ones(1,n)-1].T
            St=E.P[:,E.ang_p-1]*st[:,ones(1,n)-1].T
            Pc=E.P
            Pc[:,E.ang_p-1]=Ct
            Ps=E.P
            Ps[:,E.ang_p-1]=-St
            vT=m_array([[Ps[:]],[Pc[:]],[ones(n,1)]]).reshape(1,-1)
            T=zeros(3*n,nm)
            T[int(E.ixT-1)]=vT[int(E.vT-1)]
            if nargout()>2:
                vTx=m_array([[-Ct[:]],[-St[:]]]).reshape(1,-1)
                Tx=zeros(3*n,nm)
                Tx[int(E.ixTx-1)]=vTx[int(E.vTx-1)]
                if nargout()>3:
                    vTxx=m_array([[St[:]],[-Ct[:]]]).reshape(1,-1)
                    Txx=zeros(3*n,nm)
                    Txx[int(E.ixTx-1)]=vTxx[int(E.vTx-1)]
    return xc,T,Tx,Txx
def p23(x,**kwargs):
    s1,s2,s3=size(x,nargout=3)
    if s2<2 and s3<2:
        px=x
    else:
        px=permute(x,m_array([1,3,2]).reshape(1,-1))
    return px
def setOpts(defaults,options,**kwargs):
    if nargin()==1 or isempty(options):
        user_fields=m_array()
    else:
        if isstruct(options):
            user_fields=fieldnames(options)
        else:
            user_fields=options[1-1:2:options.shape[0]+1-1]
            options=struct(options[:])
    if isstruct(defaults):
        params=_copy(defaults)
    else:
        params=struct(defaults[:])
    for k in arange(1,length(user_fields)+1).reshape(1,-1).flat:
        setfield(params,user_fields[k-1],getfield(options,user_fields[k-1]))
    return params
def nearest(xc,radii,cp,**kwargs):
    n=size(xc,2)
    xc=m_array([xc,m_array([[cp.T],[0]]).reshape(1,-1)]).reshape(1,-1)
    radii=m_array([[radii],[0,0]]).reshape(1,-1)
    na=n
    e1,e2=deal((arange(1,n+1).reshape(1,-1)).T,(n+1)*ones(n,1),nargout=2)
    z=zeros(8,4)
    z[int(m_array([[arange(1,8+1,2).reshape(1,-1)],[arange(2,8+1,2).reshape(1,-1)]]).reshape(1,-1)+8*m_array([[arange(0,3+1).reshape(1,-1)],[arange(0,3+1).reshape(1,-1)]]).reshape(1,-1)-1)]=1
    z=z.T
    r=radii.T
    x=xc[1-1:2+1-1,:]
    a=xc[3-1,:]
    v=m_array([[cos(a)],[sin(a)]]).reshape(1,-1)
    p=m_array([[x+r[m_array([1,1]).reshape(1,-1)-1,:]*v],[x-r[m_array([1,1]).reshape(1,-1)-1,:]*v]]).reshape(1,-1)
    pp=m_array([[p[:,e1-1]],[p[:,e2-1]]]).reshape(1,-1)
    xx=m_array([[x[m_array([1,2,1,2]).reshape(1,-1)-1,e2-1]],[x[m_array([1,2,1,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
    vv=m_array([[v[m_array([1,2,1,2]).reshape(1,-1)-1,e2-1]],[v[m_array([1,2,1,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
    rr1=m_array([[r[m_array([1,1]).reshape(1,-1)-1,e2-1]],[r[m_array([1,1]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
    rr2=m_array([[r[m_array([2,2]).reshape(1,-1)-1,e2-1]],[r[m_array([2,2]).reshape(1,-1)-1,e1-1]]]).reshape(1,-1)
    t=z*((pp-xx)*vv)
    t=min(rr1,max(-rr1,t))
    uu=xx+vv*t[m_array([1,1,2,2,3,3,4,4]).reshape(1,-1)-1,:]
    d=z*((uu-pp)**2)
    l,i=min(d,m_array(),1,nargout=2)
    ax=i+4*(arange(0,na-1+1).reshape(1,-1))
    bx=5-i+4*(arange(0,na-1+1).reshape(1,-1))
    ix=m_array([[2*i-1],[2*i]]).reshape(1,-1)+m_array([[8],[8]]).reshape(1,-1)*(arange(0,na-1+1).reshape(1,-1))
    jx=m_array([[2*(5-i)-1],[2*(5-i)]]).reshape(1,-1)+m_array([[8],[8]]).reshape(1,-1)*(arange(0,na-1+1).reshape(1,-1))
    s=m_array([[pp[int(ix-1)]],[uu[int(ix-1)]]]).reshape(1,-1)
    sr=uu[int(ix-1)]-pp[int(ix-1)]
    sr=sr*(m_array([1,1]).reshape(1,-1).T*sum(sr**2,1)**(-0.5))
    dir=sign(sum(sr*(xx[int(ix-1)]-xx[int(jx-1)]),1))
    sr=sr*dir[m_array([1,1]).reshape(1,-1)-1,:]
    rr3=m_array([[m_array([1,1]).reshape(1,-1).T*rr2[int(bx-1)]],[m_array([1,1]).reshape(1,-1).T*rr2[int(ax-1)]]]).reshape(1,-1)
    sc=s+m_array([[sr],[-sr]]).reshape(1,-1)*rr3
    scr=sc[m_array([3,4]).reshape(1,-1)-1,:]-sc[m_array([1,2]).reshape(1,-1)-1,:]
    k=sum(scr*sr,1).T
    rel=sc-m_array([[xx[int(jx-1)]],[xx[int(ix-1)]]]).reshape(1,-1)
    rel[:,i>2-1]=rel[m_array([arange(3,4+1).reshape(1,-1),arange(1,2+1).reshape(1,-1)]).reshape(1,-1).T-1,i>2-1]
    dum,w=min(k,nargout=2)
    rel=expm(m_array([[0,1],[-1,0]]).reshape(1,-1)*a[int(w-1)])*rel[1-1:2+1-1,w-1]
    return w,rel
def fDown(src,evnt,**kwargs):
    axis=findobj(src,'tag','simulation')
    cp=get(axis,'CurrentPoint')
    cp=cp[1-1,1-1:2+1-1]
    xlim=get(axis,'xlim')
    ylim=get(axis,'ylim')
    if cp[int(1-1)]>xlim[int(1-1)] and cp[int(1-1)]<xlim[int(2-1)] and cp[int(2-1)]>ylim[int(1-1)] and cp[int(2-1)]<ylim[int(2-1)]:
        set(src,'WindowButtonMotionFcn',fMove())
        setappdata(src,'cursorPos',cp)
    return 
def fMove(src,evnt,**kwargs):
    h=findobj(src,'tag','simulation')
    cp=get(h,'CurrentPoint')
    cp=cp[1-1,1-1:2+1-1]
    setappdata(src,'cursorPos',cp)
    return 
def fUp(src,evnt,**kwargs):
    set(src,'WindowButtonMotionFcn','')
    setappdata(src,'cursorPos','')
    return 
def fKey(fig,evnt,**kwargs):
    if strcmp(evnt.Key,'escape'):
        setappdata(fig,'Stop',1)
    return 
def wrap(x,**kwargs):
    Z=(x>pi())*(x-2*pi())+(x<-pi())*(x+2*pi())
    d=Z+x* not Z
    return d