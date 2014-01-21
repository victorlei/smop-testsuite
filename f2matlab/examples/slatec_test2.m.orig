function dd(varargin)
clear global; clear functions;


end %program dd
function [lun,kprint,ipass]=bspck(lun,kprint,ipass);
persistent a aa adif alf as atol b bc bell bet bquad bv c cc ccv cv den dn dpi dpiov2 er fatal fbcl fbcr ff firstCall flag ftl ftl1 ftl2 i ibcl ibcr id idim_v ierr iknt ileft ilo inbv inc incmax inev inppv iwork j jhigh jj k kb kk knt kntopt kontrl ldc ldcc lf longname ltest lxi mflag n nalf nbet ndata nerr nidim ninc nkb nmax nmk nn nout nsubs pi piov2 pquad q qq qsave quad snames spv stars summlv sv t tab tol trans w x x1 x2 xi xl xs xx y ys yt yy z ; if isempty(snames),snames={};end; if isempty(firstCall),firstCall=1;end; 

if isempty(atol), atol=0; end;
if isempty(bquad), bquad=0; end;
if isempty(bv), bv=0; end;
if isempty(den), den=0; end;
if isempty(dn), dn=0; end;
if isempty(er), er=0; end;
if isempty(fbcl), fbcl=0; end;
if isempty(fbcr), fbcr=0; end;
if isempty(pquad), pquad=0; end;
if isempty(quad), quad=0; end;
if isempty(spv), spv=0; end;
if isempty(tol), tol=0; end;
if isempty(x1), x1=0; end;
if isempty(x2), x2=0; end;
if isempty(xl), xl=0; end;
if isempty(xx), xx=0; end;
if isempty(i), i=0; end;
if isempty(ibcl), ibcl=0; end;
if isempty(ibcr), ibcr=0; end;
if isempty(id), id=0; end;
if isempty(ierr), ierr=0; end;
if isempty(iknt), iknt=0; end;
if isempty(ileft), ileft=0; end;
if isempty(ilo), ilo=0; end;
if isempty(inbv), inbv=0; end;
if isempty(inev), inev=0; end;
if isempty(inppv), inppv=0; end;
if isempty(iwork), iwork=0; end;
if isempty(j), j=0; end;
if isempty(jhigh), jhigh=0; end;
if isempty(jj), jj=0; end;
if isempty(k), k=0; end;
if isempty(kk), kk=0; end;
if isempty(knt), knt=0; end;
if isempty(kntopt), kntopt=0; end;
if isempty(kontrl), kontrl=0; end;
if isempty(ldc), ldc=0; end;
if isempty(ldcc), ldcc=0; end;
if isempty(lxi), lxi=0; end;
if isempty(mflag), mflag=0; end;
if isempty(n), n=0; end;
if isempty(ndata), ndata=0; end;
if isempty(nerr), nerr=0; end;
if isempty(nmk), nmk=0; end;
if isempty(nn), nn=0; end;
if isempty(fatal), fatal=false; end;
if isempty(summlv), summlv=0; end;
if isempty(nmax), nmax=65; end;
if isempty(incmax), incmax=2 ; end;
if isempty(adif), adif=zeros(52+4,10+1,3); end;
if isempty(bc), bc=zeros(1,13+1); end;
if isempty(c), c=zeros(4,10); end;
if isempty(cc), cc=zeros(4,4); end;
if isempty(q), q=zeros(1,3); end;
if isempty(qq), qq=zeros(1,77); end;
if isempty(qsave), qsave=zeros(1,2); end;
if isempty(sv), sv=zeros(1,4); end;
if isempty(t), t=zeros(1,17); end;
if isempty(w), w=zeros(1,65); end;
if isempty(x), x=zeros(1,11); end;
if isempty(xi), xi=zeros(1,11); end;
if isempty(cv), cv = 2.9979251; end;
if isempty(y), y =(4.1 ./ 3.0); end;
if isempty(ccv), ccv =complex(2.9979251,1.2); end;
if isempty(pi), pi=3.1415927; end;
if isempty(dpi), dpi=3.141592653589793238d0 ; end;
if isempty(piov2), piov2=pi./2; end;
if isempty(dpiov2), dpiov2=dpi./2 ; end;
if isempty(flag), flag=true; end;
if isempty(longname), longname='A STRING OF 25 CHARACTERS' ; end;
if isempty(nout), nout=0; end;
if isempty(nidim), nidim=6; end;
if isempty(nkb), nkb=4; end;
if isempty(ninc), ninc=4; end;
if isempty(nalf), nalf=3; end;
if isempty(nbet), nbet=3 ; end;
global bcom_2; if isempty(bcom_2), bcom_2=false; end;
global bcom_1; if isempty(bcom_1), bcom_1=false; end;
if isempty(ftl), ftl=false; end;
if isempty(ftl1), ftl1=false; end;
if isempty(ftl2), ftl2=false; end;
%% common tsterr,same;
%% common bcom_1,bcom_2;
if isempty(trans), trans=repmat(' ',1,1); end;
if isempty(aa), aa=zeros(1,nmax.*nmax); end;
if isempty(alf), alf=zeros(1,nalf); end;
if isempty(as), as=zeros(1,nmax.*nmax); end;
if isempty(bet), bet=zeros(1,nbet); end;
if isempty(xs), xs=zeros(1,nmax.*incmax); end;
if isempty(ys), ys=zeros(1,nmax.*incmax); end;
if isempty(yt), yt=zeros(1,nmax); end;
if isempty(yy), yy=zeros(1,nmax.*incmax); end;
if isempty(z), z=zeros(1,2.*nmax); end;
if isempty(idim_v), idim_v=zeros(1,6); end;
if isempty(inc), inc=zeros(1,4); end;
if isempty(kb), kb=zeros(1,4); end;
if isempty(nsubs), nsubs=17 ; end;
if isempty(ltest), ltest=zeros(1,nsubs); end;
if isempty(snames), snames=cell(1,nsubs); end;
if firstCall,   snames={'CGEMV','CGBMV','CHEMV','CHBMV','CHPMV','CTRMV','CTBMV','CTPMV','CTRSV','CTBSV','CTPSV','CGERC','CGERU','CHER','CHPR','CHER2','CHPR2'};  end;
if firstCall,   idim_v=[0,1,2,3,5,9];  end;
if firstCall,   kb=[0,1,2,4];  end;
if isempty(a), a=zeros(1,10); end;
if isempty(b), b=zeros(1,10); end;
if isempty(bell), bell=repmat(' ',1,1); end;
if isempty(tab), tab=repmat(' ',1,1); end;
if isempty(lf), lf=repmat(' ',1,1); end;
if isempty(ff), ff=repmat(' ',1,1); end;
if isempty(stars), stars=repmat(' ',1,6); end;
if firstCall,   bell=[7];  end;
if firstCall, tab=[9];  end;
if firstCall, lf=[10];  end;
if firstCall, ff =[12];  end;
if firstCall,   a=[0];  end;
if firstCall, stars =[0,0,0,0,0,0,0,0,0,'****'];  end;
if firstCall,   inc=[1,2,-1,-2];  end;
if firstCall,  alf=[complex(0.0,0.0),complex(1.0,0.0),complex(0.7,-0.9)];  end;
firstCall=0;
if( kprint>=2 )
writef(lun,['1 QUICK CHECK FOR SPLINE ROUTINES', '\n ' , '\n '  ' \n']);
end;
%format ('1 QUICK CHECK FOR SPLINE ROUTINES',/];
ipass = 1;
tol = 1000.0e0.*r1mach(4);
end %subroutine bspck
function [r1machresult,i]=r1mach(i);
r1machresult=[];
persistent diver firstCall large log10mlv r1mach right rmach small ; if isempty(firstCall),firstCall=1;end; 

if isempty(r1machresult), r1machresult=0; end;
if isempty(small), small=zeros(1,2); end;
if isempty(large), large=zeros(1,2); end;
if isempty(right), right=zeros(1,2); end;
if isempty(diver), diver=zeros(1,2); end;
if isempty(log10mlv), log10mlv=zeros(1,2); end;
if isempty(rmach), rmach=zeros(1,5); end;
% equivalence(rmach(1),small(1)) ::;
% equivalence(rmach(2),large(1)) ::;
% equivalence(rmach(3),right(1)) ::;
% equivalence(rmach(4),diver(1)) ::;
% equivalence(rmach(5),log10(1)) ::;
if firstCall,   small(1) =[1.18e-38];  end;
if firstCall,   large(1) =[3.40e+38];  end;
if firstCall,   right(1) =[0.595e-07];  end;
if firstCall,   diver(1) =[1.19e-07];  end;
if firstCall,   log10mlv(1) =[0.30102999566];  end;
firstCall=0;
if( i < 1 || i > 5 )
% call XERMSG ('SLATEC', 'R1MACH', 'I OUT OF BOUNDS', 1, 2)
end;
rmach(1)=small(1);
rmach(2)=large(1);
rmach(3)=right(1);
rmach(4)=diver(1);
rmach(5)=log10mlv(1);
r1machresult = rmach(i);





return;
end %function r1mach




function out=writef(fid,varargin)
% function out=writef(fid,varargin)
%  Catches fortran stdout (6) and reroutes in to Matlab's stdout (1)
%  Catches fortran stderr (0) and reroutes in to Matlab's stderr (2)
if isnumeric(fid)
 if fid==6,      out=fprintf(1,varargin{:});
 elseif fid==0,  out=fprintf(2,varargin{:});
 elseif isempty(fid) %% treat empty array like a string array [sethg 2008-03-03]
  out=sprintf(varargin{:});
  if nargin>2 %set the calling var to out
   if ~isempty(inputname(1)), assignin('caller',inputname(1),out); end
  end
 else,           out=fprintf(fid,varargin{:});
 end
elseif ischar(fid)
 out=sprintf(varargin{:});
 if nargin>2 %set the calling var to out
  if ~isempty(inputname(1)), assignin('caller',inputname(1),out); end
 end
else,            out=fprintf(fid,varargin{:});
end
end