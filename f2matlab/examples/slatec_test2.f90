program dd

end program dd


SUBROUTINE BSPCK(Lun,Kprint,Ipass) 
IMPLICIT NONE 
INTEGER Ipass , Kprint , Lun 
REAL atol , bquad , bv , den , dn , er , fbcl , fbcr ,        &
   &     pquad , quad , spv , tol , x1 , x2 , xl , xx                 
INTEGER i , ibcl , ibcr , id , ierr , iknt , ileft , ilo , inbv , &
   &        inev , inppv , iwork , j , jhigh , jj , k , kk , knt ,    &
   &        kntopt , kontrl , ldc , ldcc , lxi , mflag , n , ndata ,  &
   &        nerr , nmk , nn                                           
LOGICAL fatal 
INTEGER NMAX , INCMAX , sum
PARAMETER (NMAX=65,INCMAX=2) 
REAL adif(-3:52,0:10,-incmax:3),bc(0:13) , c , cc(4,4) , q(3) , qq(77) ,&
   &     qsave(2) , sv(4) , t(17) , w(65) , x(11) , xi(11)
dimension c(4,10)
REAL, PARAMETER :: Cv = 2.9979251, Y = (4.1 / 3.0)
complex, PARAMETER :: Ccv = (2.9979251,1.2)
REAL(4) PI, PIOV2
REAL(8) DPI, DPIOV2
LOGICAL FLAG
CHARACTER*(*) LONGNAME
PARAMETER (PI=3.1415927, DPI=3.141592653589793238D0)
PARAMETER (PIOV2=PI/2, DPIOV2=DPI/2)
PARAMETER (FLAG=.TRUE., LONGNAME='A STRING OF 25 CHARACTERS')
INTEGER NALF , NBET , NIDIM , NINC , NKB ,    &
   &        Nout                                                      
REAL BVALU , FB , PPVAL , R1MACH 
INTEGER NUMXER 
EXTERNAL BVALU , FB , NUMXER , PPVAL , R1MACH 
EXTERNAL BFQAD , BINT4 , BINTK , BSPDR , BSPEV , BSPPP , BSPVD ,  &
   &         BSPVN , BSQAD , INTRV , PFQAD , PPQAD , XGETF , XSETF    
PARAMETER (NIDIM=6,NKB=4,NINC=4,NALF=3,NBET=3) 
LOGICAL same , tsterr , ftl , ftl1 , ftl2 
common tsterr,same
CHARACTER*1 trans 
COMPLEX,save:: aa(NMAX*NMAX) , alf(NALF) , as(NMAX*NMAX) ,&
   &        bet(NBET) , xs(NMAX*INCMAX), &
   &        ys(NMAX*INCMAX) , yt(NMAX) , yy(NMAX*INCMAX) ,  &
   &        z(2*NMAX)                                                 
INTRINSIC ABS , SIN 
INTEGER,save:: idim_v(6) , inc(4) , kb(4) 
INTEGER NSUBS 
PARAMETER (NSUBS=17) 
LOGICAL ltest(NSUBS) 
CHARACTER*6 snames(NSUBS) 
DATA snames/'CGEMV ' , 'CGBMV ' , 'CHEMV ' , 'CHBMV ' , 'CHPMV ' ,&
   &     'CTRMV ' , 'CTBMV ' , 'CTPMV ' , 'CTRSV ' , 'CTBSV ' ,       &
   &     'CTPSV ' , 'CGERC ' , 'CGERU ' , 'CHER  ' , 'CHPR  ' ,       &
   &     'CHER2 ' , 'CHPR2 '/                                         
DATA idim_v/0 , 1 , 2 , 3 , 5 , 9/ 
DATA kb/0 , 1 , 2 , 4/ 
INTEGER A(10), B(10)
CHARACTER BELL, TAB, LF, FF, STARS*6
DATA BELL,TAB,LF,FF /7,9,10,12/
DATA A,STARS /10*0,'****'/
DATA inc/1 , 2 , -1 , -2/, alf/(0.0,0.0) , (1.0,0.0) , (0.7,-0.9)/ 

IF ( Kprint>=2 ) WRITE (Lun,99001) 
99001 FORMAT ('1 QUICK CHECK FOR SPLINE ROUTINES',//) 
Ipass = 1 
tol = 1000.0E0*R1MACH(4) 
END SUBROUTINE BSPCK


FUNCTION R1MACH (I)
real r1mach
REAL SMALL(2)
REAL LARGE(2)
REAL RIGHT(2)
REAL DIVER(2)
REAL LOG10(2)
REAL RMACH(5)
SAVE RMACH
EQUIVALENCE (RMACH(1),SMALL(1))
EQUIVALENCE (RMACH(2),LARGE(1))
EQUIVALENCE (RMACH(3),RIGHT(1))
EQUIVALENCE (RMACH(4),DIVER(1))
EQUIVALENCE (RMACH(5),LOG10(1))
data SMALL(1) /1.18E-38     /
data LARGE(1) /3.40E+38     /
data RIGHT(1) /0.595E-07    /
data DIVER(1) /1.19E-07     /
data LOG10(1) /0.30102999566/
if ( I < 1 .OR. I > 5 ) then
! call XERMSG ('SLATEC', 'R1MACH', 'I OUT OF BOUNDS', 1, 2)
end if

RMACH(1)=SMALL(1)
RMACH(2)=LARGE(1)
RMACH(3)=RIGHT(1)
RMACH(4)=DIVER(1)
RMACH(5)=LOG10(1)

R1MACH = RMACH(I)

return
end FUNCTION R1MACH
