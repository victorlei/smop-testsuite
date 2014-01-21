!--/mmtu12.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
PROGRAM MMTU12
!
!     ===============================================================
!     Purpose: This program computes the modified Mathieu functions
!     of the first and second kinds, Mcm(1)(2)(x,q) and
!     Msm(1)(2)(x,q), and their derivatives using
!     subroutine MTU12
!     Input:   KF --- Function code
!     KF=1 for computing Mcm(x,q)
!     KF=2 for computing Msm(x,q)
!     KC --- Function Code
!     KC=1 for computing Mcm(1)(x,q) and Mcm(1)'(x,q)
!     or Msm(1)(x,q) and Msm(1)'(x,q)
!     KC=2 for computing Mcm(2)(x,q) and Mcm(2)'(x,q)
!     or Msm(2)(x,q) and Msm(2)'(x,q)
!     KC=3 for both modified Mathieu functions of the
!     first and second kinds, and their
!     derivatives
!     m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     x  --- Argument of Mathieu functions
!     Output:  F1R --- Mcm(1)(x,q) or Msm(1)(x,q)
!     D1R --- Derivative of Mcm(1)(x,q) or Msm(1)(x,q)
!     F2R --- Mcm(2)(x,q) or Msm(2)(x,q)
!     D2R --- Derivative of Mcm(2)(x,q) or Msm(2)(x,q)
!     ===============================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--MMTU1230
!
!*** Start of declarations rewritten by SPAG
!
! Local variables
!
DOUBLE PRECISION D1R , D2R , F1R , F2R , Q , X
INTEGER KC , KF , M
!
!*** End of declarations rewritten by SPAG
!
WRITE (*,*) 'Please enter KF, m, q and x '
!     READ(*,*)KF,M,Q,X
KF=1
M=1
Q=2
X=3
WRITE (*,10) KF , M , Q , X
 10   FORMAT (1X,'KF =',I2,',  ','m =',I3,',  ','q =',F5.1,',  ','x =', &
            & F5.1)
KC=3
CALL MTU12(KF,KC,M,Q,X,F1R,D1R,F2R,D2R)
WRITE (*,*)
IF (KF==1) THEN
  WRITE (*,*) '   x      Mcm(1)(x,q)    Mcm(1)''(x,q)' ,                &
             &'    Mcm(2)(x,q)     Mcm(2)''(x,q)'
ELSE
  WRITE (*,*) '   x      Msm(1)(x,q)    Msm(1)''(x,q)' ,                &
             &'    Msm(2)(x,q)     Msm(2)''(x,q)'
ENDIF
WRITE (*,*) ' --------------------------------------' ,                 &
           &'-------------------------------'
WRITE (*,20) X , F1R , D1R , F2R , D2R
 20   FORMAT (1X,F5.1,4D16.8)
WRITE (*,*)
WRITE (*,30) F1R*D2R-F2R*D1R , .63661977236758D0
 30   FORMAT (1X,'WRONSKIAN=',E16.8,3X,'should equal   2/PI=',E16.8)
WRITE (*,40)
 40   FORMAT (1X,/1X,'Caution: This check is not accurate if it ',      &
             &'involves',/1X,'         the subtraction of two ',        &
             &'similar numbers')
END PROGRAM MMTU12
!--/mtu12.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE MTU12(KF,KC,M,Q,X,F1R,D1R,F2R,D2R)
!
!     ==============================================================
!     Purpose: Compute modified Mathieu functions of the first and
!     second kinds, Mcm(1)(2)(x,q) and Msm(1)(2)(x,q),
!     and their derivatives
!     Input:   KF --- Function code
!     KF=1 for computing Mcm(x,q)
!     KF=2 for computing Msm(x,q)
!     KC --- Function Code
!     KC=1 for computing the first kind
!     KC=2 for computing the second kind
!     or Msm(2)(x,q) and Msm(2)'(x,q)
!     KC=3 for computing both the first
!     and second kinds
!     m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions ( q ò 0 )
!     x  --- Argument of Mathieu functions
!     Output:  F1R --- Mcm(1)(x,q) or Msm(1)(x,q)
!     D1R --- Derivative of Mcm(1)(x,q) or Msm(1)(x,q)
!     F2R --- Mcm(2)(x,q) or Msm(2)(x,q)
!     D2R --- Derivative of Mcm(2)(x,q) or Msm(2)(x,q)
!     Routines called:
!     (1) CVA2 for computing the characteristic values
!     (2) FCOEF for computing expansion coefficients
!     (3) JYNB for computing Jn(x), Yn(x) and their
!     derivatives
!     ==============================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--MTU12105
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION D1R , D2R , F1R , F2R , Q , X
INTEGER KC , KF , M
!
! Local variables
!
DOUBLE PRECISION A , BJ1(0:251) , BJ2(0:251) , BY1(0:251) , BY2(0:251) ,&
               & C1 , C2 , DJ1(0:251) , DJ2(0:251) , DY1(0:251) ,       &
               & DY2(0:251) , EPS , FG(251) , QM , U1 , U2 , W1 , W2
DOUBLE PRECISION DABS , DEXP , DSQRT
INTEGER IC , K , KD , KM , NM
INTEGER INT
!
!*** End of declarations rewritten by SPAG
!
EPS=1.0D-14
IF (KF==1 .AND. M==2*INT(M/2)) THEN
  KD=1
ENDIF
IF (KF==1 .AND. M/=2*INT(M/2)) THEN
  KD=2
ENDIF
IF (KF==2 .AND. M/=2*INT(M/2)) THEN
  KD=3
ENDIF
IF (KF==2 .AND. M==2*INT(M/2)) THEN
  KD=4
ENDIF
CALL CVA2(KD,M,Q,A)
IF (Q<=1.0D0) THEN
  QM=7.5+56.1*SQRT(Q)-134.7*Q+90.7*SQRT(Q)*Q
ELSE
  QM=17.0+3.1*SQRT(Q)-.126*Q+.0037*SQRT(Q)*Q
ENDIF
KM=INT(QM+0.5*M)
CALL FCOEF(KD,M,Q,A,FG)
IC=INT(M/2)+1
IF (KD==4) THEN
  IC=M/2
ENDIF
C1=DEXP(-X)
C2=DEXP(X)
U1=DSQRT(Q)*C1
U2=DSQRT(Q)*C2
CALL JYNB(KM,U1,NM,BJ1,DJ1,BY1,DY1)
CALL JYNB(KM,U2,NM,BJ2,DJ2,BY2,DY2)
IF (KC/=2) THEN
  F1R=0.0D0
  DO K=1 , KM
     IF (KD==1) THEN
        F1R=F1R+(-1)**(IC+K)*FG(K)*BJ1(K-1)*BJ2(K-1)
     ELSEIF (KD==2 .OR. KD==3) THEN
        F1R=F1R+(-1)**(IC+K)*FG(K)                                      &
          & *(BJ1(K-1)*BJ2(K)+(-1)**KD*BJ1(K)*BJ2(K-1))
     ELSE
        F1R=F1R+(-1)**(IC+K)*FG(K)*(BJ1(K-1)*BJ2(K+1)-BJ1(K+1)*BJ2(K-1))
     ENDIF
     IF (K>=5 .AND. DABS(F1R-W1)<DABS(F1R)*EPS) THEN
        EXIT
     ENDIF
     W1=F1R
  ENDDO
  F1R=F1R/FG(1)
  D1R=0.0D0
  DO K=1 , KM
     IF (KD==1) THEN
        D1R=D1R+(-1)**(IC+K)*FG(K)                                      &
          & *(C2*BJ1(K-1)*DJ2(K-1)-C1*DJ1(K-1)*BJ2(K-1))
     ELSEIF (KD==2 .OR. KD==3) THEN
        D1R=D1R+(-1)**(IC+K)*FG(K)                                      &
          & *(C2*(BJ1(K-1)*DJ2(K)+(-1)**KD*BJ1(K)*DJ2(K-1))             &
          & -C1*(DJ1(K-1)*BJ2(K)+(-1)**KD*DJ1(K)*BJ2(K-1)))
     ELSE
        D1R=D1R+(-1)**(IC+K)*FG(K)                                      &
          & *(C2*(BJ1(K-1)*DJ2(K+1)-BJ1(K+1)*DJ2(K-1))                  &
          & -C1*(DJ1(K-1)*BJ2(K+1)-DJ1(K+1)*BJ2(K-1)))
     ENDIF
     IF (K>=5 .AND. DABS(D1R-W2)<DABS(D1R)*EPS) THEN
        EXIT
     ENDIF
     W2=D1R
  ENDDO
  D1R=D1R*DSQRT(Q)/FG(1)
  IF (KC==1) THEN
     RETURN
  ENDIF
ENDIF
F2R=0.0D0
DO K=1 , KM
  IF (KD==1) THEN
     F2R=F2R+(-1)**(IC+K)*FG(K)*BJ1(K-1)*BY2(K-1)
  ELSEIF (KD==2 .OR. KD==3) THEN
     F2R=F2R+(-1)**(IC+K)*FG(K)                                         &
       & *(BJ1(K-1)*BY2(K)+(-1)**KD*BJ1(K)*BY2(K-1))
  ELSE
     F2R=F2R+(-1)**(IC+K)*FG(K)*(BJ1(K-1)*BY2(K+1)-BJ1(K+1)*BY2(K-1))
  ENDIF
  IF (K>=5 .AND. DABS(F2R-W1)<DABS(F2R)*EPS) THEN
     EXIT
  ENDIF
  W1=F2R
ENDDO
F2R=F2R/FG(1)
D2R=0.0D0
DO K=1 , KM
  IF (KD==1) THEN
     D2R=D2R+(-1)**(IC+K)*FG(K)                                         &
       & *(C2*BJ1(K-1)*DY2(K-1)-C1*DJ1(K-1)*BY2(K-1))
  ELSEIF (KD==2 .OR. KD==3) THEN
     D2R=D2R+(-1)**(IC+K)*FG(K)                                         &
       & *(C2*(BJ1(K-1)*DY2(K)+(-1)**KD*BJ1(K)*DY2(K-1))                &
       & -C1*(DJ1(K-1)*BY2(K)+(-1)**KD*DJ1(K)*BY2(K-1)))
  ELSE
     D2R=D2R+(-1)**(IC+K)*FG(K)                                         &
       & *(C2*(BJ1(K-1)*DY2(K+1)-BJ1(K+1)*DY2(K-1))                     &
       & -C1*(DJ1(K-1)*BY2(K+1)-DJ1(K+1)*BY2(K-1)))
  ENDIF
  IF (K>=5 .AND. DABS(D2R-W2)<DABS(D2R)*EPS) THEN
     EXIT
  ENDIF
  W2=D2R
ENDDO
D2R=D2R*DSQRT(Q)/FG(1)
END SUBROUTINE MTU12
!--/fcoef.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE FCOEF(KD,M,Q,A,FC)
!
!     =====================================================
!     Purpose: Compute expansion coefficients for Mathieu
!     functions and modified Mathieu functions
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     A  --- Characteristic value of Mathieu
!     functions for given m and q
!     Output:  FC(k) --- Expansion coefficients of Mathieu
!     functions ( k= 1,2,...,KM )
!     FC(1),FC(2),FC(3),... correspond to
!     A0,A2,A4,... for KD=1 case, A1,A3,
!     A5,... for KD=2 case, B1,B3,B5,...
!     for KD=3 case and B2,B4,B6,... for
!     KD=4 case
!     =====================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--FCOEF261
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A , Q
INTEGER KD , M
DOUBLE PRECISION FC(251)
!
! Local variables
!
DOUBLE PRECISION DABS , DSQRT
DOUBLE PRECISION F , F1 , F2 , F3 , QM , S , S0 , SP , SS , U , V
INTEGER I , IFOO , IFOO2 , J , K , KB , KM
INTEGER INT
!
!*** End of declarations rewritten by SPAG
!
DO I=1 , 251
  FC(I)=0.0D0
ENDDO
IF (Q<=1.0D0) THEN
  QM=7.5+56.1*SQRT(Q)-134.7*Q+90.7*SQRT(Q)*Q
ELSE
  QM=17.0+3.1*SQRT(Q)-.126*Q+.0037*SQRT(Q)*Q
ENDIF
KM=INT(QM+0.5*M)
DO K=1 , KM+1
  FC(K)=0.0D0
ENDDO
IF (Q==0.0D0) THEN
  IF (KD==1) THEN
     FC((M+2)/2)=1.0D0
     IF (M==0) THEN
        FC(1)=1.0D0/DSQRT(2.0D0)
     ENDIF
  ELSEIF (KD==4) THEN
     FC(M/2)=1.0D0
  ELSE
     FC((M+1)/2)=1.0D0
  ENDIF
  RETURN
ENDIF
KB=0
S=0.0D0
F=1.0D-100
U=0.0D0
FC(KM)=0.0D0
ifoo=1
DO WHILE (ifoo==1)
  IF (KD==1) THEN
     DO K=KM , 3 , -1
        V=U
        U=F
        F=(A-4.0D0*K*K)*U/Q-V
        IF (DABS(F)<DABS(FC(K+1))) THEN
           KB=K
           FC(1)=1.0D-100
           SP=0.0D0
           F3=FC(K+1)
           FC(2)=A/Q*FC(1)
           FC(3)=(A-4.0D0)*FC(2)/Q-2.0D0*FC(1)
           U=FC(2)
           F1=FC(3)
           DO I=3 , KB
              V=U
              U=F1
              F1=(A-4.0D0*(I-1.0D0)**2)*U/Q-V
              FC(I+1)=F1
              IF (I==KB) THEN
                 F2=F1
              ENDIF
              IF (I/=KB) THEN
                 SP=SP+F1*F1
              ENDIF
           ENDDO
           SP=SP+2.0D0*FC(1)**2+FC(2)**2+FC(3)**2
           SS=S+SP*(F3/F2)**2
           S0=DSQRT(1.0D0/SS)
           DO J=1 , KM
              IF (J<=KB+1) THEN
                 FC(J)=S0*FC(J)*F3/F2
              ELSE
                 FC(J)=S0*FC(J)
              ENDIF
           ENDDO
           ifoo=0
           EXIT                 !GO TO 85
        ELSE
           FC(K)=F
           S=S+F*F
        ENDIF
     ENDDO
     IF (ifoo==0) THEN
        EXIT
     ENDIF
     FC(2)=Q*FC(3)/(A-4.0D0-2.0D0*Q*Q/A)
     FC(1)=Q/A*FC(2)
     S=S+2.0D0*FC(1)**2+FC(2)**2
     S0=DSQRT(1.0D0/S)
     DO K=1 , KM
        FC(K)=S0*FC(K)
     ENDDO
  ELSEIF (KD==2 .OR. KD==3) THEN
     ifoo2=1
     DO K=KM , 3 , -1
        V=U
        U=F
        F=(A-(2.0D0*K-1)**2)*U/Q-V
        IF (DABS(F)>=DABS(FC(K))) THEN
           FC(K-1)=F
           S=S+F*F
        ELSE
           KB=K
           F3=FC(K)
           ifoo2=0
           EXIT
        ENDIF
     ENDDO
     IF (ifoo2==1) THEN
        FC(1)=Q/(A-1.0D0-(-1)**KD*Q)*FC(2)
        S=S+FC(1)*FC(1)
        S0=DSQRT(1.0D0/S)
        DO K=1 , KM
           FC(K)=S0*FC(K)
        ENDDO
        EXIT                    !GO TO 85
     ENDIF
     FC(1)=1.0D-100
     FC(2)=(A-1.0D0-(-1)**KD*Q)/Q*FC(1)
     SP=0.0D0
     U=FC(1)
     F1=FC(2)
     DO I=2 , KB-1
        V=U
        U=F1
        F1=(A-(2.0D0*I-1.0D0)**2)*U/Q-V
        IF (I/=KB-1) THEN
           FC(I+1)=F1
           SP=SP+F1*F1
        ELSE
           F2=F1
        ENDIF
     ENDDO
     SP=SP+FC(1)**2+FC(2)**2
     SS=S+SP*(F3/F2)**2
     S0=1.0D0/DSQRT(SS)
     DO J=1 , KM
        IF (J<KB) THEN
           FC(J)=S0*FC(J)*F3/F2
        ENDIF
        IF (J>=KB) THEN
           FC(J)=S0*FC(J)
        ENDIF
     ENDDO
  ELSEIF (KD==4) THEN
     ifoo2=1
     DO K=KM , 3 , -1
        V=U
        U=F
        F=(A-4.0D0*K*K)*U/Q-V
        IF (DABS(F)>=DABS(FC(K))) THEN
           FC(K-1)=F
           S=S+F*F
        ELSE
           KB=K
           F3=FC(K)
           ifoo2=0
           EXIT
        ENDIF
     ENDDO
     IF (ifoo2==1) THEN
        FC(1)=Q/(A-4.0D0)*FC(2)
        S=S+FC(1)*FC(1)
        S0=DSQRT(1.0D0/S)
        DO K=1 , KM
           FC(K)=S0*FC(K)
        ENDDO
        EXIT                    !GO TO 85
     ENDIF
     FC(1)=1.0D-100
     FC(2)=(A-4.0D0)/Q*FC(1)
     SP=0.0D0
     U=FC(1)
     F1=FC(2)
     DO I=2 , KB-1
        V=U
        U=F1
        F1=(A-4.0D0*I*I)*U/Q-V
        IF (I/=KB-1) THEN
           FC(I+1)=F1
           SP=SP+F1*F1
        ELSE
           F2=F1
        ENDIF
     ENDDO
     SP=SP+FC(1)**2+FC(2)**2
     SS=S+SP*(F3/F2)**2
     S0=1.0D0/DSQRT(SS)
     DO J=1 , KM
        IF (J<KB) THEN
           FC(J)=S0*FC(J)*F3/F2
        ENDIF
        IF (J>=KB) THEN
           FC(J)=S0*FC(J)
        ENDIF
     ENDDO
  ENDIF
  ifoo=0
ENDDO
IF (FC(1)<0.0D0) THEN
  DO J=1 , KM
     FC(J)=-FC(J)
  ENDDO
ENDIF
END SUBROUTINE FCOEF
!--/cva2.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE CVA2(KD,M,Q,A)
!
!     ======================================================
!     Purpose: Calculate a specific characteristic value of
!     Mathieu functions
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     KD --- Case code
!     KD=1 for cem(x,q)  ( m = 0,2,4,...)
!     KD=2 for cem(x,q)  ( m = 1,3,5,...)
!     KD=3 for sem(x,q)  ( m = 1,3,5,...)
!     KD=4 for sem(x,q)  ( m = 2,4,6,...)
!     Output:  A  --- Characteristic value
!     Routines called:
!     (1) REFINE for finding accurate characteristic
!     value using an iteration method
!     (2) CV0 for finding initial characteristic
!     values using polynomial approximation
!     (3) CVQM for computing initial characteristic
!     values for q ó 3*m
!     (3) CVQL for computing initial characteristic
!     values for q ò m*m
!     ======================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--CVA2506
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A , Q
INTEGER KD , M
!
! Local variables
!
DOUBLE PRECISION A1 , A2 , DELTA , Q1 , Q2 , QQ
INTEGER I , IFLAG , NDIV , NN
INTEGER INT
!
!*** End of declarations rewritten by SPAG
!
IF (M<=12 .OR. Q<=3.0*M .OR. Q>M*M) THEN
  CALL CV0(KD,M,Q,A)
  IF (Q/=0.0D0) THEN
     CALL REFINE(KD,M,Q,A,1)
  ENDIF
ELSE
  NDIV=10
  DELTA=(M-3.0)*M/NDIV
  IF ((Q-3.0*M)<=(M*M-Q)) THEN
     DO
        NN=INT((Q-3.0*M)/DELTA)+1
        DELTA=(Q-3.0*M)/NN
        Q1=2.0*M
        CALL CVQM(M,Q1,A1)
        Q2=3.0*M
        CALL CVQM(M,Q2,A2)
        QQ=3.0*M
        DO I=1 , NN
           QQ=QQ+DELTA
           A=(A1*Q2-A2*Q1+(A2-A1)*QQ)/(Q2-Q1)
           IFLAG=1
           IF (I==NN) THEN
              IFLAG=-1
           ENDIF
           CALL REFINE(KD,M,QQ,A,IFLAG)
           Q1=Q2
           Q2=QQ
           A1=A2
           A2=A
        ENDDO
        IF (IFLAG==-10) THEN
           NDIV=NDIV*2
           DELTA=(M-3.0)*M/NDIV
           EXIT
        ENDIF
     ENDDO
  ELSE
     DO
        NN=INT((M*M-Q)/DELTA)+1
        DELTA=(M*M-Q)/NN
        Q1=M*(M-1.0)
        CALL CVQL(KD,M,Q1,A1)
        Q2=M*M
        CALL CVQL(KD,M,Q2,A2)
        QQ=M*M
        DO I=1 , NN
           QQ=QQ-DELTA
           A=(A1*Q2-A2*Q1+(A2-A1)*QQ)/(Q2-Q1)
           IFLAG=1
           IF (I==NN) THEN
              IFLAG=-1
           ENDIF
           CALL REFINE(KD,M,QQ,A,IFLAG)
           Q1=Q2
           Q2=QQ
           A1=A2
           A2=A
        ENDDO
        IF (IFLAG==-10) THEN
           NDIV=NDIV*2
           DELTA=(M-3.0)*M/NDIV
           EXIT
        ENDIF
     ENDDO
  ENDIF
ENDIF
END SUBROUTINE CVA2
!--/refine.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE REFINE(KD,M,Q,A,IFLAG)
!
!     =====================================================
!     Purpose: calculate the accurate characteristic value
!     by the secant method
!     Input :  m --- Order of Mathieu functions
!     q --- Parameter of Mathieu functions
!     A --- Initial characteristic value
!     Output:  A --- Refineed characteristic value
!     Routine called:  CVF for computing the value of F for
!     characteristic equation
!     ========================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--REFINE607
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A , Q
INTEGER IFLAG , KD , M
!
! Local variables
!
DOUBLE PRECISION EPS , F , F0 , F1 , X , X0 , X1
INTEGER IT , MJ
!
!*** End of declarations rewritten by SPAG
!
EPS=1.0D-14
MJ=10+M
X0=A
CALL CVF(KD,M,Q,X0,MJ,F0)
X1=1.002*A
CALL CVF(KD,M,Q,X1,MJ,F1)
DO IT=1 , 100
  MJ=MJ+1
  X=X1-(X1-X0)/(1.0D0-F0/F1)
  CALL CVF(KD,M,Q,X,MJ,F)
  IF (ABS(1.0-X1/X)<EPS .OR. F==0.0) THEN
     EXIT
  ENDIF
  X0=X1
  F0=F1
  X1=X
  F1=F
ENDDO
A=X
END SUBROUTINE REFINE
!--/cvf.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE CVF(KD,M,Q,A,MJ,F)
!
!     ======================================================
!     Purpose: Compute the value of F for characteristic
!     equation of Mathieu functions
!     Input :  m --- Order of Mathieu functions
!     q --- Parameter of Mathieu functions
!     A --- Characteristic value
!     Output:  F --- Value of F for characteristic equation
!     ======================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--CVF658
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A , F , Q
INTEGER KD , M , MJ
!
! Local variables
!
DOUBLE PRECISION B , T0 , T1 , T2
INTEGER IC , J , J0 , JF , L , L0
INTEGER INT
!
!*** End of declarations rewritten by SPAG
!
B=A
IC=INT(M/2)
L=0
L0=0
J0=2
JF=IC
IF (KD==1) THEN
  L0=2
ENDIF
IF (KD==1) THEN
  J0=3
ENDIF
IF (KD==2 .OR. KD==3) THEN
  L=1
ENDIF
IF (KD==4) THEN
  JF=IC-1
ENDIF
T1=0.0D0
DO J=MJ , IC+1 , -1
  T1=-Q*Q/((2.0D0*J+L)**2-B+T1)
ENDDO
IF (M<=2) THEN
  T2=0.0D0
  IF (KD==1 .AND. M==0) THEN
     T1=T1+T1
  ENDIF
  IF (KD==1 .AND. M==2) THEN
     T1=-2.0*Q*Q/(4.0-B+T1)-4.0
  ENDIF
  IF (KD==2 .AND. M==1) THEN
     T1=T1+Q
  ENDIF
  IF (KD==3 .AND. M==1) THEN
     T1=T1-Q
  ENDIF
ELSE
  IF (KD==1) THEN
     T0=4.0D0-B+2.0D0*Q*Q/B
  ENDIF
  IF (KD==2) THEN
     T0=1.0D0-B+Q
  ENDIF
  IF (KD==3) THEN
     T0=1.0D0-B-Q
  ENDIF
  IF (KD==4) THEN
     T0=4.0D0-B
  ENDIF
  T2=-Q*Q/T0
  DO J=J0 , JF
     T2=-Q*Q/((2.0D0*J-L-L0)**2-B+T2)
  ENDDO
ENDIF
F=(2.0D0*IC+L)**2+T1+T2-B
END SUBROUTINE CVF
!--/cv0.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE CV0(KD,M,Q,A0)
!
!     =====================================================
!     Purpose: Compute the initial characteristic value of
!     Mathieu functions for m ó 12  or q ó 300 or
!     q ò m*m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Characteristic value
!     Routines called:
!     (1) CVQM for computing initial characteristic
!     value for q ó 3*m
!     (2) CVQL for computing initial characteristic
!     value for q ò m*m
!     ====================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--CV0751
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A0 , Q
INTEGER KD , M
!
! Local variables
!
DOUBLE PRECISION Q2
!
!*** End of declarations rewritten by SPAG
!
Q2=Q*Q
IF (M==0) THEN
  IF (Q<=1.0) THEN
     A0=(((.0036392*Q2-.0125868)*Q2+.0546875)*Q2-.5)*Q2
  ELSEIF (Q<=10.0) THEN
     A0=((3.999267D-3*Q-9.638957D-2)*Q-.88297)*Q+.5542818
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==1) THEN
  IF (Q<=1.0 .AND. KD==2) THEN
     A0=(((-6.51E-4*Q-.015625)*Q-.125)*Q+1.0)*Q+1.0
  ELSEIF (Q<=1.0 .AND. KD==3) THEN
     A0=(((-6.51E-4*Q+.015625)*Q-.125)*Q-1.0)*Q+1.0
  ELSEIF (Q<=10.0 .AND. KD==2) THEN
     A0=(((-4.94603D-4*Q+1.92917D-2)*Q-.3089229)*Q+1.33372)*Q+.811752
  ELSEIF (Q<=10.0 .AND. KD==3) THEN
     A0=((1.971096D-3*Q-5.482465D-2)*Q-1.152218)*Q+1.10427
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==2) THEN
  IF (Q<=1.0 .AND. KD==1) THEN
     A0=(((-.0036391*Q2+.0125888)*Q2-.0551939)*Q2+.416667)*Q2+4.0
  ELSEIF (Q<=1.0 .AND. KD==4) THEN
     A0=(.0003617*Q2-.0833333)*Q2+4.0
  ELSEIF (Q<=15 .AND. KD==1) THEN
     A0=(((3.200972D-4*Q-8.667445D-3)*Q-1.829032D-4)*Q+.9919999)        &
      & *Q+3.3290504
  ELSEIF (Q<=10.0 .AND. KD==4) THEN
     A0=((2.38446D-3*Q-.08725329)*Q-4.732542D-3)*Q+4.00909
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==3) THEN
  IF (Q<=1.0 .AND. KD==2) THEN
     A0=((6.348E-4*Q+.015625)*Q+.0625)*Q2+9.0
  ELSEIF (Q<=1.0 .AND. KD==3) THEN
     A0=((6.348E-4*Q-.015625)*Q+.0625)*Q2+9.0
  ELSEIF (Q<=20.0 .AND. KD==2) THEN
     A0=(((3.035731D-4*Q-1.453021D-2)*Q+.19069602)*Q-.1039356)          &
      & *Q+8.9449274
  ELSEIF (Q<=15.0 .AND. KD==3) THEN
     A0=((9.369364D-5*Q-.03569325)*Q+.2689874)*Q+8.771735
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==4) THEN
  IF (Q<=1.0 .AND. KD==1) THEN
     A0=((-2.1E-6*Q2+5.012E-4)*Q2+.0333333)*Q2+16.0
  ELSEIF (Q<=1.0 .AND. KD==4) THEN
     A0=((3.7E-6*Q2-3.669E-4)*Q2+.0333333)*Q2+16.0
  ELSEIF (Q<=25.0 .AND. KD==1) THEN
     A0=(((1.076676D-4*Q-7.9684875D-3)*Q+.17344854)*Q-.5924058)         &
      & *Q+16.620847
  ELSEIF (Q<=20.0 .AND. KD==4) THEN
     A0=((-7.08719D-4*Q+3.8216144D-3)*Q+.1907493)*Q+15.744
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==5) THEN
  IF (Q<=1.0 .AND. KD==2) THEN
     A0=((6.8E-6*Q+1.42E-5)*Q2+.0208333)*Q2+25.0
  ELSEIF (Q<=1.0 .AND. KD==3) THEN
     A0=((-6.8E-6*Q+1.42E-5)*Q2+.0208333)*Q2+25.0
  ELSEIF (Q<=35.0 .AND. KD==2) THEN
     A0=(((2.238231D-5*Q-2.983416D-3)*Q+.10706975)*Q-.600205)*Q+25.93515
  ELSEIF (Q<=25.0 .AND. KD==3) THEN
     A0=((-7.425364D-4*Q+2.18225D-2)*Q+4.16399D-2)*Q+24.897
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==6) THEN
  IF (Q<=1.0) THEN
     A0=(.4D-6*Q2+.0142857)*Q2+36.0
  ELSEIF (Q<=40.0 .AND. KD==1) THEN
     A0=(((-1.66846D-5*Q+4.80263D-4)*Q+2.53998D-2)*Q-.181233)*Q+36.423
  ELSEIF (Q<=35.0 .AND. KD==4) THEN
     A0=((-4.57146D-4*Q+2.16609D-2)*Q-2.349616D-2)*Q+35.99251
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M==7) THEN
  IF (Q<=10.0) THEN
     CALL CVQM(M,Q,A0)
  ELSEIF (Q<=50.0 .AND. KD==2) THEN
     A0=(((-1.411114D-5*Q+9.730514D-4)*Q-3.097887D-3)*Q+3.533597D-2)    &
      & *Q+49.0547
  ELSEIF (Q<=40.0 .AND. KD==3) THEN
     A0=((-3.043872D-4*Q+2.05511D-2)*Q-9.16292D-2)*Q+49.19035
  ELSE
     CALL CVQL(KD,M,Q,A0)
  ENDIF
ELSEIF (M>=8) THEN
  IF (Q<=3.*M) THEN
     CALL CVQM(M,Q,A0)
  ELSEIF (Q>M*M) THEN
     CALL CVQL(KD,M,Q,A0)
  ELSEIF (M==8 .AND. KD==1) THEN
     A0=(((8.634308D-6*Q-2.100289D-3)*Q+.169072)*Q-4.64336)*Q+109.4211
  ELSEIF (M==8 .AND. KD==4) THEN
     A0=((-6.7842D-5*Q+2.2057D-3)*Q+.48296)*Q+56.59
  ELSEIF (M==9 .AND. KD==2) THEN
     A0=(((2.906435D-6*Q-1.019893D-3)*Q+.1101965)*Q-3.821851)*Q+127.6098
  ELSEIF (M==9 .AND. KD==3) THEN
     A0=((-9.577289D-5*Q+.01043839)*Q+.06588934)*Q+78.0198
  ELSEIF (M==10 .AND. KD==1) THEN
     A0=(((5.44927D-7*Q-3.926119D-4)*Q+.0612099)*Q-2.600805)*Q+138.1923
  ELSEIF (M==10 .AND. KD==4) THEN
     A0=((-7.660143D-5*Q+.01132506)*Q-.09746023)*Q+99.29494
  ELSEIF (M==11 .AND. KD==2) THEN
     A0=(((-5.67615D-7*Q+7.152722D-6)*Q+.01920291)*Q-1.081583)*Q+140.88
  ELSEIF (M==11 .AND. KD==3) THEN
     A0=((-6.310551D-5*Q+.0119247)*Q-.2681195)*Q+123.667
  ELSEIF (M==12 .AND. KD==1) THEN
     A0=(((-2.38351D-7*Q-2.90139D-5)*Q+.02023088)*Q-1.289)*Q+171.2723
  ELSEIF (M==12 .AND. KD==4) THEN
     A0=(((3.08902D-7*Q-1.577869D-4)*Q+.0247911)*Q-1.05454)*Q+161.471
  ENDIF
ENDIF
END SUBROUTINE CV0
!--/cvql.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE CVQL(KD,M,Q,A0)
!
!     ========================================================
!     Purpose: Compute the characteristic value of Mathieu
!     functions  for q ò 3m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Initial characteristic value
!     ========================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--CVQL901
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A0 , Q
INTEGER KD , M
!
! Local variables
!
DOUBLE PRECISION C1 , CV1 , CV2 , D1 , D2 , D3 , D4 , P1 , P2 , W , W2 ,&
               & W3 , W4 , W6
DOUBLE PRECISION DSQRT
!
!*** End of declarations rewritten by SPAG
!
IF (KD==1 .OR. KD==2) THEN
  W=2.0D0*M+1.0D0
ENDIF
IF (KD==3 .OR. KD==4) THEN
  W=2.0D0*M-1.0D0
ENDIF
W2=W*W
W3=W*W2
W4=W2*W2
W6=W2*W4
D1=5.0+34.0/W2+9.0/W4
D2=(33.0+410.0/W2+405.0/W4)/W
D3=(63.0+1260.0/W2+2943.0/W4+486.0/W6)/W2
D4=(527.0+15617.0/W2+69001.0/W4+41607.0/W6)/W3
C1=128.0
P2=Q/W4
P1=DSQRT(P2)
CV1=-2.0*Q+2.0*W*DSQRT(Q)-(W2+1.0)/8.0
CV2=(W+3.0/W)+D1/(32.0*P1)+D2/(8.0*C1*P2)
CV2=CV2+D3/(64.0*C1*P1*P2)+D4/(16.0*C1*C1*P2*P2)
A0=CV1-CV2/(C1*P1)
END SUBROUTINE CVQL
!--/cvqm.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE CVQM(M,Q,A0)
!
!     =====================================================
!     Purpose: Compute the characteristic value of Mathieu
!     functions for q ó m*m
!     Input :  m  --- Order of Mathieu functions
!     q  --- Parameter of Mathieu functions
!     Output:  A0 --- Initial characteristic value
!     =====================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--CVQM954
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
DOUBLE PRECISION A0 , Q
INTEGER M
!
! Local variables
!
DOUBLE PRECISION HM1 , HM3 , HM5
!
!*** End of declarations rewritten by SPAG
!
HM1=.5*Q/(M*M-1.0)
HM3=.25*HM1**3/(M*M-4.0)
HM5=HM1*HM3*Q/((M*M-1.0)*(M*M-9.0))
A0=M*M+Q*(HM1+(5.0*M*M+7.0)*HM3+(9.0*M**4+58.0*M*M+29.0)*HM5)
END SUBROUTINE CVQM
!--/jynb.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
SUBROUTINE JYNB(N,X,NM,BJ,DJ,BY,DY)
!
!     =====================================================
!     Purpose: Compute Bessel functions Jn(x), Yn(x) and
!     their derivatives
!     Input :  x --- Argument of Jn(x) and Yn(x) ( x ò 0 )
!     n --- Order of Jn(x) and Yn(x)
!     Output:  BJ(n) --- Jn(x)
!     DJ(n) --- Jn'(x)
!     BY(n) --- Yn(x)
!     DY(n) --- Yn'(x)
!     NM --- Highest order computed
!     Routines called:
!     MSTA1 and MSTA2 to calculate the starting
!     point for backward recurrence
!     =====================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--JYNB995
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
INTEGER N , NM
DOUBLE PRECISION X
DOUBLE PRECISION BJ(0:N) , BY(0:N) , DJ(0:N) , DY(0:N)
!
! Local variables
!
DOUBLE PRECISION A(4) , A1(4) , B(4) , B1(4) , BJ0 , BJ1 , BJK , BS ,   &
               & BY0 , BY1 , BYK , CU , EC , F , F1 , F2 , P0 , P1 ,    &
               & PI , Q0 , Q1 , R2P , S0 , SU , SV , T1 , T2
DOUBLE PRECISION DCOS , DLOG , DSIN , DSQRT
INTEGER INT
INTEGER K , M
INTEGER MSTA1 , MSTA2
!
!*** End of declarations rewritten by SPAG
!
PI=3.141592653589793D0
R2P=.63661977236758D0
NM=N
IF (X<1.0D-100) THEN
  DO K=0 , N
     BJ(K)=0.0D0
     DJ(K)=0.0D0
     BY(K)=-1.0D+300
     DY(K)=1.0D+300
  ENDDO
  BJ(0)=1.0D0
  DJ(1)=0.5D0
  RETURN
ENDIF
IF (X<=300.0 .OR. N>INT(0.9*X)) THEN
  IF (N==0) THEN
     NM=1
  ENDIF
  M=MSTA1(X,200)
  IF (M<NM) THEN
     NM=M
  ELSE
     M=MSTA2(X,NM,15)
  ENDIF
  BS=0.0D0
  SU=0.0D0
  SV=0.0D0
  F2=0.0D0
  F1=1.0D-100
  DO K=M , 0 , -1
     F=2.0D0*(K+1.0D0)/X*F1-F2
     IF (K<=NM) THEN
        BJ(K)=F
     ENDIF
     IF (K==2*INT(K/2) .AND. K/=0) THEN
        BS=BS+2.0D0*F
        SU=SU+(-1)**(K/2)*F/K
     ELSEIF (K>1) THEN
        SV=SV+(-1)**(int(K/2))*K/(K*K-1.0)*F
     ENDIF
     F2=F1
     F1=F
  ENDDO
  S0=BS+F
  DO K=0 , NM
     BJ(K)=BJ(K)/S0
  ENDDO
  EC=DLOG(X/2.0D0)+0.5772156649015329D0
  BY0=R2P*(EC*BJ(0)-4.0D0*SU/S0)
  BY(0)=BY0
  BY1=R2P*((EC-1.0D0)*BJ(1)-BJ(0)/X-4.0D0*SV/S0)
  BY(1)=BY1
ELSE
  DATA A/-.7031250000000000D-01 , .1121520996093750D+00 ,               &
     & -.5725014209747314D+00 , .6074042001273483D+01/
  DATA B/.7324218750000000D-01 , -.2271080017089844D+00 ,               &
     & .1727727502584457D+01 , -.2438052969955606D+02/
  DATA A1/.1171875000000000D+00 , -.1441955566406250D+00 ,              &
     & .6765925884246826D+00 , -.6883914268109947D+01/
  DATA B1/-.1025390625000000D+00 , .2775764465332031D+00 ,              &
     & -.1993531733751297D+01 , .2724882731126854D+02/
  T1=X-0.25D0*PI
  P0=1.0D0
  Q0=-0.125D0/X
  DO K=1 , 4
     P0=P0+A(K)*X**(-2*K)
     Q0=Q0+B(K)*X**(-2*K-1)
  ENDDO
  CU=DSQRT(R2P/X)
  BJ0=CU*(P0*DCOS(T1)-Q0*DSIN(T1))
  BY0=CU*(P0*DSIN(T1)+Q0*DCOS(T1))
  BJ(0)=BJ0
  BY(0)=BY0
  T2=X-0.75D0*PI
  P1=1.0D0
  Q1=0.375D0/X
  DO K=1 , 4
     P1=P1+A1(K)*X**(-2*K)
     Q1=Q1+B1(K)*X**(-2*K-1)
  ENDDO
  BJ1=CU*(P1*DCOS(T2)-Q1*DSIN(T2))
  BY1=CU*(P1*DSIN(T2)+Q1*DCOS(T2))
  BJ(1)=BJ1
  BY(1)=BY1
  DO K=2 , NM
     BJK=2.0D0*(K-1.0D0)/X*BJ1-BJ0
     BJ(K)=BJK
     BJ0=BJ1
     BJ1=BJK
  ENDDO
ENDIF
DJ(0)=-BJ(1)
DO K=1 , NM
  DJ(K)=BJ(K-1)-K/X*BJ(K)
ENDDO
DO K=2 , NM
  BYK=2.0D0*(K-1.0D0)*BY1/X-BY0
  BY(K)=BYK
  BY0=BY1
  BY1=BYK
ENDDO
DY(0)=-BY(1)
DO K=1 , NM
  DY(K)=BY(K-1)-K*BY(K)/X
ENDDO
END SUBROUTINE JYNB
!--/msta1.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
INTEGER FUNCTION MSTA1(X,MP)
!
!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that the magnitude of
!     Jn(x) at that point is about 10^(-MP)
!     Input :  x     --- Argument of Jn(x)
!     MP    --- Value of magnitude
!     Output:  MSTA1 --- Starting point
!     ===================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--MSTA11138
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
INTEGER MP
DOUBLE PRECISION X
!
! Local variables
!
DOUBLE PRECISION A0 , F , F0 , F1
DOUBLE PRECISION DABS
DOUBLE PRECISION ENVJ
INTEGER INT
INTEGER IT , N0 , N1 , NN
!
!*** End of declarations rewritten by SPAG
!
A0=DABS(X)
N0=INT(1.1*A0)+1
F0=ENVJ(N0,A0)-MP
N1=N0+5
F1=ENVJ(N1,A0)-MP
DO IT=1 , 20
  NN=N1-(N1-N0)/(1.0D0-F0/F1)
  F=ENVJ(NN,A0)-MP
  IF (ABS(NN-N1)<1) THEN
     EXIT
  ENDIF
  N0=N1
  F0=F1
  N1=NN
  F1=F
ENDDO
MSTA1=int(NN)
END FUNCTION MSTA1
!--/msta2.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
!
INTEGER FUNCTION MSTA2(X,N,MP)
!
!     ===================================================
!     Purpose: Determine the starting point for backward
!     recurrence such that all Jn(x) has MP
!     significant digits
!     Input :  x  --- Argument of Jn(x)
!     n  --- Order of Jn(x)
!     MP --- Significant digit
!     Output:  MSTA2 --- Starting point
!     ===================================================
!
IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!*--MSTA21191
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
INTEGER MP , N
DOUBLE PRECISION X
!
! Local variables
!
DOUBLE PRECISION A0 , EJN , F , F0 , F1 , HMP , OBJ
DOUBLE PRECISION DABS
DOUBLE PRECISION ENVJ
INTEGER INT
INTEGER IT , N0 , N1 , NN
!
!*** End of declarations rewritten by SPAG
!
A0=DABS(X)
HMP=0.5D0*MP
EJN=ENVJ(N,A0)
IF (EJN<=HMP) THEN
  OBJ=MP
  N0=INT(1.1*A0)
ELSE
  OBJ=HMP+EJN
  N0=N
ENDIF
F0=ENVJ(N0,A0)-OBJ
N1=N0+5
F1=ENVJ(N1,A0)-OBJ
DO IT=1 , 20
  NN=N1-(N1-N0)/(1.0D0-F0/F1)
  F=ENVJ(NN,A0)-OBJ
  IF (ABS(NN-N1)<1) THEN
     EXIT
  ENDIF
  N0=N1
  F0=F1
  N1=NN
  F1=F
ENDDO
MSTA2=int(NN+10)
END FUNCTION MSTA2
!--/envj.f90  processed by SPAG 6.53Rc at 13:11 on 28 Jun 2004
!
REAL*8 FUNCTION ENVJ(N,X)
!*--ENVJ1239
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
INTEGER N
DOUBLE PRECISION X
!
! Local variables
!
DOUBLE PRECISION DLOG10
!
!*** End of declarations rewritten by SPAG
!
ENVJ=0.5D0*DLOG10(6.28D0*N)-N*DLOG10(1.36D0*X/N)
END FUNCTION ENVJ
