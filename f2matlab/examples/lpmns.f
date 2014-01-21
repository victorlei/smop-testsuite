       SUBROUTINE LPMNS(M,N,X,PM,PD)
c**************************************************************
C
C       ========================================================
C       Purpose: Compute associated Legendre functions Pmn(x)
C                and Pmn'(x) for a given order
C       Input :  x --- Argument of Pmn(x)
C                m --- Order of Pmn(x),  m = 0,1,2,...,n
C                n --- Degree of Pmn(x), n = 0,1,2,...,N
C       Output:  PM(n) --- Pmn(x)
C                PD(n) --- Pmn'(x)
C       ========================================================
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION PM(0:N),PD(0:N)
        DO 10 K=0,N
           PM(K)=0.0D0
10         PD(K)=0.0D0
        IF (DABS(X).EQ.1.0D0) THEN
           DO 15 K=0,N
              IF (M.EQ.0) THEN
                 PM(K)=1.0D0
                 PD(K)=0.5D0*K*(K+1.0)
                 IF (X.LT.0.0) THEN
                    PM(K)=(-1)**K*PM(K)
                    PD(K)=(-1)**(K+1)*PD(K)
                 ENDIF
              ELSE IF (M.EQ.1) THEN
                 PD(K)=1.0D+300
              ELSE IF (M.EQ.2) THEN
                 PD(K)=-0.25D0*(K+2.0)*(K+1.0)*K*(K-1.0)
                 IF (X.LT.0.0) PD(K)=(-1)**(K+1)*PD(K)
              ENDIF
15         CONTINUE
           RETURN
        ENDIF
        X0=DABS(1.0D0-X*X)
        PM0=1.0D0
        PMK=PM0
        DO 20 K=1,M
           PMK=(2.0D0*K-1.0D0)*DSQRT(X0)*PM0
20         PM0=PMK
        PM1=(2.0D0*M+1.0D0)*X*PM0
        PM(M)=PMK
        PM(M+1)=PM1
        DO 25 K=M+2,N
           PM2=((2.0D0*K-1.0D0)*X*PM1-(K+M-1.0D0)*PMK)/(K-M)
           PM(K)=PM2
           PMK=PM1
25         PM1=PM2
        PD(0)=((1.0D0-M)*PM(1)-X*PM(0))/(X*X-1.0)  
        DO 30 K=1,N
30          PD(K)=(K*X*PM(K)-(K+M)*PM(K-1))/(X*X-1.0D0)
        RETURN
        END
