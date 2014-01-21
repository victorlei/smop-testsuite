      subroutine mexfunction(nlhs, plhs, nrhs, prhs)
c--------------------------------------------------------------------
       interface
        function mxGetPr(pm)
        integer, pointer :: mxGetPr
        integer :: pm
        end function mxGetPr
       end interface
       interface
        function mxGetPi(pm)
        integer, pointer :: mxGetPi
        integer :: pm
        end function mxGetPi
       end interface
c     ---------------------------------------------------------------
      integer*8 plhs(*), prhs(*)
      integer nlhs, nrhs      
c--------------------------------------------------------------------
c     Create pointers for calling the computational subroutine.
c     inputs
      integer, pointer :: m,n,x,outsize
c     outputs
      integer, pointer :: pm,pd
c     Any other variables needed
      integer m_m,m_n,n_m,n_n,x_m,x_n,outsize_m,outsize_n
c---------------------------------------------------------------------
C     CHECK FOR PROPER NUMBER OF ARGUMENTS
      if (nrhs .ne. 4) then
        call mexerrmsgtxt('lpmns2 requires 4 input arguments')
      elseif (nlhs .ne. 2) then
        call mexerrmsgtxt('lpmns2 requires 2 output arguments')
      endif
c---------------------------------------------------------------------
c     Get the sizes of all the input variables
      m_m=mxGetm(prhs(1));m_n=mxGetn(prhs(1))
      n_m=mxGetm(prhs(2));n_n=mxGetn(prhs(2))
      x_m=mxGetm(prhs(3));x_n=mxGetn(prhs(3))
      outsize_m=mxGetm(prhs(4));outsize_n=mxGetn(prhs(4))
C     Create matrices for the return argument
      plhs(1)=mxCreateFull(outsize_m,outsize_n,0)
      pm=>mxGetPr(plhs(1))
      plhs(2)=mxCreateFull(outsize_m,outsize_n,0)
      pd=>mxGetPr(plhs(2))
C     COPY RIGHT HAND ARGUMENTS TO LOCAL ARRAYS
      m=>mxGetPr(prhs(1))
      n=>mxGetPr(prhs(2))
      x=>mxGetPr(prhs(3))
      outsize=>mxGetPr(prhs(4))
C---------------------------------------------------------------------
C     DO THE ACTUAL COMPUTATIONS IN A SUBROUTINE
      call lpmns2(pm,pd,m,n,x,outsize,m_m,m_n,n_m,n_n,x_m,x_n,outsize_m,
     &outsize_n)
C---------------------------------------------------------------------
      return
      end 


c     /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
c     /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
C     COMPUTATIONAL SUBROUTINE
      subroutine lpmns2(pm,pd,m,n,x,outsize,m_m,m_n,n_m,n_n,x_m,x_n,outs
     &ize_m,outsize_n)
c     Before anything, list what packages we will use.
      use mexfunctions;   use mexoperators;   use mexcallback
c     size variables
      integer m_m,m_n,n_m,n_n,x_m,x_n,outsize_m,outsize_n
C     First create all the calling variables. 
c     REM*** The changes are returned to the caller.
c     Matlab function pointers
c     All other local variables
      integer k
      real x0
      integer pm0
      real pmk
      real pm1
      real pm2
c     Input/Output local mirrors
      real m
      real n
      real x
      real outsize(outsize_m,outsize_n)
      real pm(outsize_m,outsize_n)
      real pd(outsize_m,outsize_n)
c     Fill in vars going in and out

c --- Main computational routine. --------------------------------------
      do  k=0.0,n
      pm(1,k+1)=0.0d0
      pd(1,k+1)=0.0d0
      enddo
      if ((abs(x) == 1.0d0)) then
      do  k=0.0,n
      if ((m == 0.0)) then
      pm(1,k+1)=1.0d0
      pd(1,k+1)=0.5d0*k*(k+1.0)
      if ((x < 0.0)) then
      pm(1,k+1)=(-1.0)**k*pm(1,k+1)
      pd(1,k+1)=(-1.0)**(k+1.0)*pd(1,k+1)
      endif
      elseif ((m == 1.0)) then
      pd(1,k+1)=1.0d+300
      elseif ((m == 2.0)) then
      pd(1,k+1)=-0.25d0*(k+2.0)*(k+1.0)*k*(k-1.0)
      if ((x < 0.0)) then
      pd(1,k+1)=(-1.0)**(k+1.0)*pd(1,k+1)
      endif
      endif
      enddo
      return
      endif
      x0=abs(1.0d0-x*x)
      pm0=1.0d0
      pmk=pm0
      do  k=1.0,m
      pmk=(2.0d0*k-1.0d0)*sqrt(x0)*pm0
      pm0=pmk
      enddo
      pm1=(2.0d0*m+1.0d0)*x*pm0
      pm(1,Int(m)+1)=pmk
      pm(1,Int(m)+1+1)=pm1
      do  k=m+2.0,n
      pm2=((2.0d0*k-1.0d0)*x*pm1-(k+m-1.0d0)*pmk)/(k-m)
      pm(1,k+1)=pm2
      pmk=pm1
      pm1=pm2
      enddo
      pd(1,0+1)=((1.0d0-m)*pm(1,1+1)-x*pm(1,0+1))/(x*x-1.0)
      do  k=1.0,n
      pd(1,k+1)=(k*x*pm(1,k+1)-(k+m)*pm(1,k-1+1))/(x*x-1.0d0)
      enddo

      return
      end 
C---------------------------------------------------------------------



c------------------------------------------------------------------------
c     This file generated by:
c matlab2fmex('lpmns2')
c---------------------------------------------------------------------
