      subroutine H1f(xi,eta,phi,m,n,lambda,H0,AN,AM,pt,crea_par)
      doubleprecision xi
      doubleprecision eta
      doubleprecision phi
      doubleprecision m
      doubleprecision n
      doubleprecision lambda
      doubleprecision H0
      doubleprecision AN
      doubleprecision AM
      doubleprecision pt
      doubleprecision crea_par(3)

      doubleprecision H1(3)
      doubleprecision t1
      doubleprecision t10
      doubleprecision t100
      doubleprecision t103
      doubleprecision t108
      doubleprecision t11
      doubleprecision t112
      doubleprecision t12
      doubleprecision t13
      doubleprecision t15
      doubleprecision t16
      doubleprecision t166
      doubleprecision t17
      doubleprecision t2
      doubleprecision t21
      doubleprecision t22
      doubleprecision t23
      doubleprecision t25
      doubleprecision t28
      doubleprecision t3
      doubleprecision t30
      doubleprecision t32
      doubleprecision t35
      doubleprecision t39
      doubleprecision t4
      doubleprecision t44
      doubleprecision t45
      doubleprecision t46
      doubleprecision t5
      doubleprecision t52
      doubleprecision t56
      doubleprecision t57
      doubleprecision t62
      doubleprecision t68
      doubleprecision t7
      doubleprecision t71
      doubleprecision t78
      doubleprecision t79
      doubleprecision t8
      doubleprecision t83
      doubleprecision t94
      doubleprecision t95
      doubleprecision t99

      t1 = xi**2
      t2 = eta**2
      t3 = t1-t2
      t4 = sqrt(t3)
      t5 = 1/t4
      t7 = S(eta)
      t8 = R(xi)
      t10 = AM*t5*t7*t8
      t11 = (-1)**pt
      t12 = t11*m
      t13 = T(phi)
      t15 = -1+t1
      t16 = sqrt(t15)
      t17 = 1/t16
      t21 = 1/c
      t22 = t21*t16
      t23 = 1-t2
      t25 = 1/t3
      t28 = c**2
      t30 = t2**2
      t32 = m**2
      t35 = (eta-1)**2
      t39 = (eta+1)**2
      t44 = 1/t23
      t45 = 1/t15
      t46 = t44*t45
      t52 = t23*t25
      t56 = t3**2
      t57 = 1/t56
      t62 = diff(R(xi),xi)
      t68 = eta*t25
      t71 = -t23
      t78 = diff(S(eta),eta)
      t79 = t78*t8
      t83 = 1/t4/t3
      H1(1) = (-t10*t12*t13*eta*t17+((-t22*t5*(xi*t23*t25*(-lambda+lam
     #     bda*t2+t28*t2-t28*t30+t32)/t35/t39-t32*xi*t46)*t8+t22*t5*(t52
     &     -2*t2*t25+2*t2*t23*t57)*t62)*t7-t22*t5*xi*(-2*t68+2*t23*t57
     &     *eta-2*t52*eta/t71)*t79+t22*t83*eta*t23*t78*t62)*t13*AN)*H0
      t94 = sqrt(t23)
      t95 = 1/t94
      t99 = t21*t94
      t100 = eta*t15
      t103 = t1**2
      t108 = (xi-1)**2
      t112 = (xi+1)**2
      H1(2) = (t10*t12*t13*xi*t95+((t99*t5*(-t100*t25*(lambda*t1-lambd
     #     a-t28*t103+t28*t1+t32)/t108/t112+t32*eta*t46)*t8+2*t99/t4/t56
     &     *t100*t62*xi)*t7+t99*t5*t78*(t15*t25+2*t1*t25-2*t1*t15*t57)
     &     *t8+t99*t83*t78*xi*t15*t62)*t13*AN)*H0
      t166 = t21*m
      H1(3) = ((t71*t62*eta*t16*t25*t95*t7-t71*t78*t8*xi*t16*t25*t95)*
     #     t13*AM+((t166*t94*t16*t25*(-t45-t44)*t11*t8-t166*t95*t16*t25
     &     *xi*t62*t11)*t7-t166*t94*t17*t68*t79*t11)*t13*AN)*H0
      crea_par(1) = H1(1)
      crea_par(2) = H1(2)
      crea_par(3) = H1(3)
      return
      return
      end
