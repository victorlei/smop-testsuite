program bayes


 double precision tepoch,xref(6),maxit,reject
 
 tepoch=sqrt(2.0)
 xref=[sqrt(3.0),2.,3.,4.,5.,6.]
 maxit=sqrt(5.0)
 reject=sqrt(6.0)

open(9,file='baydebug.out',status='unknown')


11 format ( e20.13'tepoch='e20.13,'tepoch='e20.13)
write (*,11) tepoch,tepoch,tepoch

10 format( "abc",4x,/,2(6(e20.13),//),//)

write(9,fmt=10) xref,xref


write(9,'("abc",4x,/,2(6(e20.13),//),//)') xref,xref


6 format( //,2x,"BAYES FILTER",/,2x, &
'epoch time :',e20.13,/, &
2x,'Previous Estimated State Vector:',/,2x,3e20.13,/,2x,3e20.13,/ &
, 2x,'maximum iterations:',i3, &
/, 2x,'reject if gt ',e12.5,' sigma ',/// )

write (9,6) tepoch,xref,maxit,reject;


write (*, *) tepoch,'ggggggg';

write(9,*) tepoch,'asd',xref,'tytyty',maxit
write(9,*) tepoch

print *,'wrote it'

close(9)


print *, tepoch,'asd',xref,'tytyty',maxit

print 6, tepoch,xref,maxit,reject;




end program bayes

