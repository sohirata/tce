SUBROUTINE np_lccsd_t1(N,f1,i0,nocc,t1,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h3
INTEGER :: p3
INTEGER :: h4
INTEGER :: p4
INTEGER :: h5
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: f1(*)
REAL*8 :: t1(*)
REAL*8 :: v2(*)
REAL*8 :: t2(*)
DO p2=nocc+1,N
DO h1=1,nocc
i0(((p2-1)*N+h1))=0.0d0
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((p2-1)*N+h1))
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h3=1,nocc
TMP=TMP+t1(((p2-1)*N+h3))*f1(((h3-1)*N+h1))
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p3=nocc+1,N
TMP=TMP+t1(((p3-1)*N+h1))*f1(((p2-1)*N+p3))
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h4=1,nocc
DO p3=nocc+1,N
TMP=TMP+t1(((p3-1)*N+h4))*v2(((((h4-1)*N+p2-1)*N+h1-1)*N+p3))
END DO
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p4=nocc+1,N
DO h3=1,nocc
TMP=TMP+t2(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))*f1(((h3-1)*N+p4))
END DO
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h4=1,nocc
DO h5=1,nocc
DO p3=nocc+1,N
TMP=TMP+t2(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))*v2(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))
END DO
END DO
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-0.5d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*v2(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-0.5d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((p2-1)*N+h1))
i0(((p2-1)*N+h1)) = TMP / 1.0d0
END DO
END DO
RETURN
END SUBROUTINE
