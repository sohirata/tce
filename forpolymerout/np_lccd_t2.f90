SUBROUTINE np_lccd_t2(N,f1,i0,nocc,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: h5
INTEGER :: p5
INTEGER :: h6
INTEGER :: p6
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: v2(*)
REAL*8 :: t2(*)
REAL*8 :: f1(*)
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=0.0d0
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h5=1,nocc
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*f1(((h5-1)*N+h2))
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))=i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p5=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))*f1(((p4-1)*N+p5))
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))=i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO h6=1,nocc
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h5-1)*N+h6))*v2(((((h5-1)*N+h6-1)*N+h1-1)*N+h2))
END DO
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))*v2(((((h6-1)*N+p4-1)*N+h2-1)*N+p5))
END DO
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))=i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))=i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h2-1)*N+h1))=i0(((((p4-1)*N+p3-1)*N+h2-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p5=nocc+1,N
DO p6=nocc+1,N
TMP=TMP+t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*v2(((((p3-1)*N+p4-1)*N+p5-1)*N+p6))
END DO
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((((p4-1)*N+p3-1)*N+h2-1)*N+h1))
TMP = TMP - i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))
TMP = TMP - i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))
TMP = TMP + i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2)) = TMP / 4.0d0
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
