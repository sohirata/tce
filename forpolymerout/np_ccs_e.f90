SUBROUTINE np_ccs_e(N,f1,i0,i1,nocc,t1,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p5
INTEGER :: h6
INTEGER :: h4
INTEGER :: p3
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t1(*)
REAL*8 :: i1(*)
REAL*8 :: f1(*)
REAL*8 :: v2(*)
DO h6=1,nocc
DO p5=nocc+1,N
i1(((h6-1)*N+p5))=0.0d0
END DO
END DO
DO h6=1,nocc
DO p5=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h6-1)*N+p5))
i1(((h6-1)*N+p5))=i1(((h6-1)*N+p5))+(1.0d0)*TMP
END DO
END DO
DO h6=1,nocc
DO p5=nocc+1,N
TMP=0.0d0
DO h4=1,nocc
DO p3=nocc+1,N
TMP=TMP+t1(((p3-1)*N+h4))*v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
END DO
END DO
i1(((h6-1)*N+p5))=i1(((h6-1)*N+p5))+(0.5d0)*TMP
END DO
END DO
i0(1)=0.0d0
TMP=0.0d0
DO p5=nocc+1,N
DO h6=1,nocc
TMP=TMP+t1(((p5-1)*N+h6))*i1(((h6-1)*N+p5))
END DO
END DO
i0(1)=i0(1)+(1.0d0)*TMP
RETURN
END SUBROUTINE
