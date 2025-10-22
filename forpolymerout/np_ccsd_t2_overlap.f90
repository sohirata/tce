SUBROUTINE np_ccsd_t2_overlap(N,i0,nocc,t1,t2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t2(*)
REAL*8 :: t1(*)
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
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))
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
TMP=TMP+t1(((p3-1)*N+h1))*t1(((p4-1)*N+h2))
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))=i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))+(-0.5d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))=i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))+(-0.5d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h2-1)*N+h1))=i0(((((p4-1)*N+p3-1)*N+h2-1)*N+h1))+(0.5d0)*TMP
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
