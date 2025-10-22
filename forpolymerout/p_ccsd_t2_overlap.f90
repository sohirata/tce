SUBROUTINE p_ccsd_t2_overlap(N,i0,i0e,nocc,t1,t1e,t2,t2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=(1.0d0)*t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*t1(((p3-1)*N+h1))*t1(((p4-1)*N+h2))+(-0.5d0)*t1(((p&
&3-1)*N+h2))*t1(((p4-1)*N+h1))+(-0.5d0)*t1(((p4-1)*N+h1))*t1(((p3-1)*N+h2))+(0.5d0)*t1(((p4-1)*N+h2))*t1(((p3-1)*N+h1))
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
