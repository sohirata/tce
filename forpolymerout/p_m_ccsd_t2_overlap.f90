SUBROUTINE p_m_ccsd_t2_overlap(N,i0,i0e,m,nocc,t1,t1e,t2,t2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: m(*)
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*t1(((m(p3)-1)*N+m(h1)))*t1(&
&((m(p4)-1)*N+m(h2)))+(-0.5d0)*t1(((m(p3)-1)*N+m(h2)))*t1(((m(p4)-1)*N+m(h1)))+(-0.5d0)*t1(((m(p4)-1)*N+m(h1)))*t1(((m(p3)-1)*N+m(h&
&2)))+(0.5d0)*t1(((m(p4)-1)*N+m(h2)))*t1(((m(p3)-1)*N+m(h1)))
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
