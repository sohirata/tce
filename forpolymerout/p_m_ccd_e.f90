SUBROUTINE p_m_ccd_e(N,i0,i0e,m,nocc,t2,t2e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p1
INTEGER :: p2
INTEGER :: h3
INTEGER :: h4
INTEGER :: m(*)
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
i0(1)=0.0d0
DO p1=nocc+1,N
DO p2=nocc+1,N
IF (p1>p2) CYCLE
DO h3=1,nocc
DO h4=1,nocc
IF (h3>h4) CYCLE
i0(1)=i0(1)+(1.0d0)*t2(((((m(p1)-1)*N+m(p2)-1)*N+m(h3)-1)*N+m(h4)))*v2(((((m(h3)-1)*N+m(h4)-1)*N+m(p1)-1)*N+m(p2)))
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
