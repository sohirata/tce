SUBROUTINE np_m_ccsd_t2_overlap(N,i0,m,nocc,t1,t2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: m(*)
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t2(*)
REAL*8 :: t1(*)
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=0.0d0
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+t1(((m(p3)-1)*N+m(h1)))*t1(((m(p4)-1)*N+m(h2)))
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*TMP
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))+(-0.5d0)*TMP
i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))+(-0.5d0)*TMP
i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h2)-1)*N+m(h1)))=i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h2)-1)*N+m(h1)))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP - i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP - i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP + i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2))) = TMP / 4.0d0
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
