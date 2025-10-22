SUBROUTINE np_m_ccsdt_t3_overlap(N,i0,i1,m,nocc,t1,t2,t3)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p4
INTEGER :: p5
INTEGER :: p6
INTEGER :: h1
INTEGER :: h2
INTEGER :: h3
INTEGER :: m(*)
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t3(*)
REAL*8 :: t2(*)
REAL*8 :: t1(*)
REAL*8 :: i1(*)
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=0.0d0
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
TMP=TMP+t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
TMP=TMP+t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*t1(((m(p6)-1)*N+m(h3)))
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*TMP
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h1)-&
&1)*N+m(h2)))+(1.0d0)*TMP
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)-&
&1)*N+m(h2)))+(-1.0d0)*TMP
i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*TMP
i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-&
&1)*N+m(h2)))+(1.0d0)*TMP
i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))=i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-&
&1)*N+m(h2)))+(-1.0d0)*TMP
i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-1.0d0)*TMP
i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-&
&1)*N+m(h2)))+(-1.0d0)*TMP
i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))=i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-&
&1)*N+m(h2)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=0.0d0
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+t1(((m(p4)-1)*N+m(h1)))*t1(((m(p5)-1)*N+m(h2)))
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-0.25d0)*TMP
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h1)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h1)))+(0.25d0)*TMP
i1(((((m(p5)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p5)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.25d0)*TMP
i1(((((m(p5)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))=i1(((((m(p5)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))+(-0.25d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
TMP=TMP+t1(((m(p4)-1)*N+m(h1)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-0.6666666666666666d0)*TMP
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-&
&1)*N+m(h3)))+(0.6666666666666666d0)*TMP
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-&
&1)*N+m(h1)))+(-0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))=i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-&
&1)*N+m(h3)))+(-0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))=i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-&
&1)*N+m(h1)))+(0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))=i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)-&
&1)*N+m(h3)))+(0.6666666666666666d0)*TMP
i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))=i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h3)-&
&1)*N+m(h1)))+(-0.6666666666666666d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p5)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p5)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP + i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p6)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p6)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h2)-1)*N+m(h1)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h1)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h1)-1)*N+m(h3)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h1)-1)*N+m(h2)))
TMP = TMP - i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h2)))
TMP = TMP + i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3))) = TMP / 36.0d0
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
