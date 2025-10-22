SUBROUTINE np_m_ccd_t2_disconnected(N,f1,i0,i1,m,nocc,t2,v2)
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
INTEGER :: h8
INTEGER :: p7
INTEGER :: h7
INTEGER :: p8
INTEGER :: m(*)
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: v2(*)
REAL*8 :: t2(*)
REAL*8 :: i1(*)
REAL*8 :: f1(*)
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
TMP=TMP+v2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
i1(((m(h5)-1)*N+m(h1)))=0.0d0
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((m(h5)-1)*N+m(h1)))
i1(((m(h5)-1)*N+m(h1)))=i1(((m(h5)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h8=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
TMP=TMP+t2(((((m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))*v2(((((m(h5)-1)*N+m(h8)-1)*N+m(p6)-1)*N+m(p7)))
END DO
END DO
END DO
i1(((m(h5)-1)*N+m(h1)))=i1(((m(h5)-1)*N+m(h1)))+(0.5d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h5=1,nocc
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))*i1(((m(h5)-1)*N+m(h2)))
END DO
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*TMP
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
i1(((m(p3)-1)*N+m(p5)))=0.0d0
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((m(p3)-1)*N+m(p5)))
i1(((m(p3)-1)*N+m(p5)))=i1(((m(p3)-1)*N+m(p5)))+(1.0d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
TMP=0.0d0
DO h7=1,nocc
DO h8=1,nocc
DO p6=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))*v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))
END DO
END DO
END DO
i1(((m(p3)-1)*N+m(p5)))=i1(((m(p3)-1)*N+m(p5)))+(-0.5d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p5=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*i1(((m(p4)-1)*N+m(p5)))
END DO
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*TMP
i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h6=1,nocc
DO h1=1,nocc
DO h2=1,nocc
i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))=0.0d0
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h6=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))
i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h6=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
TMP=TMP+t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)))*v2(((((m(h5)-1)*N+m(h6)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*TMP
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
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h5)-1)*N+m(h6)))*i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))=0.0d0
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))
i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))=i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
TMP=0.0d0
DO h8=1,nocc
DO p7=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))*v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
END DO
END DO
i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))=i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))+(-0.5d0)*TMP
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
TMP=TMP+t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))*i1(((((m(h6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p5)))
END DO
END DO
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*TMP
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h1)))+(1.0d0)*TMP
i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*TMP
i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h2)-1)*N+m(h1)))=i0(((((m(p4)-1)*N+m(p3)-1)*N+m(h2)-1)*N+m(h1)))+(-1.0d0)*TMP
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
TMP=TMP+t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))*v2(((((m(p3)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(p6)))
END DO
END DO
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
i1(1)=0.0d0
TMP=0.0d0
DO h7=1,nocc
DO h8=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
TMP=TMP+t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))*v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))
END DO
END DO
END DO
END DO
i1(1)=i1(1)+(1.0d0)*TMP
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))*i1(1)
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.25d0)*TMP
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
