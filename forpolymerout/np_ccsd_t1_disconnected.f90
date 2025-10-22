SUBROUTINE np_ccsd_t1_disconnected(N,f1,i0,i1,i2,nocc,t1,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h7
INTEGER :: p3
INTEGER :: h4
INTEGER :: p7
INTEGER :: h8
INTEGER :: h5
INTEGER :: p4
INTEGER :: p8
INTEGER :: h6
INTEGER :: p5
INTEGER :: p6
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: f1(*)
REAL*8 :: t1(*)
REAL*8 :: i1(*)
REAL*8 :: i2(*)
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
DO h7=1,nocc
DO h1=1,nocc
i1(((h7-1)*N+h1))=0.0d0
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((h7-1)*N+h1))
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
i2(((h7-1)*N+p8))=0.0d0
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h7-1)*N+p8))
i2(((h7-1)*N+p8))=i2(((h7-1)*N+p8))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((p5-1)*N+h6))*v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
END DO
END DO
i2(((h7-1)*N+p8))=i2(((h7-1)*N+p8))+(0.9999999999999999d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h1))*i2(((h7-1)*N+p8))
END DO
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p4=nocc+1,N
TMP=TMP+t1(((p4-1)*N+h5))*v2(((((h5-1)*N+h7-1)*N+h1-1)*N+p4))
END DO
END DO
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*v2(((((h5-1)*N+h7-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(-0.5d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h7=1,nocc
TMP=TMP+t1(((p2-1)*N+h7))*i1(((h7-1)*N+h1))
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
i1(((p2-1)*N+p3))=0.0d0
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((p2-1)*N+p3))
i1(((p2-1)*N+p3))=i1(((p2-1)*N+p3))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
TMP=0.0d0
DO h5=1,nocc
DO p4=nocc+1,N
TMP=TMP+t1(((p4-1)*N+h5))*v2(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
i1(((p2-1)*N+p3))=i1(((p2-1)*N+p3))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p3=nocc+1,N
TMP=TMP+t1(((p3-1)*N+h1))*i1(((p2-1)*N+p3))
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
DO h8=1,nocc
DO p7=nocc+1,N
i1(((h8-1)*N+p7))=0.0d0
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h8-1)*N+p7))
i1(((h8-1)*N+p7))=i1(((h8-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((p5-1)*N+h6))*v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
END DO
END DO
i1(((h8-1)*N+p7))=i1(((h8-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
TMP=TMP+t2(((((p2-1)*N+p7-1)*N+h1-1)*N+h8))*i1(((h8-1)*N+p7))
END DO
END DO
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))=0.0d0
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))
i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))=i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
DO p6=nocc+1,N
TMP=TMP+t1(((p6-1)*N+h1))*v2(((((h4-1)*N+h5-1)*N+p3-1)*N+p6))
END DO
i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))=i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h4=1,nocc
DO h5=1,nocc
DO p3=nocc+1,N
TMP=TMP+t2(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))*i1(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))
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
DO h4=1,nocc
DO p3=nocc+1,N
i2(((h4-1)*N+p3))=0.0d0
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h4-1)*N+p3))
i2(((h4-1)*N+p3))=i2(((h4-1)*N+p3))+(1.0d0)*TMP
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((p5-1)*N+h6))*v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
END DO
END DO
i2(((h4-1)*N+p3))=i2(((h4-1)*N+p3))+(0.5d0)*TMP
END DO
END DO
i1(1)=0.0d0
TMP=0.0d0
DO h4=1,nocc
DO p3=nocc+1,N
TMP=TMP+t1(((p3-1)*N+h4))*i2(((h4-1)*N+p3))
END DO
END DO
i1(1)=i1(1)+(1.0d0)*TMP
TMP=0.0d0
DO h5=1,nocc
DO h6=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h5-1)*N+h6))*v2(((((h5-1)*N+h6-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
END DO
i1(1)=i1(1)+(0.25d0)*TMP
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+t1(((p2-1)*N+h1))*i1(1)
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*TMP
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
