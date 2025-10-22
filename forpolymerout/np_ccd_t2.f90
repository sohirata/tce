SUBROUTINE np_ccd_t2(N,f1,i0,i1,nocc,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: h5
INTEGER :: p5
INTEGER :: h9
INTEGER :: h7
INTEGER :: h6
INTEGER :: p6
INTEGER :: h8
INTEGER :: p7
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
DO h5=1,nocc
DO h1=1,nocc
i1(((h5-1)*N+h1))=0.0d0
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((h5-1)*N+h1))
i1(((h5-1)*N+h1))=i1(((h5-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h8=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
TMP=TMP+t2(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))*v2(((((h5-1)*N+h8-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
i1(((h5-1)*N+h1))=i1(((h5-1)*N+h1))+(0.5d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h5=1,nocc
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*i1(((h5-1)*N+h2))
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))=i0(((((p3-1)*N+p4-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
i1(((p3-1)*N+p5))=0.0d0
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((p3-1)*N+p5))
i1(((p3-1)*N+p5))=i1(((p3-1)*N+p5))+(1.0d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
TMP=0.0d0
DO h7=1,nocc
DO h8=1,nocc
DO p6=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p6-1)*N+h7-1)*N+h8))*v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
i1(((p3-1)*N+p5))=i1(((p3-1)*N+p5))+(-0.5d0)*TMP
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p5=nocc+1,N
TMP=TMP+t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))*i1(((p4-1)*N+p5))
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))=i0(((((p4-1)*N+p3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))=0.0d0
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))
i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))=i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p5=nocc+1,N
DO p6=nocc+1,N
TMP=TMP+t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
END DO
END DO
i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))=i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))+(-0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO h7=1,nocc
TMP=TMP+t2(((((p3-1)*N+p4-1)*N+h7-1)*N+h9))*i1(((((h7-1)*N+h9-1)*N+h1-1)*N+h2))
END DO
END DO
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=0.0d0
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))+(1.0d0)*TMP
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
TMP=TMP+t2(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))*v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
END DO
END DO
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))+(-0.5d0)*TMP
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
TMP=TMP+t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))*i1(((((h6-1)*N+p4-1)*N+h2-1)*N+p5))
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
