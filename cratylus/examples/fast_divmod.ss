
SUB div_mod X Y Q R
	xmov X0 X
	xzero Q
	xzero R
    outer:
		jz X0 outer_end

        xmov Yc  Y
        xmov Q1  1

        inner:
            xmov Yc2 Yc
            xshl Yc2 1
            xmov X_Yc2 X0
            xsub X_Yc2 Yc2
            jz X_Yc2 inner_end

            xshl Q1 1
            xshl Yc 1

            jmp inner
        inner_end:

		xmov Yc2 Yc
		xsub Yc2 X0
		IFZ Yc2
			xsub X0 Yc
			xadd Q Q1
			jmp outer
		END
		
		xmov R X0	
		
    outer_end:
		xzero Yc
		xzero Yc2
		xzero Q1
		xzero X0
END

div_mod X Y Z Rrr
xzero X
xzero Y

! X 35
! Y 9
