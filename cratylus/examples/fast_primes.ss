
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

SUB is_prime X IsPrime
	xzero IsPrime

	# check for 0
	IFZ X
		jmp end_loop
	END

	# check for 1
	dec X
	IFZ X
		inc X
		jmp end_loop
	END
	inc X

	inc IsPrime

	xmov P X
	dec P
	loop:
		dec P
		jz P end_loop
		inc P

		div_mod X P Q R
		IFZ R
			dec IsPrime
			jmp end_loop
		END

		dec P
		jmp loop

	end_loop:
		xzero P
		xzero Q
		xzero R
END

SUB nth_prime N P
	xzero P
	WHILENZ N
		inc P	
		is_prime P B
		IFNZ B
			dec N
		END
	END
	xzero B
END

nth_prime N Z
xzero N

! N 100

