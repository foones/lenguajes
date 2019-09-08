
SUB mul X Y Res
	xmov aX X
	xmov aY Y
	xzero Res
	WHILENZ aX
		xshr_rem aX bit 1
		IFNZ bit
			xadd Res aY
		END
		xshl aY 1
	END
	xzero bit
	xzero aY
	xzero aX
END

SUB fact X Res
	xmov aX X
	xzero Res
	inc Res	

	dec aX
	WHILENZ aX
		inc aX
		mul aX Res Res1
		xmov Res Res1
		dec aX
		dec aX
	END
	xzero Res1
END

fact X Res
xzero X

! X 10
