
SUB mul X Y Z
	xmov aX X
	xmov aY Y
	xzero Z
	WHILENZ aX
		xshr_rem aX bit 1
		IFNZ bit
			xadd Z aY
		END
		xshl aY 1
	END
	xzero bit
	xzero aY
	xzero aX
	xzero X
	xzero Y
END

mul X Y Z

! X 10
! Y 3
