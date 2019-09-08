
SUB rename X Y
	WHILENZ X
		dec X
		inc Y
	END
END

SUB bicopy X Y Z
	WHILENZ X
		dec X
		inc Y
		inc Z
	END
END

SUB zero X
	WHILENZ X
		dec X
	END
END

SUB mult X Y Z
	WHILENZ X 
		dec X
		bicopy Y Y2 Z
		rename Y2 Y
	END
	zero Y
END

SUB fact X Y
	inc Y
	WHILENZ X
		bicopy X X1 X2
		mult X1 Y T
		rename T Y
		rename X2 X
		dec X
	END
END

fact X Z

! X 20

