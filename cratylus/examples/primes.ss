
SUB rename X Y
	WHILENZ X
		dec X
		inc Y
	END
END

SUB copy X Y Z
	WHILENZ X
		dec X
		inc Y
		inc Z
	END
END

SUB safe_copy X Y
	copy X Y Z
	rename Z X
END

SUB zero X
	WHILENZ X
		dec X
	END
END

SUB remainder X Y R
    WHILENZ X
		copy Y Y1 R
		WHILENZ Y1
			jz X break
			dec X
			dec Y1
		END
		rename R Y
    END
	zero Y
break:
	WHILENZ Y1
		dec Y1
		dec R
	END
END

SUB is_prime P IsPrime
	jz P zero_not_prime
	dec P
	jz P one_not_prime
	dec P
	jz P two_prime
	inc P
	inc P

	safe_copy P D
	dec D
	dec D
	WHILENZ D
		inc D
		safe_copy P P2
		safe_copy D D2
		remainder P2 D2 R
		jz R zero_not_prime
		zero R
		dec D
		dec D
	END
	inc IsPrime
	jmp zero_not_prime

two_prime:
	inc IsPrime
	inc P
one_not_prime:
	inc P
zero_not_prime:
	zero D
END

SUB nth_prime N P
	inc P
	WHILENZ N
		inc P
		is_prime P IsPrime
		IFNZ IsPrime
			dec N
		END
		dec IsPrime
	END
END

nth_prime N Z
! N 20

