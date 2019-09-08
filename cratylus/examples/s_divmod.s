
divmod:
    jz X erase
	
	copy:
		jz Y sub
		dec Y
		inc Y1
		inc Y2
		jmp copy

	sub:
		jz Y1 rename
		jz X rem
		dec X
		dec Y1
		jmp sub

	rem:
		jz Y1 rem2
		dec Y2
		dec Y1 
		jmp rem

	rem2:
		jz Y2 divmod_end
		dec Y2
		inc R
		jmp rem2

	rename:
		jz Y2 divmod_cont
		dec Y2
		inc Y
		jmp rename

divmod_cont:
	inc Q
	jmp divmod

    erase:
  		dec Y
		jnz Y erase

divmod_end:

! X 10
! Y 2

