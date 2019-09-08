
mult:
    jz X mult_end

	# Loop to copy Y to Y1 and Z
	copy:
		jz Y copy_end
		dec Y
		inc Y1
		inc Z
        jmp copy
    copy_end:

	# Loop to rename Y1 to Y 
	rename:
		jz Y1 rename_end
		dec Y1
		inc Y
        jmp rename
    rename_end:

    dec X
    jmp mult
mult_end:

# Loop to erase all copies of Y
erase:
    dec Y
    jnz Y erase

# Initial values    

! X 11
! Y 9
