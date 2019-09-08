    # fact X Res
    xmov fact:1:aX X
    xzero Res
    inc Res
    dec fact:1:aX
:l:1:
    jz fact:1:aX :l:2
    inc fact:1:aX
    # mul aX Res Res1
    xmov mul:1:aX fact:1:aX
    xmov mul:1:aY Res
    xzero fact:1:Res1
:l:3:
    jz mul:1:aX :l:4
    xshr_rem mul:1:aX mul:1:bit 1
    jz mul:1:bit :l:6
    xadd fact:1:Res1 mul:1:aY
:l:6:
    xshl mul:1:aY 1
    jmp :l:3
:l:4:
    xzero mul:1:bit
    xzero mul:1:aY
    xzero mul:1:aX
    xmov Res fact:1:Res1
    dec fact:1:aX
    dec fact:1:aX
    jmp :l:1
:l:2:
    xzero fact:1:Res1
    xzero X
    ! X 10