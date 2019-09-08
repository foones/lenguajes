    # mul X Y Z
    xmov mul:1:aX X
    xmov mul:1:aY Y
    xzero Z
:l:1:
    jz mul:1:aX :l:2
    xshr_rem mul:1:aX mul:1:bit 1
    jz mul:1:bit :l:4
    xadd Z mul:1:aY
:l:4:
    xshl mul:1:aY 1
    jmp :l:1
:l:2:
    xzero mul:1:bit
    xzero mul:1:aY
    xzero mul:1:aX
    xzero X
    xzero Y
    ! X 10
    ! Y 3