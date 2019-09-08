    # fact X Z
    inc Z
:l:1:
    jz X :l:2
    # bicopy X X1 X2
:l:3:
    jz X :l:4
    dec X
    inc fact:1:X1
    inc fact:1:X2
    jmp :l:3
:l:4:
    # mult X1 Y T
:l:5:
    jz fact:1:X1 :l:6
    dec fact:1:X1
    # bicopy Y Y2 Z
:l:7:
    jz Z :l:8
    dec Z
    inc mult:1:Y2
    inc fact:1:T
    jmp :l:7
:l:8:
    # rename Y2 Y
:l:9:
    jz mult:1:Y2 :l:10
    dec mult:1:Y2
    inc Z
    jmp :l:9
:l:10:
    jmp :l:5
:l:6:
    # zero Y
:l:11:
    jz Z :l:12
    dec Z
    jmp :l:11
:l:12:
    # rename T Y
:l:13:
    jz fact:1:T :l:14
    dec fact:1:T
    inc Z
    jmp :l:13
:l:14:
    # rename X2 X
:l:15:
    jz fact:1:X2 :l:16
    dec fact:1:X2
    inc X
    jmp :l:15
:l:16:
    dec X
    jmp :l:1
:l:2:
    ! X 20