    # nth_prime N Z
    inc Z
:l:1:
    jz N :l:2
    inc Z
    # is_prime P IsPrime
    jz Z is_prime:1:zero_not_prime
    dec Z
    jz Z is_prime:1:one_not_prime
    dec Z
    jz Z is_prime:1:two_prime
    inc Z
    inc Z
    # safe_copy P D
    # copy X Y Z
:l:3:
    jz Z :l:4
    dec Z
    inc is_prime:1:D
    inc safe_copy:1:Z
    jmp :l:3
:l:4:
    # rename Z X
:l:5:
    jz safe_copy:1:Z :l:6
    dec safe_copy:1:Z
    inc Z
    jmp :l:5
:l:6:
    dec is_prime:1:D
    dec is_prime:1:D
:l:7:
    jz is_prime:1:D :l:8
    inc is_prime:1:D
    # safe_copy P P2
    # copy X Y Z
:l:9:
    jz Z :l:10
    dec Z
    inc is_prime:1:P2
    inc safe_copy:2:Z
    jmp :l:9
:l:10:
    # rename Z X
:l:11:
    jz safe_copy:2:Z :l:12
    dec safe_copy:2:Z
    inc Z
    jmp :l:11
:l:12:
    # safe_copy D D2
    # copy X Y Z
:l:13:
    jz is_prime:1:D :l:14
    dec is_prime:1:D
    inc is_prime:1:D2
    inc safe_copy:3:Z
    jmp :l:13
:l:14:
    # rename Z X
:l:15:
    jz safe_copy:3:Z :l:16
    dec safe_copy:3:Z
    inc is_prime:1:D
    jmp :l:15
:l:16:
    # remainder P2 D2 R
:l:17:
    jz is_prime:1:P2 :l:18
    # copy Y Y1 R
:l:19:
    jz is_prime:1:D2 :l:20
    dec is_prime:1:D2
    inc remainder:1:Y1
    inc is_prime:1:R
    jmp :l:19
:l:20:
:l:21:
    jz remainder:1:Y1 :l:22
    jz is_prime:1:P2 remainder:1:break
    dec is_prime:1:P2
    dec remainder:1:Y1
    jmp :l:21
:l:22:
    # rename R Y
:l:23:
    jz is_prime:1:R :l:24
    dec is_prime:1:R
    inc is_prime:1:D2
    jmp :l:23
:l:24:
    jmp :l:17
:l:18:
    # zero Y
:l:25:
    jz is_prime:1:D2 :l:26
    dec is_prime:1:D2
    jmp :l:25
:l:26:
remainder:1:break:
:l:27:
    jz remainder:1:Y1 :l:28
    dec remainder:1:Y1
    dec is_prime:1:R
    jmp :l:27
:l:28:
    jz is_prime:1:R is_prime:1:zero_not_prime
    # zero R
:l:29:
    jz is_prime:1:R :l:30
    dec is_prime:1:R
    jmp :l:29
:l:30:
    dec is_prime:1:D
    dec is_prime:1:D
    jmp :l:7
:l:8:
    inc nth_prime:1:IsPrime
    jmp is_prime:1:zero_not_prime
is_prime:1:two_prime:
    inc nth_prime:1:IsPrime
    inc Z
is_prime:1:one_not_prime:
    inc Z
is_prime:1:zero_not_prime:
    # zero D
:l:31:
    jz is_prime:1:D :l:32
    dec is_prime:1:D
    jmp :l:31
:l:32:
    jz nth_prime:1:IsPrime :l:34
    dec N
:l:34:
    dec nth_prime:1:IsPrime
    jmp :l:1
:l:2:
    ! N 20