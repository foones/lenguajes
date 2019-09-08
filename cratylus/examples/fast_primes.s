    # nth_prime N Z
    xzero Z
:l:1:
    jz N :l:2
    inc Z
    # is_prime P B
    xzero nth_prime:1:B
    jnz Z :l:4
    jmp is_prime:1:end_loop
:l:4:
    dec Z
    jnz Z :l:6
    inc Z
    jmp is_prime:1:end_loop
:l:6:
    inc Z
    inc nth_prime:1:B
    xmov is_prime:1:P Z
    dec is_prime:1:P
is_prime:1:loop:
    dec is_prime:1:P
    jz is_prime:1:P is_prime:1:end_loop
    inc is_prime:1:P
    # div_mod X P Q R
    xmov div_mod:1:X0 Z
    xzero is_prime:1:Q
    xzero is_prime:1:R
div_mod:1:outer:
    jz div_mod:1:X0 div_mod:1:outer_end
    xmov div_mod:1:Yc  is_prime:1:P
    xmov div_mod:1:Q1  1
div_mod:1:inner:
    xmov div_mod:1:Yc2 div_mod:1:Yc
    xshl div_mod:1:Yc2 1
    xmov div_mod:1:X_Yc2 div_mod:1:X0
    xsub div_mod:1:X_Yc2 div_mod:1:Yc2
    jz div_mod:1:X_Yc2 div_mod:1:inner_end
    xshl div_mod:1:Q1 1
    xshl div_mod:1:Yc 1
    jmp div_mod:1:inner
div_mod:1:inner_end:
    xmov div_mod:1:Yc2 div_mod:1:Yc
    xsub div_mod:1:Yc2 div_mod:1:X0
    jnz div_mod:1:Yc2 :l:8
    xsub div_mod:1:X0 div_mod:1:Yc
    xadd is_prime:1:Q div_mod:1:Q1
    jmp div_mod:1:outer
:l:8:
    xmov is_prime:1:R div_mod:1:X0
div_mod:1:outer_end:
    xzero div_mod:1:Yc
    xzero div_mod:1:Yc2
    xzero div_mod:1:Q1
    xzero div_mod:1:X0
    jnz is_prime:1:R :l:10
    dec nth_prime:1:B
    jmp is_prime:1:end_loop
:l:10:
    dec is_prime:1:P
    jmp is_prime:1:loop
is_prime:1:end_loop:
    xzero is_prime:1:P
    xzero is_prime:1:Q
    xzero is_prime:1:R
    jz nth_prime:1:B :l:12
    dec N
:l:12:
    jmp :l:1
:l:2:
    xzero nth_prime:1:B
    xzero N
    ! N 100