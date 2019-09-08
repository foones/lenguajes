    # div_mod X Y Z Rrr
    xmov div_mod:1:X0 X
    xzero Z
    xzero Rrr
div_mod:1:outer:
    jz div_mod:1:X0 div_mod:1:outer_end
    xmov div_mod:1:Yc  Y
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
    jnz div_mod:1:Yc2 :l:2
    xsub div_mod:1:X0 div_mod:1:Yc
    xadd Z div_mod:1:Q1
    jmp div_mod:1:outer
:l:2:
    xmov Rrr div_mod:1:X0
div_mod:1:outer_end:
    xzero div_mod:1:Yc
    xzero div_mod:1:Yc2
    xzero div_mod:1:Q1
    xzero div_mod:1:X0
    xzero X
    xzero Y
    ! X 35
    ! Y 9