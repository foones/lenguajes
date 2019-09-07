
enchufar Chamuyo

el producto de Numerito en Numerito en Numerito
dados n m da  
  mirar PRIM.igual_numerito n 0#
  si Sí da 0#
  si no da PRIM.sumar_numerito m (producto (PRIM.restar_numerito n 1#) m)
  boludo

el cociente_resto de Numerito en Numerito en (Numerito, Numerito)
dados n m da  
  mirar PRIM.menor_o_igual_numerito n (PRIM.restar_numerito m 1#)
  si Sí da (0#, n)
  si no da
    mirar cociente_resto (PRIM.restar_numerito n m) m
    si (q, r) da (PRIM.sumar_numerito q 1#, r)
    boludo
  boludo

el en_base de Numerito en Numerito en [Numerito]
dados 0# _ da []
dados n  b da
  mirar cociente_resto n b
  si (q, r) da en_base q b ++ [r]
  boludo

el mostrar_digitos de [Numerito] en Texto
dado [] da ""
dado (0# : ds) da "0" ++ mostrar_digitos ds
dado (1# : ds) da "1" ++ mostrar_digitos ds
dado (2# : ds) da "2" ++ mostrar_digitos ds
dado (3# : ds) da "3" ++ mostrar_digitos ds
dado (4# : ds) da "4" ++ mostrar_digitos ds
dado (5# : ds) da "5" ++ mostrar_digitos ds
dado (6# : ds) da "6" ++ mostrar_digitos ds
dado (7# : ds) da "7" ++ mostrar_digitos ds
dado (8# : ds) da "8" ++ mostrar_digitos ds
dado (9# : ds) da "9" ++ mostrar_digitos ds
dado (_  : ds) da "_" ++ mostrar_digitos ds

encarnar Mostrable para Numerito
  el mostrar
    dado 0# da "0#"
    dado n  da mostrar_digitos (en_base n 10#) ++ "#"
boludo

el programa es escupir (mostrar 123456789# ++ "\n")

