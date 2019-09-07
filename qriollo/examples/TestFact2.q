
enchufar Chamuyo

el producto
  dados 0# m da 0#
  dados n  m da m + producto (n - 1#) m

el cociente
  dados n m
    si n <= m - 1# da 0#
    si no da cociente (n - m) m + 1#

el resto
  dados n m
    si n <= m - 1# da n
    si no da resto (n - m) m

el en_base
  dados 0# _ da []
  dados n  b da en_base (cociente n b) b ++ [resto n b]

el factorial
  dado n
    si n == 0# da 1#
    si no da
      producto n (factorial (n - 1#))

el mostrar_digitos
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

el programa es escupir (mostrar_digitos (en_base (factorial 9#) 10#) ++ "\n")

