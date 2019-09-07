
enchufar Chamuyo

el producto de Numerito en Numerito en Numerito
  dados n m
    si n == 0# da 0#
    si no da m + producto (n - 1#) m

el cociente de Numerito en Numerito en Numerito
  dados n m
    si n <= m - 1# da 0#
    si no da cociente (n - m) m + 1#

el resto de Numerito en Numerito en Numerito
  dados n m
    si n <= m - 1# da n
    si no da resto (n - m) m

el en_base de Numerito en Numerito en [Numerito]
dados 0# _ da []
dados n  b da en_base (cociente n b) b ++ [resto n b]

el factorial de Numerito en Numerito dado n
  si n == 0# da 1#
  si no da
    producto n (factorial (n - 1#))

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

el programa es (
  factorial 1#,
  factorial 2#,
  factorial 3#,
  factorial 4#,
  factorial 5#
)

