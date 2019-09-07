enchufar Chamuyo

el mostrar_numerito dado n
si PRIM.igual_numerito n 0# da ""
si no da "#" ++ mostrar_numerito (PRIM.restar_numerito n 1#)

la f dado x da escupir (mostrar_numerito (
    mirar x
    si 20# da 10#
    si 21# da 11#
    si 22# da 12#
    si 90# da 20#
    si 91# da 21#
    si 92# da 22#
    si 93# da 23#
    si no  da 55#
    boludo
  ) ++ "\n")

el para_cada
dados []     f da ()
dados (x:xs) f da ponele que el _ es f x en para_cada xs f

el programa es
  para_cada [19#, 20#, 21#, 22#, 23#, 89#, 90#, 91#, 92#, 93#, 94#]
    f

