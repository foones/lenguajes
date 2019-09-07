
entregar (Natural)

enchufar Chamuyo

chirimbolo diestro 500 ::+

OJO. Números naturales

OJO. Los naturales se representan como una lista de dígitos
OJO. en base (1# << exp_base_natural).

un Natural es
  bien NATURAL [Numerito]

la exp_base_natural es 16#

la máscara_natural es (1# << exp_base_natural) - 1

el natural_mul2 de Natural en Natural
  dado n da n + n

el natural_es_par de Natural en Posta
  dado (NATURAL [])      da Sí
  dado (NATURAL (x : _)) da PRIM.resto_numerito x 2# == 0#

el ::+ de Numerito en [Numerito] en [Numerito]
  dados 0# [] da []
  dados x  xs da (x : xs)

el natural_div2 de Natural en Natural
  dado (NATURAL xs) da NATURAL $ pi1 (rec xs)
  donde
    la rec de [Numerito] en ([Numerito], Numerito)
      dado []       da ([], 0#)
      dado (x : xs) da
        mirar rec xs
        si (q, r) da
           ((r << (exp_base_natural - 1#)) | (x >> 1#) ::+ q,
            x & 1#)
        boludo
  boludo

el natural_div_mod de Natural en Natural en (Natural, Natural)
  dados n m da rec n m 0
  donde
    el rec de Natural en Natural en Natural
           en (Natural, Natural)
    dados n m q
     si n < m da (q, n)
     si no da
       mirar max_pot2_m_lt_n n m 1
       si (mp, p) da 
         rec (n - mp) m (q + p)
       boludo
    la max_pot2_m_lt_n de Natural en Natural en Natural
                       en (Natural, Natural)
    dados n mp p da
      ponele que el mp2 es mp + mp en
       si mp2 <= n da max_pot2_m_lt_n n mp2 (p + p)
       si no       da (mp, p)
  boludo

encarnar Numérico para Natural
  el + dados (NATURAL xs) (NATURAL ys) da
             NATURAL $ sumar_naturales xs ys 0#
  donde
    el sumar_naturales
    dados [] [] c       da c ::+ []
    dados [] (y : ys) c da sumar_naturales [0#] (y : ys) c
    dados (x : xs) [] c da sumar_naturales (x : xs) [0#] c
    dados (x : xs) (y : ys) c
      da ponele que el r es x + y + c en
        r & máscara_natural ::+
        sumar_naturales xs ys (r >> exp_base_natural)
  boludo

  el - dados (NATURAL xs) (NATURAL ys)
    si NATURAL xs >= NATURAL ys da
       NATURAL $ restar_naturales xs ys 0#
    si no da 0
  donde
    el restar_naturales
    dados []       _ _  da []
    dados (x : xs) [] b da restar_naturales (x : xs) [0#] b
    dados (x : xs) (y : ys) b da
      si y + b <= x da (x - y - b) ::+
                       restar_naturales xs ys 0#
      si no         da ((1# << exp_base_natural) - y - b + x) ::+
                       restar_naturales xs ys 1#
  boludo

  el * dados n m
    si n == 0 da 0
    si no da
      natural_div2 n * natural_mul2 m +
      (si natural_es_par n da 0
       si no               da m)

  el / dados n m da pi1 (natural_div_mod n m)

  el % dados n m da pi2 (natural_div_mod n m)
boludo

encarnar Igualable para Natural
  el ==
    dados (NATURAL xs) (NATURAL ys) da xs == ys
boludo

encarnar Ordenable para Natural
  el comparar
    dados (NATURAL xs) (NATURAL ys)
       da mirar comparar_longitudes xs ys
          si MENOR da MENOR
          si MAYOR da MAYOR
          si IGUAL da comparar (dar_vuelta xs) (dar_vuelta ys)
          boludo
  donde
    el comparar_longitudes
     dadas []     []     da IGUAL
     dadas []     (_:_)  da MENOR
     dadas (_:_)  []     da MAYOR
     dadas (_:xs) (_:ys) da comparar_longitudes xs ys
  boludo
boludo

encarnar Mostrable para Natural
  el mostrar dado n
  si n == 0 da "0"
  si no     da mostrar_dígitos (expansion_decimal n [])
  donde
    la expansion_decimal de Natural en [Numerito] en [Numerito]
    dados n acc
    si n == 0 da acc
    si no     da expansion_decimal (n / 10) (dígito (n % 10) : acc)

    el dígito de Natural en Numerito
    dado (NATURAL [])      da 0#
    dado (NATURAL (d : _)) da d

	el mostrar_dígitos de [Numerito] en Texto
	dado [] da ""
	dado (0# : ds) da "0" ++ mostrar_dígitos ds
	dado (1# : ds) da "1" ++ mostrar_dígitos ds
	dado (2# : ds) da "2" ++ mostrar_dígitos ds
	dado (3# : ds) da "3" ++ mostrar_dígitos ds
	dado (4# : ds) da "4" ++ mostrar_dígitos ds
	dado (5# : ds) da "5" ++ mostrar_dígitos ds
	dado (6# : ds) da "6" ++ mostrar_dígitos ds
	dado (7# : ds) da "7" ++ mostrar_dígitos ds
	dado (8# : ds) da "8" ++ mostrar_dígitos ds
	dado (9# : ds) da "9" ++ mostrar_dígitos ds
	dado (_  : ds) da "_" ++ mostrar_dígitos ds

  boludo
boludo

encarnar Digital para Natural
  el digitalizar
    dado n  da []
  el levantar_dígitos dado xs da NATURAL $ rec xs
  donde
    la rec
    dado []       da []
    dado (x : xs) da
      x & máscara_natural ::+
      x >> exp_base_natural ::+
      rec xs
  boludo
boludo

