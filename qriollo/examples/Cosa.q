
chirimbolo diestro 25 ||
chirimbolo diestro 35 &&
chirimbolo zurdo   50 ++
chirimbolo diestro 55 :
chirimbolo zurdo   90 .
chirimbolo zurdo   90 ..

la identidad de coso en coso
  dado x da x

el siempre de coso en cosito en coso
  dados x y da x

la . de (mengano en zutano)
     en (fulano en mengano)
     en fulano en zutano
  dados f g x da f (g x)

la .. de (zutano en perengano)
      en (fulano en mengano en zutano)
      en fulano en mengano en perengano 
  dados f g x y da f (g x y)

el voltear de (fulano en mengano en zutano) en
               mengano en fulano en zutano
  dados f x y da f y x

una Posta es
    bien No
    bien Sí

el && de Posta en Posta en Posta
  dado Sí da identidad
  dado No da siempre No

el || de Posta en Posta en Posta
  dado Sí da siempre Sí
  dado No da identidad

el si_no de Posta en coso en coso en coso
  dados Sí a _ da a
  dados No _ b da b

una Lista de a es
    bien Vacía
    bien : a (Lista de a)

el plegar de (coso en cosito en cosito)
          en cosito
          en Lista de coso en cosito
  dadas f z Vacía    da z
  dadas f z (x : xs) da f x (plegar f z xs)

el aplicar de (coso en cosito)
           en Lista de coso
           en Lista de cosito
  es voltear plegar Vacía . (el .) (el :)

el ++ de Lista de coso
      en Lista de coso
      en Lista de coso
  es plegar ((el .) . (el :)) identidad

el emparejar de Lista de coso
             en Lista de cosito
             en Lista de (coso, cosito)
  es plegar (la que dados _ _ Vacía    da Vacía
                    dados x e (y : ys) da (x, y) : e ys)
            (siempre Vacía)

el filtrar de (coso en Posta)
           en Lista de coso
           en Lista de coso
  dados p Vacía    da Vacía
  dados p (x : xs) da
    mirar p x
    si Sí da x : filtrar p xs
    si No da     filtrar p xs
    boludo

el yy de Lista de Posta
      en Posta
  es plegar (el &&) Sí

el oo de Lista de Posta
      en Posta
  es plegar (el ||) No

el todos de (coso en Posta)
         en Lista de coso
         en Posta
  es yy .. aplicar

el alguno de (coso en Posta)
          en Lista de coso
          en Posta
  es oo .. aplicar

un AB de coso es
   bien Nil
   bien Bin coso (AB de coso) coso

cualidad Comparable para a
  el comparar de a en a en Bool
boludo

el programa
  es
    emparejar (1# : 2# : 3# : Vacía)
              (4# : 5# : 6# : Vacía)

