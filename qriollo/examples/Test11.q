
enchufar Chamuyo

una Pila de coso
tiene
  la representación de [coso]
boludo

el vacía de () en Ref de (Pila de coso)
  dado () da
    Ref (Pila cuya representación es [])

el apilar de coso
          en Ref de (Pila de coso)
          en Ref de (Pila de coso)
dados x yo da
  mirar desref yo
  si Pila cuya representación es xs
  da yo := (Pila cuya representación es x : xs); yo
  boludo

encarnar Mostrable para Pila de coso
                   con Mostrable para coso
  el mostrar dada Pila cuya representación es xs
               da "Pila (" ++ mostrar xs ++ ")"
boludo

el programa es
  ponele que la p de Ref de (Pila de Letra)
                  es vacía ()
  en
    p->apilar 'c';
    p->apilar 'b';
    p->apilar 'a';
    escupir (mostrar p);
    escupir_letra '\n'

