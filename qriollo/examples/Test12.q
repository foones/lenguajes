
enchufar Chamuyo




cualidad Pila para bolsa
  la vacía de () en Ref de (bolsa de coso) 
  el apilar de coso
            en Ref de (bolsa de coso) 
            en Ref de (bolsa de coso) 
boludo





una Pila_Lista de coso
tiene
  la representación de [coso]
boludo

encarnar Pila para Pila_Lista

  el vacía
    dado () da
      Ref (Pila_Lista cuya representación es [])

  el apilar
    dados x yo da
      mirar desref yo
      si Pila_Lista cuya representación es xs
      da yo := (Pila_Lista cuya representación es x : xs);
         yo
      boludo

boludo

encarnar Mostrable para Pila_Lista de coso
                   con Mostrable para coso
  el mostrar dada Pila_Lista cuya representación es xs
               da "Pila_Lista (" ++ mostrar xs ++ ")"
boludo




una Pila_Nativa de coso es
  bien Pila_Nativa_Vacía
  bien Pila_Nativa_Apilar coso (Pila_Nativa de coso)

encarnar Pila para Pila_Nativa
  el vacía dado () da Ref Pila_Nativa_Vacía
  el apilar dados x yo da
    yo := Pila_Nativa_Apilar x (desref yo);
    yo
boludo

encarnar Mostrable para Pila_Nativa de coso
                   con Mostrable para coso
  el mostrar
  dada Pila_Nativa_Vacía da "<>"
  dada (Pila_Nativa_Apilar p ps) da
    mostrar p ++ " " ++ mostrar ps
boludo



el programa es
  ponele que la p de Ref de (Pila_Nativa de Letra)
                  es vacía ()
  en
    p->apilar 'c';
    p->apilar 'b';
    p->apilar 'a';
    escupir (mostrar p);
    escupir_letra '\n'

