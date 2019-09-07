
enchufar Chamuyo

chirimbolo zurdo 450 <==
chirimbolo zurdo 450 ===

cualidad Igualable para coso
  el === de coso en coso en Posta
boludo

encarnar Igualable para Numerito
  el === es PRIM.igual_numerito
boludo

encarnar Igualable para [coso]
                    con Igualable para coso
  el ===
  dadas []     []     da Sí
  dadas []     _      da No
  dadas (x:xs) []     da No
  dadas (x:xs) (y:ys) da x === y && xs === ys
boludo

cualidad Ordenable para coso
  el <== de coso en coso en Posta
boludo

encarnar Ordenable para Numerito
  el <== es PRIM.menor_o_igual_numerito
boludo

encarnar Ordenable para [coso]
                    con Ordenable para coso
  el <==
  dadas []     _      da Sí
  dadas (x:xs) []     da No
  dadas (x:xs) (y:ys) da x <== y && xs <== ys
boludo

el programa es
  escupir (mostrar ([1#,2#] <== [1#,2#,3#]))

