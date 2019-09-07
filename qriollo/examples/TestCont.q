enchufar Chamuyo

las alternativas de Ref de [coso] son Ref [] 

el colgarse dado ()
  da colgarse ()

el fallar de () en coso
  dado () da
   mirar desref alternativas
     si (alt : alts) da
       alternativas := alts;
       alt ()
     si [] da colgarse ()
   boludo

el map de (coso en cosito) en [coso] en [cosito]
  es fap

el elegir_no_determinísticamente de [coso] en (coso en cosito) en cosito
  dados valores f
     da f (ccc (ante k da
                  alternativas := map (ante valor _ da invocar k valor) valores ++
                                  desref alternativas;
                  fallar ()))

el programa es
   elegir_no_determinísticamente [1#, 2#, 3#, 4#, 5#] (ante a da
   elegir_no_determinísticamente [1#, 2#, 3#, 4#, 5#] (ante b da
   (si a + b != 5# da fallar () si no da ());
   escupir . mostrar $ (a, b); escupir "\n";
   fallar ()))

