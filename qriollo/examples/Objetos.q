
enchufar Chamuyo

chirimbolo zurdo 10 #
chirimbolo zurdo 10 :#

un Id es
   bien IdNil
   bien Id_

un Mensaje es
   bien Mensaje Texto

un Método es
  bien Método ([Objeto] en Objeto)

un Id es
  bien Id Numerito
 
un Objeto es
  bien Objeto Id (Mensaje en [Objeto] en Objeto)

el nil    es Objeto IdNil (el que dados _ _ da nil)

el dicc_metaclase es (
    el que
      dados _ _ da
        nil
  )

el dicc_clase_objeto es (
    el que
      dados (Mensaje "clase") [self] da
        clase_objeto
      dados _ _ da
        nil
  )

la clase_objeto es Objeto Id_ dicc_metaclase

el objeto es Objeto Id_ dicc_clase_objeto

encarnar Mostrable para Objeto
  el mostrar
    dado (Objeto IdNil _) da "nil"
    dado _                da "<obj>"
boludo

el #  dados (Objeto id f) msj      da f (Mensaje msj) [Objeto id f]
el :# dados (Objeto id f) msj args da f (Mensaje msj) (Objeto id f : args)

el programa es
  escupir (mostrar res)
donde
  el res es   objeto # "clase"
boludo

