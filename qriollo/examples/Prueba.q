chirimbolo diestro 10 :
chirimbolo zurdo   30 ++
chirimbolo zurdo   90 :=

una Lista de coso es
  bien Vacía 
  bien : coso [coso] 

el ++ de [coso] en [coso] en [coso] 
  dada []       ys da ys 
  dada (x : xs) ys da x : (xs ++ ys) 
el := es asignar 

una Empanada es 
  bien Carne 
  bien Pollo 
  bien Caprese 
  bien Humita 
  bien Verdura 

el nombre de Empanada en [Letra] 
 dada Carne   da ['C','a','r','n', 'e'] 
 dada Pollo   da ['P','o','l','l', 'o'] 
 dada Caprese da ['C','a','p','r','e','s','e'] 
 dada Humita  da ['H','u','m','i','t','a'] 
 dada Verdura da ['V','e','r','d','u','r','a'] 

el juntar de [[Letra]] en [Letra] en [Letra] 
  dada []     _   da [] 
  dada [x]    _   da x  
  dada (x:xs) sep da x ++ sep ++ juntar xs sep 

el aplicarf de (coso en cosito) en [coso] en [cosito] 
dadas f []       da [] 
dadas f (x : xs) da f x : aplicarf f xs 

el programa es 
  juntar 
    (aplicarf nombre [Verdura, Humita, Caprese, Pollo, Carne]) 
    [',', ' ']

