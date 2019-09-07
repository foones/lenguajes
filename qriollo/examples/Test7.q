enchufar Chamuyo

un T es bien A Letra
        bien B T Letra T

el preorder de T en [Letra]
  dado (A x)       da [x]
  dado (B t1 x t2) da [x] ++ preorder t1 ++ preorder t2

el inorder de T en [Letra]
  dado (A x)       da [x]
  dado (B t1 x t2) da inorder t1 ++ [x] ++ inorder t2

el postorder de T en [Letra]
  dado (A x)       da [x]
  dado (B t1 x t2) da postorder t1 ++ postorder t2 ++ [x]

el programa es
  escupir (
    Texto (
     preorder árbol ++ ['.'] ++
     inorder árbol ++ ['.'] ++
     postorder árbol ++ ['.']
    ) ++ "\n"
  )
donde el árbol es (B (B (A 'a') 'b' (A 'c'))
                     'd'
                     (B (A 'e') 'f' (A 'g')))
boludo

