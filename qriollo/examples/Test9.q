enchufar Chamuyo

el comentario es "ejecutar con --manija 0 --malpensado"

un Acaso de coso es
  bien Nada
  bien Este coso

una Promesa de coso
tiene
  el evaluar_promesa de () en coso
boludo

el traeme de Promesa de coso en coso
  dada promesa da evaluar_promesa promesa ()

el bancame de (() en coso) en Promesa de coso
  dada evaluar da
    Promesa cuyo evaluar_promesa es
      ponele que la caja es Ref Nada en {
        mirar desref caja
        si Nada   da ponele que el valor es evaluar () en
                       caja := Este valor;
                       valor
        si Este x da x
        boludo
      }

una ListaPajera de coso es
bien AgregarPajeramente coso (Promesa de (ListaPajera de coso))

las alternar de Letra en Letra en ListaPajera de Letra
dadas a b
   da ponele que
        la empieza_con_a es
          AgregarPajeramente a (bancame {empieza_con_b})
        la empieza_con_b es
          AgregarPajeramente b (bancame {empieza_con_a})
      en
        empieza_con_a

el dame de Numerito en ListaPajera de coso en [coso]
dados 0# _ da []
dados n (AgregarPajeramente x xs) da x : dame (n - 1#) (traeme xs)

el escupir_letras de [Letra] en ()
dado []     da ()
dado (x:xs) da escupir_letra x;
               escupir_letras xs

el programa es
  escupir_letras (dame 10# (alternar 'a' 'b'));
  escupir_letra '\n'

