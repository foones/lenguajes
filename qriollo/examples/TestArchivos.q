
enchufar Chamuyo
enchufar Archivos

el probarSalida es
  che el archivo es abrirArchivo "test.txt" Escritura
  che archivo->escupirle "Hola múndo\n"
  che archivo->cerrarlo
  en fija "OK"

el probarEntrada es
  che el archivo es abrirArchivo "test.txt" Lectura
  che la línea   es archivo->leerLínea
  che archivo->cerrarlo
  en fija línea

el vomitar
  dado x da escupir (mostrar x);
            escupir "\n"

el programa es vomitar (
  che probarSalida
  che la línea es probarEntrada
  en fija línea
)

