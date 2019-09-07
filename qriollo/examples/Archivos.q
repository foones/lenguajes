
entregar
  (
    Archivo,
    Modo,
      Lectura,
      Escritura,
    abrirArchivo,
    escupirle,
    leerLínea,
    cerrarlo
  )

OJO. La implementación actual está maldita:
OJO.
OJO. - Leer una línea en Python incluye el '\n' final
OJO.   pero en Java no.
OJO.
OJO. - Si se llegó al final del archivo, Python devuelve ""
OJO.   mientras que Java eleva una excepción.
OJO.

enchufar Chamuyo

un Modo es
  bien Lectura
  bien Escritura

encarnar Mostrable para Modo
  el mostrar
    dado Lectura   da "Lectura"
    dado Escritura da "Escritura"
boludo

SI (+Py)

    un Archivo es
      bien ArchivoPy Texto Modo Pendorcho

    encarnar Mostrable para Archivo
      el mostrar
        dado (ArchivoPy nombre modo _) 
          da "<archivo \"" ++ nombre ++ "\" " ++ mostrar modo ++ ">"
    boludo

    gringo Py "open(${1}, ${2})" _abrirArchivo
      de Texto
      en Texto
      en Falible de Pendorcho

    gringo Py "${1}.write(${2}.encode('utf-8'))" _escupirEn
      de Pendorcho
      en Texto
      en Falible de ()

    gringo Py "${1}.readline().decode('utf-8')" _leerLíneaDe
      de Pendorcho
      en Falible de Texto

    gringo Py "${1}.close()" _cerrarArchivo
      de Pendorcho
      en Falible de ()

    el abrirArchivo de Texto
                     en Modo
                     en Falible de Archivo
      dadas ruta modo
         da mirar _abrirArchivo ruta (m modo)
            si Joya a   da Joya (ArchivoPy ruta modo a)
            si Cagó msj da Cagó msj
            boludo
      donde
        el m
          dado Lectura   da "r"
          dado Escritura da "w"
      boludo

    el escupirle de Texto
                 en Archivo
                 en Falible de ()
      dados texto (ArchivoPy _ Escritura archivo)
         da _escupirEn archivo texto
      dados texto (ArchivoPy _ Lectura _)
         da Cagó "No se puede escribir en un archivo para lectura."

    el leerLínea de Archivo
                  en Falible de Texto
      dado (ArchivoPy _ Lectura archivo)
         da _leerLíneaDe archivo
      dado (ArchivoPy _ Escritura _)
         da Cagó "No se puede leer de un archivo para escritura."

    el cerrarlo de Archivo
                en Falible de ()
      dado (ArchivoPy _ _ archivo)
         da _cerrarArchivo archivo

BOLUDO

SI (+Jvm)

    un Archivo es
      bien ArchivoLectura Texto (Pendorcho "java/io/FileReader")
      bien ArchivoEscritura Texto (Pendorcho "java/io/FileWriter")

    encarnar Mostrable para Archivo
      el mostrar
        dado (ArchivoLectura nombre _)
          da "<archivo \"" ++ nombre ++ "\" Lectura>"
        dado (ArchivoEscritura nombre _)
          da "<archivo \"" ++ nombre ++ "\" Escritura>"
    boludo

    gringo Jvm "new" _escritorDeArchivos
      de Texto
      en Pendorcho "java/io/FileWriter"

    gringo Jvm "new" _escritorDeACachos
      de Pendorcho "java/io/Writer"
      en Pendorcho "java/io/BufferedWriter"

    gringo Jvm "write" _escupir
      de Pendorcho "java/io/Writer"
      en Texto
      en ()

    gringo Jvm "close" _cerrarEscritor
      de Pendorcho "java/io/BufferedWriter"
      en ()

    gringo Jvm "new" _lectorDeArchivos
      de Texto
      en Pendorcho "java/io/FileReader"

    gringo Jvm "new" _lectorDeACachos
      de Pendorcho "java/io/Reader"
      en Pendorcho "java/io/BufferedReader"

    gringo Jvm "readLine" _chuparLínea
      de Pendorcho "java/io/BufferedReader"
      en Texto

    gringo Jvm "close" _cerrarLector
      de Pendorcho "java/io/BufferedReader"
      en ()

    el abrirArchivo de Texto
                    en Modo
                    en Falible de Archivo
      dadas ruta Lectura
         da Joya $
              ArchivoLectura
                ruta
                (_lectorDeACachos (_lectorDeArchivos ruta))
      dadas ruta Escritura
         da Joya $
              ArchivoEscritura
                ruta
                (_escritorDeACachos (_escritorDeArchivos ruta))

    el escupirle de Texto
                 en Archivo
                 en Falible de ()
      dados texto (ArchivoEscritura _ archivo)
         da Joya $ _escupir archivo texto
      dados texto (ArchivoLectura _ archivo)
         da Cagó "No se puede escribir en un archivo para lectura."

    el leerLínea de Archivo
                  en Falible de Texto
      dado (ArchivoLectura _ archivo)
         da Joya $ _chuparLínea archivo
      dado (ArchivoEscritura _ archivo)
         da Cagó "No se puede leer de un archivo para escritura."

    el cerrarlo de Archivo
                en Falible de ()
      dado (ArchivoLectura _ archivo)
         da Joya $ _cerrarLector archivo
      dado (ArchivoEscritura _ archivo)
         da Joya $ _cerrarEscritor archivo

BOLUDO

