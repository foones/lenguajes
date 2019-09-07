
enchufar Chamuyo

GRINGO C "typedef unsigned long long *ArregloDeNumeritos;"

gringo C "malloc(${1} * sizeof(ArregloDeNumeritos))"
  arreglo_de_numeritos de Numerito en Pendorcho "ArregloDeNumeritos"

gringo C "free(${1})"
  liberar_arreglo_de_numeritos de Pendorcho "ArregloDeNumeritos" en ()

gringo C "(${1}[${2}] = ${3})"
  meter de Pendorcho "ArregloDeNumeritos" en Numerito en Numerito en ()

gringo C "(${1}[${2}])"
  dame de Pendorcho "ArregloDeNumeritos" en Numerito en Numerito

gringo C "printf(\"%s\", ${1})"
  etf de Texto en ()

gringo C "getenv(${1})"
  getenv de Texto en Texto

el programa es
   etf "Hola mundo\n";
   meter a 0# 0#;
   meter a 1# 1#;
   meter a 2# 4#;
   meter a 3# 9#;
   escupir . mostrar $
     [dame a 0#, dame a 1#, dame a 2#, dame a 3#];
   escupir "\n";
   liberar_arreglo_de_numeritos a;
   escupir (getenv "PATH"); escupir "\n"
donde
   el a es arreglo_de_numeritos 100#
boludo

