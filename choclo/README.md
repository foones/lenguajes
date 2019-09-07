# choclo -- bootstrapping compiler

runtime/   --- Archivos de soporte en C.
prechoclo/ --- Compilador Choclo => C escrito en Common Lisp.
pochoclo/  --- Compilador Choclo => C escrito en Choclo.

Prechoclo: compilador Choclo => C escrito en Common Lisp:
  prechoclo foo.ch
  Compila foo.ch y deja un binario en foo/foo

Pochoclo: compilador Choclo => C escrito en Choclo:
  bootstrap.ch
  => strap.exe
  chocstrap <archivo>
  Compila <archivo>.ch

Bootstrapping:

- Paso 1
Compilar el compilador Choclo => C escrito en Choclo mediante
el compilador Choclo => C escrito en Clisp:

  ./prechoclo src/pochoclo/pochoclo.ch

  Resulta en el archivo:
  ./_prechoclo_output/pochoclo/pochoclo

- Paso 2

  ./pochoclo examples/test/test01

  Resulta en el archivo:
  ./_pochoclo_output/test01/test01
