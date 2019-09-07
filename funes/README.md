# Funes

Installation

  0) Extract Funes in a directory "<funes-path>".

  1) Compile Funes.

     Currently, Funes works under unixen with gcc, and under
     Windows with mingw32. The installation is still very
     manual, using make.

     $ make

  2) [Unix only]
     Create a soft link to the shared object:

     $ ln -s <funes-path>/src/libfunes.so /usr/local/lib/libfunes.so

     you must be superuser to create the link.

  3) Set the environment variable FUNES_PATH to point
     to the root Funes directory.

     $ export FUNES_PATH=<funes-path>

     At this point, you should be able to run:
       <funes-path>/funes
     and get a Funes banner and a prompt with no warnings.

     Add the variable definition to your ~/.bashrc

  4) [Optional]
     Compile the libraries.

     Currently the libraries must be compiled by hand, and
     require gcc (linux).

     A *.fub file is a binding definition for a C library.

     To compile a "foo.fub" library, just do:
       <funes-path>/funes foo.fub

     that should create an associated dynamic library called
     "foo.so".

  5) Happy Funes hacking!

--

Notes:

  Funes requires Hans Boehm's conservative garbage
  collector (libgc).

