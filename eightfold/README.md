Eightfold
=========

*1) Bugs exist.*

*2) Bugs arise from attachment to correctness with respect to a specification.*

*3) Bugs cease when attachment to correctness ceases.*

*4) Freedom from bugs is possible by practising the Eightfold Path.*

Introduction
------------

Eightfold is a dependently typed esolang, designed for submission
to [December 2012 PLT Games](http://www.pltgames.com/competition/2012/12).

Being a dependently typed language, types are, in principle, expressive
enough to encode program specifications. Eightfold can also be used
as a logical framework, i.e. a proof assistant for user-defined deductive
systems, encoding, by means of Curry-Howard, propositions as types and
derivations as programs.

Example: factorial
------------------

The following Eightfold program calculates factorials. The query (`?? ...`)
asks for the factorial of 5:

    u=*.
    t::a*,b*.>a>ba=:a*,b*,xa.>bx.
    e::a*,b*.>a>bb=:a*,b*.>a:yb.y.
    f=:au.>aa.
    f2=:au.>afa.
    h=:au.f(fa).
    i::au.fa=:au,xa.x.
    0:h=:au.>(fa)ia.
    s:fh=:nh,au,gfa,za.g(nagz).
    1=s(0).
    l:f2h=:nh,mh.mh(nhs)0.
    2=:au,bu,cu.>(>a>bc)c.
    +::au,bu.>a>b(2ab)
     =:au,bu,xa,yb,cu,g(>a>bc).gxy.
    -1::au,bu.>(2ab)a
      =:au,bu,p(2ab).pa(tab).
    -2::au,bu.>(2ab)b
      =:au,bu,p(2ab).pb(eab).
    o:fh
     =:nh.-2hh(n(2hh)
                (:p(2hh).+hh(s(-1hhp))
                            (l(s(-1hhp))(-2hhp)))
                (+hh(0)1)).
    R:u.Z:R.S:>RR.
    Factorial :>hR=:nh.onRSZ.

    ?? Factorial (s(s(s(s(s(0)))))).

When loaded, the query is answered with 120 (in unary):

    !! Factorial(s(s(s(s(s 0))))) : R = S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S Z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))).

Installation
------------

First make sure the Haskell module `System.Console.Readline` is installed:

    $ cabal install readline

To compile the project with GHC:

    $ ghc --make Main.hs -o eightfold

Overview
--------

Eightfold is a combination of a reduction and a type-checking
engine.
A program is a sequence of declarations and queries.

When Eightfold is started interactively, the following
read-eval-print-loop is displayed:

    $ ./eightfold
     ___ _      _   _    __     _    _
    | __(_)__ _| |_| |_ / _|___| |__| |
    | _|| / _` | ' \  _|  _/ _ \ / _` |
    |___|_\__, |_||_\__|_| \___/_\__,_|
          |___/
    Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>
    Usage: eightfold [options]
    Options:
        file.8f     read the given input file
        -s          do not start toplevel interaction

    8f> 

The following is a very simple interaction:

    8f> t : *            # declare "t" as a new type
    t : *.

    8f> a : t            # declare "a" as a new constant of type "t"
    a : t.

    8f> ? a              # query : what is the type of "a"?
    ! a : t.             # answer: "a" is of type "t"

    8f> ? :xt.x          # query : what is the type of a function that takes
                         #         an "x" of type "t" and returns "x"
    ! :xt.x : >tt.       # answer: it has type ">tt", i.e. it takes
                         #         a term of type "t" and returns a term
                         #         of type "t"

    8f> ?? (:xt.x)a      # query : what is the result of applying the
                         #         identity to "a"
    !! (:xt.x)a : t = a. # answer: it has type "t" and evaluates to "a"

    8f> b : >tt          # declare "b" as a new constructor that
                         # takes a term of type "t" and returns
                         # a term of type "t"
    b : >tt.

    8f> ?? ba            # query : what is the result of applying such
                         #         constructor to a?
    !! ba : t = ba.      # answer: it has type "t" and evaluates to "ba"

    8f> ?? bb            # query : what is the result of applying "b"
                         #         to "b"
                         # (answer is the following error message)

    Error:
    in application bb -- argument has wrong type
    types do not match: t -- >tt in env {b : >tt ; a : t ; t : * ; * : *}

In Eightfold, the variable `*` is globally bound to the basic
kind, i.e. the kind of all data types. The type of `*` is `*`,
which presumably makes the system inconsistent (and thus uninteresting)
from a strictly logical point of view:

    8f> ? *
    ! * : *.

Syntax
------

### Syntax for terms

Eightfold terms are given by a syntax similar to that
of simply typed lambda calculus, with the difference that
type annotations do not belong to a separate syntactic
category, but are regular terms:

    [term] ::= [var]                       # variable
             | [term] [term]               # application
             | : [var] [term] . [term]     # abstraction

In terms of lexical syntax, a variable is either a lowercase character
followed by zero or more digits (`[a-z][0-9]*`),
a sequence of digits (`[0-9]+`),
a symbol (`[_+-*/]`), or an uppercase character followed by
lowercase characters and digits (`[A-Z][_a-z0-9]*`).
Notice that two uppercase characters
in the same identifier are not allowed, which is for `FooBar` to
be parsed as `Foo Bar`.

The associativity and precedence rules are those usual in
lambda-calculus (application is left-associative and has
higher precedence than abstraction). As usual,
free occurrences of `x` in `N` are bound in `:xM.N`.

For instance, `:XInt.ConsXNil` is an abstraction
that takes an `X` of type `Int` and applies `Cons` to
`X` and `Nil`. (This could, for example, be a function that
takes an integer and builds a singleton).

Beyond the syntax presented above, Eightfold allows
two abbreviations for denoting abstractions:

`> [term1] [term2]` is an abbreviation for
`: _ [term1] . [term2]`, where `_` is a variable that does
not occur free in `[term2]`. Thus, `>ta` is a function that
takes any term of type `t` and returns `a`.

`: [var1] [type1], [...] . [body]` is
an abbreviation for
`: [var1] [type1] . : [...] . [body]`.
Thus, `:XBool,YBool.AndXY` abbreviates
`:XBool.:YBool.AndXY`.

### Syntax for programs

An Eightfold program is a sequence of rules and queries,
terminated by periods.

By Curry-Howard, one can view an Eightfold script
either as a program or as a proof. Most language
constructs can be analyzed under the light of
two possible interpretations:

* Seen as a programming language, terms encode programs
  and types encode the usual data types. In this case
  the focus is usually centered in the dynamic semantics
  of the programs, i.e. one is interested in normalizing
  a term to know its normal form.

* Seen as a proof assistant, types encode propositions
  and terms encode proofs. Here, one is usually
  less interested in normalizing terms, and more
  interested in typechecking a term to ensure it effectively
  has a certain type.

Facts can be of three forms:
* type declarations: `var : type` which adds the assumption that `var` has type `type`
* type/value declarations: `var : type = term` which binds `var` to `term`,
  checking that `term` is of type `type`
* value declarations: `var = term` which binds `var` to `term`
  (checking that `term` is typable)

Queries can be of two forms:
* type queries: `? term` which returns `! term : type`
* type/value queries: `?? term` which returns `!! term : type = normal_form`

#### Type declarations

Type declarations are of the form `[var] : [term]`.

Under the program interpretation, this declares a basic type,
type constructor, constant or constructor. For example:

    # Declare "Bool" as a basic type and "True" and "False"
    # as boolean constants:

    Bool : *.

    True : Bool.
    False : Bool.

    ? True.
    # ! True : Bool.

    # Declare "List" as type constructor,
    # so that "List Bool" is a type.
    List : >**.

    # Declare "Nil" as a constructor that given a type "a"
    # returns a list of "a".
    # For instance "Nil Bool" denotes the empty list of booleans.
    Nil : :a*.List a.

    ? Nil Bool.
    # ! Nil Bool : List Bool.

    # Declare "Cons" as a function that given a type "a", an
    # element of type "a", and a list of "a" returns another
    # list of "a". For instance "Cons Bool True (Nil Bool)"
    # denotes the singleton list `[True]`.
    Cons : :a*.>a>(List a)(List a).

    ? Cons Bool True (Cons Bool False (Nil Bool)).
    # ! Cons Bool True (Cons Bool False (Nil Bool))
    #   : List Bool.

When this program is loaded, Eightfold checks that all
declarations are correct, and prints them out.
Declarations are read from top to bottom.
For a declaration to be correct, the right hand side,
i.e. the type, has to have a kind in the current context
(given by all previous declarations).

Under the proof assistant interpretation, this declares
axioms and inference rules in the system.
For example:

    # Declare a datatype for representing strings of bits.
    # For example "1(0(1(1 Empty)))" is a term of type
    # "Bits" representing the binary string "1011".

    Bits : *.
    Empty : Bits.
    0     : > Bits Bits.
    1     : > Bits Bits.

    ? 0(1(0(1 Empty))).
    # ! 0(1(0(1 Empty))) : Bits.

    # Declare a predicate for representing the statement
    # that a given string of bits ends in zero.
    #
    # For example:
    #     Ends_in_0 (1(0 Empty))
    # represents the fact that "10" ends in "0". While:
    #     Ends_in_0 (1(1(1 Empty)))
    # represents the fact that "111" ends in "0".
    #
    # Note that a term need not represent a true statement to
    # be well formed.

    Ends_in_0 : > Bits *.

    ? Ends_in_0 (0(1 Empty)).
    # ! Ends_in_0 (0(1 Empty)) : *.

    # The following definitions represent a formal system
    # with one axiom and two rules.
    #
    # These rules ensure that only the true statements
    # are inhabited. Well, technically not, since Eightfold
    # is probably inconsistent anyway :)
    #
    # The axiom states that "0" ends in 0.
    # Rule0 states that if w ends in 0, also does 0w.
    # Rule1 states that if w ends in 0, also does 1w.

    Axiom: Ends_in_0 (0 Empty).
    Rule0: :w Bits. > (Ends_in_0 w) (Ends_in_0 (0 w)).
    Rule1: :w Bits. > (Ends_in_0 w) (Ends_in_0 (1 w)).

    ? Rule1 (0(0 Empty)) (Rule0 (0 Empty) Axiom).
    # ! Rule1 (0(0 Empty)) (Rule0 (0 Empty) Axiom)
    #   : Ends_in_0 (1(0(0 Empty))).

#### Type/value declarations

Type/value declarations are of the form `[var] : [term1] = [term2]`,
where `[term1]` represents a type and `[term2]` represents a value.

Interpreted as a program, this fact defines a new binding, which
checks that `[term2]` has type `[term1]` in the current environment,
and binds `var` to `term2`.

For instance, the usual way of encoding booleans in the
lambda-calculus can be typechecked in Eightfold:

    # Define Bool as the type ":a*.>a>aa", i.e. the function that 
    # takes a type "a", two terms of type "a" and returns the type
    # "a".
    # 
    Bool : :a*.>a>a* = :a*.>a>aa.

    # Define True as the term of type Bool that takes
    # a type "a", two terms of type "a" and returns the
    # first one.
    True : Bool = :a*,xa,ya.x.

    # Similarly, define False as the term of type Bool
    # that takes a type "a", two terms of type "a" and
    # returns the second one.
    False : Bool = :a*,xa,ya.y.

    ? True.
    # ! True : Bool.

    # Define If as a term that takes a type "a", a boolean
    # and two terms of type "a", returning the first one in
    # case the boolean is True, and the second one otherwise.
    If : :a*.>Bool>a>aa
       = :a*,bBool,xa,ya.baxy.

    # Declare a type S with two constants A and B for testing.
    S : *. A : S. B : S.

    ?? If S True A B.
    # !! If S True A B : S = A.

    ?? If S False A B.
    # !! If S False A B : S = B.

With the proof assistant interpretation, a type/value declaration
is an assertion of a theorem, along with a term that encodes
its proof in the system. For instance, the example that defines
the `Ends_in_0` predicate above can be completed with a proof
that the string "010" ends in zero:

    # ... previous declarations

    Theorem_010_ends_in_0
      : Ends_in_0 (0(1(0 Empty)))
      = Rule0 (1(0 Empty)) (Rule1 (0 Empty) Axiom).

The program is loaded with no errors, which means the given
term effectively has type `Ends_in_0 (0(1(0 Empty)))`,
i.e. it encodes a derivation for the given proposition.

#### Value declarations

Value declarations are of the form `[var] = [term]`.
This is just a convenience to abbreviate type/value declarations,
since the type of the term can be inferred. For example, instead
of writing:

    Bool : :a*.>a>a* = :a*.>a>aa.

one can write:

    Bool = :a*.>a>aa.

    ? Bool.
    # ! Bool : :a*>a>a*.

