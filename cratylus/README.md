Cratylus
========

*Si, como el griego afirma en el Cratilo,*  
*el nombre es arquetipo de la cosa,*  
*en las letras de* rosa *está la rosa*  
*y todo el Nilo en la palabra* Nilo.  

Introduction
------------

Cratylus is a polynomial rewriting esolang, designed
for submission to [December 2012 PLT Games](http://www.pltgames.com/competition/2012/12).

A Cratylus program is a sequence of rewrite rules between multivariate polynomials.
For instance, the following program has only one rule:

    xy => z.

Given a query:

    ? x^2 y^2.

it gets rewritten to `xyz`, and this in turn to `z^2`,
which is the normal form.
More precisely, a Cratylus program is a sequence of rules, of the form:

    R1 => S1.
    R2 => S2.
    ...
    Rn => Sn.

where each `Ri` and `Si` are multivariate polynomials with integer coefficients.
Given a polynomial `P`, let `i` be the least index such that `Ri` divides `P`,
that is, `P = Ri Q`. Then `P` gets rewritten to `Si Q`.
If there is no such index, `P` is in normal form.

Attempts to catch your attention
--------------------------------

This Cratylus program computes factorials:

    H=>mZ.am=>af.m=>J.af=>aB.f=>k.aB=>u.B=>u.u=>cL.L=>eG.G=>f.
    ck=>cr.k=>d.cr=>b.r=>b.bZ=>yZ.b=>j.yZ=>t.y=>t.t=>iK.K=>lE.
    E=>b.ij=>is.j=>I.is=>x.s=>x.x=>DZ.D=>j.I=>k.dZ=>wZ.d=>h.
    wZ=>o.w=>o.o=>d.hl=>lp.h=>g.lp=>v.p=>v.v=>CZ.C=>h.eg=>en.
    g=>q.en=>z.n=>z.z=>aF.F=>g.aq=>A.q=>A.A=>m.J.

For instance, interacting with the Cratylus toplevel:

    ? H a^5
    Z^120

    ? H a^7
    Z^5040

Bear in mind the second query takes a *really* long time to arrive
to the answer. The reason for such slowness is that, internally,
additions and multiplications are carried out in unary, and that
takes a number of steps that grows exponentially with the length
of the numbers involved. By using the Cratylus to C
compiler, one can speed up the constant factors to compute up to
the factorial of 10 (`Z^3628800`) in a few seconds.

Although there seems to be no way of improving the algorithm
to carry out additions other than in unary, it is still possible
to adapt it to behave linearly up to a "big" number.
In unary one knows that "1 + 1 = 2". The key to speed up the
process is also acknowledging the facts that "10 + 10 = 20",
"100 + 100 = 200", etc.
If one could represent those facts for arbitrary powers of 10,
the program would effectively perform additions in a linear
number of steps. Unfortunately, Cratylus is not expressive enough
for us to be able to state that equations in general, but only
up to a given fixed number (NB: below we describe Cratylus^@,
a variant that allows expressing precisely that kind of rewrite
rules).
For example, one could write a version of factorial improved to work
"fast" up to 64-bit integers. See
[the code](https://github.com/foones/cratylus/blob/master/examples/fast_factorial.compact.cr).

The following Cratylus program is able to calculate the n-th prime
number. The main goal, `? BcE^20.`, queries for the 20-th prime number:

    Bc=>{_}G.EG=>BhE.G=>Bd.Bh=>{_}Aj.{_}Aj=>{_}Am.Aj=>e.
    {_}Am=>F.Am=>F.{_}F=>{_}M.F=>N.{_}M=>n.M=>n.n{_}=>{_}Bb.
    n=>Be.Bb=>{_}Az.Az=>m{_}.m{_}=>{_}L.m=>y.{_}L=>Y.L=>Y.
    Y=>aBa.Ba=>xBk.Bk=>m.xy=>xAp.y=>I.xAp=>Al.Ap=>Al.Al=>{_}Ax.
    Ax=>y.aI=>s.I=>s.as=>h.s=>h.ah=>aBi.h=>Bj.Bi=>aq.
    q{_}=>{_}An.q=>v.{_}An=>Ab.An=>Ab.Ab=>fAw.Aw=>oBl.Bl=>q.
    ov=>oK.v=>u.oK=>Ai.K=>Ai.Ai=>{_}Bg.Bg=>v.au=>aV.u=>D.aV=>Q.
    V=>Q.Q=>cAv.Av=>tBp.Bp=>u.tD=>tAf.D=>w.tAf=>Aa.Af=>Aa.
    Aa=>aBn.Bn=>D.fw=>fz.w=>B.cz=>cAg.z=>C.cAg=>W.Ag=>W.W=>dAt.
    At=>bAy.Ay=>z.dC=>dAq.C=>r.fAq=>fAd.Aq=>i.fAd=>l.Ad=>l.
    dl=>H.l=>H.H=>C.br=>bS.r=>Bo.bS=>Ah.S=>Ah.Ah=>cBf.Bf=>r.
    Bo=>w.cB=>cP.B=>i.cP=>J.P=>J.J=>B.di=>dO.i=>Ac.dO=>p.O=>p.
    bp=>X.p=>X.X=>i.bAc=>bA.Ac=>e.bA=>bAk.A=>Z.bAk=>Ar.Ak=>Ar.
    Ar=>A.aZ=>k.Z=>k.ak=>U.k=>U.U=>h.Bj=>gAu.Au=>e.Be=>gBm.
    Bm=>{_}N.N=>e{_}.ae=>aAo.e=>Ae.aAo=>T.Ao=>T.T=>e.gAe=>gAs.
    Ae=>j.AsE=>j.As=>j.gj=>R.j=>R.R=>G.Bd.

    ? BcE^20.

Again, the results take a very long time with the standard
interpreter. By using the Cratylus to C compiler, the
answer is:

    {_}^71
 
which is, in fact, the 20-th prime number.

The programs above make heavy use of the fact that Cratylus
allows working with multivariate polynomials. One may wonder
if there is a way of writing programs using only univariate
polynomials. Indeed, the following is also a Cratylus program that
calculates factorials:

    x - 17 => x^2 + 7x.
    x^2 + 8x + 7 => x^2 - 2x - 3.
    x + 7 => x - 18.
    x^2 - 2x - 3 => x^2 - 13x - 14.
    x - 3 => x + 6.
    x^2 - 13x - 14 => x + 11.
    x - 14 => x + 11.
    x + 11 => x^2 - 17x - 38.
    x - 19 => x^2 + 20x + 51.
    x + 17 => x - 3.
    x^2 + 8,x + 12 => x^2 - 7x - 18.
    x + 6 => x - 2.
    x^2 - 7x - 18 => x - 1.
    x - 9 => x - 1.
    x^2 - x => x^2 + 13x.
    x - 1 => x - 5.
    x^2 + 13x => x - 10.
    x + 13 => x - 10.
    x - 10 => x^2 + 24x + 95.
    x + 19 => x^2 + 10x - 96.
    x + 16 => x - 1.
    x^2 - 25 => x^2 + 15x + 50.
    x - 5 => x + 18.
    x^2 + 15x + 50 => x - 12.
    x + 10 => x - 12.
    x - 12 => x^2 - 15x.
    x - 15 => x - 5.
    x + 18 => x + 6.
    x^2 - 2x => x^2 + 12x.
    x - 2 => x - 4.
    x^2 + 12x => x + 8.
    x + 12 => x + 8.
    x + 8 => x - 2.
    x^2 - 10x + 24 => x^2 - 14x + 48.
    x - 4 => x + 4.
    x^2 - 14x + 48 => x - 11.
    x - 8 => x - 11.
    x - 11 => x^2 + 15x.
    x + 15 => x - 4.
    x^2 + 7x + 12 => x^2 - 4x - 21.
    x + 4 => x + 9.
    x^2 - 4x - 21 => x - 13.
    x - 7 => x - 13.
    x - 13 => x^2 - 15x - 16.
    x - 16 => x + 4.
    x^2 + 10x + 9 => x + 14.
    x + 9 => x + 14.
    x + 14 => x + 7.
    x - 18.

For instance, on the following input:

    ? (x - 17) (x + 1)^3

it calculates 3! = 6:

    x^6

For reasons not so evident to me, this version is orders
of magnitude slower. This probably relates with the fact
that polynomial multiplication and division take quadratic
time on the degree.

Even though, superficially, our two factorial programs are
very different, in essence they are exactly the same program.
They can be put in correspondence by a relation like the
following:

    H <==> x - 17
    m <==> x + 7
    Z <==> x
    a <==> x + 1
    f <==> x - 3
    ...

See, for example, that the first rewrite rule of the first
factorial program was `H => m Z`, which in the new program
can be read as `x - 17 => (x + 7) x`, i.e.
`x - 17 => x^2 + 7x`.

Features
--------

As part of the Cratylus distribution, the following scripts
are provided:

* `cratylus.py` -- the Cratylus toplevel interpreter.

* `tools/simp_cr.py` -- the Cratylus simplifier. Transforms Cratylus programs into equivalent Cratylus programs with particular restrictions.

* `tools/crc.py` -- the Cratylus to C compiler. Compiles a subset of Cratylus into C.

* `tools/s2cr.py` -- the S to Cratylus compiler. S is a simple assembler-like language with few instructions: increment and decrement, conditional and unconditional jumps.

* `tools/ss2s.py` -- the S-with-macros to S compiler. S-with-macros is a slightly higher level language, with subroutines, and primitive control structures that get macroexpanded into plain S instructions.

Beyond standard Cratylus, the Cratylus^@ dialect is an improved
version of Cratylus which allows for more efficient scripts and has
an extension for performing I/O. With these extensions,
[a Cratylus^@ quine](https://github.com/foones/cratylus/blob/master/examples/strings/cratylus_quine.crm) is provided.

Turing completeness
-------------------

Cratylus is a superset of [Conway's FRACTRAN](http://en.wikipedia.org/wiki/FRACTRAN).
Restricting ourselves to constant polynomials, a FRACTRAN program:

    (a1/b1, a2/b2, ..., aN/bN)

gets translated to an equivalent Cratylus program:

    b1 => a1.
    b2 => a2.
    ...
    bN => aN.

Since FRACTRAN is Turing complete, also is Cratylus.

Moreover, as part of the Cratylus distribution, the script `s2cr.py`
translates a program in the (theoretical) programming language S, which
is well-known to be Turing complete, into a Cratylus program, which
again shows Cratylus' completeness.
See below for details.

Syntax for polynomials
----------------------

Multivariate polynomials are formed by operating with variables and 
positive integers.

Variables are either single lowercase characters
or a sequence of valid characters started by uppercase.

    <variable> ::= [a-z] | [A-Z][_a-z0-9]

Notice that variables can start in uppercase but cannot
contain uppercase characters in between. This is
for `FooBar` to be parsed as `Foo * Bar`.
Additionally, any string delimited by braces is a variable:

    <variable> ::= ... | {.*}

Numeric literals are restricted to be natural numbers:

    <num> ::= [0-9]+

More interesting polynomials are built by operating on variables
and numbers. The abstract syntax is given by:

    <poly> ::= <poly> + <poly>
             | <poly> - <poly>
             | <poly> * <poly>
             | <poly> ^ <num>

The usual precedence rules apply. All operators are left-associative.
More concretely:

    <atom> ::= <variable> | <num> | ( <poly> )

    <expatom> ::= <expatom>
                | <expatom> ^ <num>

    <factor> ::= <expatom>
               | <factor> <expatom>

    <term> ::= <factor>
             | <term> * <factor>

    <poly> ::= <term>
             | + <term>
             | - <term>
             | <poly> + <term>
             | <poly> - <term>

Notice there are two ways of writing products:
`<poly><poly>` and `<poly> * <poly>`.

Using Cratylus as a basic polynomial normalizer
-----------------------------------------------

Invoke Cratylus with no arguments (no rewrite rules):

    $ python cratylus.py

      ____           _         _           
     / ___|_ __ __ _| |_ _   _| |_   _ ___ 
    | |   | '__/ _` | __| | | | | | | / __|
    | |___| | | (_| | |_| |_| | | |_| \__ \
     \____|_|  \__,_|\__|\__, |_|\__,_|___/
                         |___/             

    Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>
    ? 42
    42
    ? x
    x
    ? abracadabra
    a^5b^2cdr^2
    ? x^2 - 1
    x^2 - 1
    ? (x + y)(x - y)
    x^2 - y^2
    ? (Foo + Bar)^2
    Bar^2 + 2BarFoo + Foo^2
    ? -({x}-{y}){x}
    -{x}^2 + {x}{y}

Syntax for programs
-------------------

A program is just a list of rules. Rules of the form
`p => 1.` can be abbreviated as `p.`. Additionally, a
Cratylus script can contain goals which are to be solved
as soon as the program is loaded.

    <program> ::= <EMPTY>
                | <program> <rule>
                | <program> <goal>

    <rule> ::= <poly> .
             | <poly> => <poly> .

    <goal> ::= ? <poly> .

Example 1: addition
-------------------

This example is used to model addition.
A polynomial `a x^n y^m` is rewritten to a polynomial `z^(n + m)`.
That is, the exponents of the variables `x` and `y` are added
resulting in the corresponding exponent of `z`.
When the following program is loaded:

    ax => az.
    ay => az.
    a => 1.

    ? a x^3 y^2.

Cratylus displays the following final result:

    z^5 

Intuitively, given a polynomial of the form `a x^n y^m`,
the first rule transforms all `x`s into `z`s, one
at a time. When there are no more `x`s to transform, the second
rule applies, which transforms all `y`s to `z`s. After that,
the third rule applies, which removes the redundant `a`.

The chain of reduction steps would be:

       a x^3 y^2
    => a x^2 y^2 z
    => a x   y^2 z^2
    => a     y^2 z^3
    => a     y   z^4
    => a         z^5
    =>           z^5

When the `-v` command line option is used, Cratylus traces the steps of the reduction. 

    ----------------------------------------
    Current goal : ax^3y^2
    Applying rule: ax => az
    Factorization: ax^3y^2 = (ax) * (x^2y^2)
    New goal     : ax^2y^2z
    ----------------------------------------
    Current goal : ax^2y^2z
    Applying rule: ax => az
    Factorization: ax^2y^2z = (ax) * (xy^2z)
    New goal     : axy^2z^2
    ----------------------------------------
    Current goal : axy^2z^2
    Applying rule: ax => az
    Factorization: axy^2z^2 = (ax) * (y^2z^2)
    New goal     : ay^2z^3
    ----------------------------------------
    Current goal : ay^2z^3
    Applying rule: ay => az
    Factorization: ay^2z^3 = (ay) * (yz^3)
    New goal     : ayz^4
    ----------------------------------------
    Current goal : ayz^4
    Applying rule: ay => az
    Factorization: ayz^4 = (ay) * (z^4)
    New goal     : az^5
    ----------------------------------------
    Current goal : az^5
    Applying rule: a => 1
    Factorization: az^5 = (a) * (z^5)
    New goal     : z^5
    ----------------------------------------
    Final result:
    z^5

The following is a different, more declarative, way of presenting
the above rules:

    Add X => Add Z.
    Add Y => Add Z.
    Add.

Toplevel interaction:

    ? Add X^9 Y^7
    Z^16

One could question why define addition in such a convoluted way,
given that the interpreter itself is a polynomial calculator
(which was already able to add constants).

The key aspect here is that we are representing information,
and manipulating a representation. The difference is that this way
of doing things "scales", in the sense that allows us to carry
out other, more complex, processes.

As in FRACTRAN, each variable can be thought as a register, and
rewrite rules can be thought as simple incrementing / decrementing
operations on exponents.

Example 2: erasing a variable
-----------------------------

The following operation can be used to remove all occurrences of X:

    Erase X => Erase.
    Erase.

Toplevel interaction:

    ? Erase X^9 Y^7
    Y^7

Example 3: copying a variable
-----------------------------

This operation creates two copies of X:

    Copy X => Copy Y Z.
    Copy.

Toplevel interaction:

    ? Copy X^9
    Y^9Z^9

Example 4: multiplication
-------------------------

The combination of these ideas allow us to write a procedure
that does multiplication:

    Mul X Y => Copy X Y.        # [1]
    Mul => Erase.               # [2]

    Copy Y => Copy Y1 Z.        # [3]
    Copy   => Rename.           # [4]

    Rename Y1 => Rename Y.      # [5]
    Rename    => Del1.          # [6]

    Del1 X   => Mul.            # [7]
    Del1     => Mul.            # [8]

    Erase Y  => Erase.          # [9]
    Erase.                      # [10]

Toplevel interaction:

    ? Mul X^10 Y^9
    Z^90

Intuitively, given a term of the form `Mul X^n Y^m`, we
want to carry out the product `n * m`. For doing so, we
add up `m` exactly `n` times.

The variable `Z` is used as an accumulator.
After the `i`-th iteration of the algorithm, the partial
result will be `Mul X^(n - i) Y^m Z^(i * m)`.
When `i` reaches `n`, we will have something like
`Mul Y^m Z^(n * m)`, after which we only need to
"clean" all the `Y` factors.

More precisely, if `n = 0`, there are no `X` factors
in the polynomial.
So we have a polynomial of the form `Mul Y^m ...`, i.e.
we want to multiply `m` times zero.
Rule [2] applies, which takes us to rules [9] and [10].
These erase, one by one, all occurrences of `Y`.

If `n > 0`, rule [1] applies. Rules [3] and [4]
are used to transform a term of the form
`Mul X^(n - i) Y^m Z^(i * m)` into a term of the form
`Mul X^(n - i) Y1^m Z^(i * m + m)`, that is, they copy the
exponent of `Y` into `Y1` and also add the exponent
of `Y` into the accumulator `Z`.

After doing so, we need to rename `Y1` back to `Y`,
to keep on with the iterations. Also, we delete one
occurrence of `X`. After this we reach:

    Mul X^(n - i - 1) Y^m Z^((i + 1) * m)

and the invariant is maintained.

Example 5: quotient and remainder
---------------------------------

Quotient and remainder algorithm:

    DivMod X Y => Copy X Y.
    DivMod => End.

    Copy Y => Copy Y1 Y2.
    Copy => Sub.

    Sub X Y2 => Sub.
    Sub Y2 => Rem Y2.
    Sub => Continue Q.

    Continue Y1 => Continue Y.
    Continue => DivMod.

    Rem Y1 Y2 => Rem.
    Rem Y1 => Rem R.
    Rem.

    End Y => End.
    End.

Toplevel interaction:

    ? DivMod X^62 Y^11           # 62 = 5 * 11 + 7
    Q^5R^7

An equivalent obfuscated version:

    dxy => cxy.
    d => e.
    cy => abc.
    c => s.
    bsx => s.
    bs => bl.
    s => nq.
    an => ny.
    n => d.
    abl => l.
    al => lr.
    l.
    ey => e.
    e.

Toplevel interaction:

    ? d x^62 y^11
    q^5r^7

S to Cratylus compiler
----------------------

As part of the Cratylus distribution, the script `s2cr.py` translates a program in
the programming language S to Cratylus. The programming language S is introduced
in M. Davis' *Computability, Complexity, and Languages* to study computability.

A program in S works with an infinite set of variables, which
hold natural numbers. A program is a list of operations delimited
by newlines. Labels can be introduced. At the end of the program,
variables are initialized by declarations of the form `! var value`.

    <program> ::= <instructions> <init>

    <instructions> ::= <EMPTY>
                     | <label>: <instructions>
                     | <op> <instructions>

    <op> ::= inc <var>
           | dec <var>
           | jmp <label>
           | jz <var> <label>
           | jnz <var> <label>

    <init> ::= <EMPTY>
              | ! <var> <num> <init>

For instance, the following S program calculates the product of
`X` and `Y`, with input `X = 11` and `Y = 9`:

    mult:
        jz X mult_end

        # Loop to copy Y to Y1 and Z
        copy:
            jz Y copy_end
            dec Y
            inc Y1
            inc Z
            jmp copy
        copy_end:

        # Loop to rename Y1 to Y 
        rename:
            jz Y1 rename_end
            dec Y1
            inc Y
            jmp rename
        rename_end:

        dec X
        jmp mult
    mult_end:

    # Loop to erase all copies of Y
    erase:
        dec Y
        jnz Y erase

    # Initial values    

    ! X 11
    ! Y 9

After translation with `s2cr.py`, we get the following Cratylus program: 

    {0}{X} => {1}{X}.
    {0} => {12}.
    {1}{Y} => {2}{Y}.
    {1} => {6}.
    {2}{Y} => {3}.
    {2} => {3}.
    {3} => {4}{Y1}.
    {4} => {5}{Z}.
    {5} => {1}.
    {6}{Y1} => {7}{Y1}.
    {6} => {10}.
    {7}{Y1} => {8}.
    {7} => {8}.
    {8} => {9}{Y}.
    {9} => {6}.
    {10}{X} => {11}.
    {10} => {11}.
    {11} => {0}.
    {12}{Y} => {13}.
    {12} => {13}.
    {13}{Y} => {12}{Y}.
    {13} => {14}.
    {14}.
    ? {0}{X}^11{Y}^9.

When run, the answer is:

    {Z}^99

For compiling an S program, we associate a variable `{k}` for
representing the `k`-th instruction in the source program.
For instance, if we are in the initial state, at the beginning
of the program, the polynomial has `{0}` as a factor.
This way we can represent an arbitrary state machine,
where the only factor of the form `{k}` represents the
instruction pointer / current state.

The simplest example is a non-conditional jump from
the `i`-th instruction to the `j`-th instruction, which is
represented by the following rule:

    {i} => {j}.

If the `i`-th instruction increments the variable `X`,
we get the following rule:

    {i} => {i+1} {X}.

Besides incrementing the exponent of `{X}` this rule
also increments the instruction pointer. Decrementation
is handled similarly, with the caveat that `{X}` might
have a `0` exponent, in which case we just advance the
instruction pointer:

    {i} {X} => {i+1}.
    {i} => {i+1}.

Conditional jumps combine the previous ideas.
A jump from the `i`-th instruction to the `j`-th
instruction testing if `X` is zero (`jz`) is compiled
as:

    {i} {X} => {i+1} {X}.
    {i} => {j}.

The additional `{X}` in the right-hand side of the
first rule is to avoid decrementing the value of `X`,
since we only want to check if it is zero or not.
The `jnz` instruction is symmetric:

    {i} {X} => {j} {X}.
    {i} => {i+1}.

Cratylus simplifier
-------------------

Also part of the Cratylus distribution is the script `simp_cr.py` which
normalizes Cratylus programs.

Notice that most of the Cratylus programs above, are written in *monomial form*.
That is, all the polynomials that appear in the source program are monomials
with leading coefficient equal to 1.

It is easy to see that a program in monomial form can be translated to an
equivalent FRACTRAN program, if the set of variables that occur in the
source program is put in 1-1 correspondence with an arbitrary set of primes.
(More generally, a program in monomial form can be translated to an
equivalent program if the set of variables is put in 1-1 correspondence
with a set of different irreducible elements in a unique factorization domain).

For instance, as we saw before, the S program which calculates the product
of `X` and `Y` gave output to the following Cratylus program:

    {0}{X} => {1}{X}.
    {0} => {12}.
    {1}{Y} => {2}{Y}.
    {1} => {6}.
    {2}{Y} => {3}.
    {2} => {3}.
    {3} => {4}{Y1}.
    {4} => {5}{Z}.
    {5} => {1}.
    {6}{Y1} => {7}{Y1}.
    {6} => {10}.
    {7}{Y1} => {8}.
    {7} => {8}.
    {8} => {9}{Y}.
    {9} => {6}.
    {10}{X} => {11}.
    {10} => {11}.
    {11} => {0}.
    {12}{Y} => {13}.
    {12} => {13}.
    {13}{Y} => {12}{Y}.
    {13} => {14}.
    {14}.

    ? {0}{X}^11{Y}^9.

Given a Cratylus program in monomial form, `tools/simp_cr.py -f file.cr` translates
it to an equivalent FRACTRAN program. The output of `simp_cr.py -f` in
this case is:

    85 => 65.
    17 => 11.
    26 => 86.
    13 => 3.
    86 => 41.
    43 => 41.
    41 => 427.
    61 => 3149.
    47 => 13.
    21 => 161.
    3 => 37.
    161 => 31.
    23 => 31.
    31 => 106.
    53 => 3.
    185 => 29.
    37 => 29.
    29 => 17.
    22 => 19.
    11 => 19.
    38 => 22.
    19 => 59.
    59.

    ? 425000000000.

Notice that, in the translation, the variable representing
the initial state was `{0}` and got translated into the numeric
constant `17`. The input variables, `{X}` and `{Y}` got
translated into `5` and `2`.
The query `425000000000` is equivalent to
`17 * 5^11 * 2^9`. That is, we are asking for the product of
`11` and `9`. The answer will be given as the exponent of
`{Z}`, which got translated into `67`. Indeed we get:

    6045127530961181628652411227196693490382260383969108811846856679868932815922570165203117478978324258618429876174558448218719550835202670830699753771294118292547613370699765040813403

which is exactly `67^99`.

The command `tools/simp_cr.py -u file.cr` transforms a Cratylus
program in monomial form to an equivalent program using univariate
polynomials. In our case:

    x^2 + 2x - 8 => x^2 - 6x + 8.
    x + 4 => x - 3.
    x^2 - 3x - 4 => x^2 - 5x - 6.
    x - 4 => x + 3.
    x^2 - 5x - 6 => x + 7.
    x - 6 => x + 7.
    x + 7 => x^2 + 11x + 18.
    x + 9 => x^2 + x - 90.
    x - 9 => x - 4.
    x^2 + 5x + 6 => x^2 + 8x + 12.
    x + 3 => x + 5.
    x^2 + 8x + 12 => x - 7.
    x + 6 => x - 7.
    x - 7 => x^2 + 9x + 8.
    x + 8 => x + 3.
    x^2 + 3x - 10 => x - 5.
    x + 5 => x - 5.
    x - 5 => x + 4.
    x^2 - 2x - 3 => x - 1.
    x - 3 => x - 1.
    x^2 - 1 => x^2 - 2x - 3.
    x - 1 => x - 8.
    x - 8.

In this case our query can be:

    ? (x + 4) (x - 2)^2 (x + 1)^3.

which asks for the product of `2` and `3`. We get the answer:

    x^6 + 60x^5 + 1500x^4 + 20000x^3 + 150000x^2 + 600000x + 1000000

which is `(x + 10)^6`.

The command `tools/simp_cr.py -v file.cr` transforms a Cratylus
program in monomial form to an equivalent program using variable
names as short as possible. In our case we get:

    dg => dh.
    g => f.
    ah => al.
    h => e.
    al => m.
    l => m.
    m => cq.
    q => rs.
    r => h.
    ce => ck.
    e => i.
    ck => n.
    k => n.
    n => ao.
    o => e.
    di => j.
    i => j.
    j => g.
    af => b.
    f => b.
    ab => af.
    b => p.
    p.

And the query is:

    ? g a^9 d^11
    s^99

S-with-macros to S compiler
---------------------------

Also part of the Cratylus distribution is the script `ss2s.py` which
translates an S program with macros to a regular S program.

An S program with macros allows defining subroutines with parameters,
and using some basic control constructs (if and while), besides
allowing labels, `inc`, `dec`, `jmp`, `jz` and `jnz`.
The grammar of S-with-macros is a superset of that of S:

    <program> ::= <instructions> <init>

    <instructions> ::= <EMPTY>
                     | <sub> \n <instructions>
                     | <label>: \n <instructions>
                     | <op> \n <instructions>

    <op> ::= inc <var>
           | dec <var>
           | jmp <label>
           | jz <var> <label>
           | jnz <var> <label>
           | <sub_name> <var1> ... <varN>
           | WHILEZ var \n <instructions> \n END
           | WHILENZ var \n <instructions> \n END
           | IFZ var \n <instructions> \n END
           | IFNZ var \n <instructions> \n END

    <sub> ::= SUB <sub_name> <var1> ... <varN> \n <instructions> \n END

    <init> ::= <EMPTY>
              | ! <var> <num> <init>

For instance, the following S-with-macros program calculates factorials:

    SUB rename X Y
        WHILENZ X
            dec X
            inc Y
        END
    END

    SUB bicopy X Y Z
        WHILENZ X
            dec X
            inc Y
            inc Z
        END
    END

    SUB zero X
        WHILENZ X
            dec X
        END
    END

    SUB mult X Y Z
        WHILENZ X 
            dec X
            bicopy Y Y2 Z
            rename Y2 Y
        END
        zero Y
    END

    SUB fact X Y
        inc Y
        WHILENZ X
            bicopy X X1 X2
            mult X1 Y T
            rename T Y
            rename X2 X
            dec X
        END
    END

    fact X Z

    ! X 5

The `ss2s.py` script translates the S-with-macros program to the
following plain S program:

        # fact X Z
        inc Z
    :l:1:
        jz X :l:2
        # bicopy X X1 X2
    :l:3:
        jz X :l:4
        dec X
        inc fact:1:X1
        inc fact:1:X2
        jmp :l:3
    :l:4:
        # mult X1 Y T
    :l:5:
        jz fact:1:X1 :l:6
        dec fact:1:X1
        # bicopy Y Y2 Z
    :l:7:
        jz Z :l:8
        dec Z
        inc mult:1:Y2
        inc fact:1:T
        jmp :l:7
    :l:8:
        # rename Y2 Y
    :l:9:
        jz mult:1:Y2 :l:10
        dec mult:1:Y2
        inc Z
        jmp :l:9
    :l:10:
        jmp :l:5
    :l:6:
        # zero Y
    :l:11:
        jz Z :l:12
        dec Z
        jmp :l:11
    :l:12:
        # rename T Y
    :l:13:
        jz fact:1:T :l:14
        dec fact:1:T
        inc Z
        jmp :l:13
    :l:14:
        # rename X2 X
    :l:15:
        jz fact:1:X2 :l:16
        dec fact:1:X2
        inc X
        jmp :l:15
    :l:16:
        dec X
        jmp :l:1
    :l:2:
        ! X 5

By using the S to Cratylus compiler (`s2cr.py` script), this in
turn gets compiled to the following Cratylus program:

    {0} => {1}{Z}.
    {1}{X} => {2}{X}.
    {1} => {32}.
    {2}{X} => {3}{X}.
    {2} => {7}.
    {3}{X} => {4}.
    {3} => {4}.
    {4} => {5}{fact:1:X1}.
    {5} => {6}{fact:1:X2}.
    {6} => {2}.
    {7}{fact:1:X1} => {8}{fact:1:X1}.
    {7} => {19}.
    {8}{fact:1:X1} => {9}.
    {8} => {9}.
    {9}{Z} => {10}{Z}.
    {9} => {14}.
    {10}{Z} => {11}.
    {10} => {11}.
    {11} => {12}{mult:1:Y2}.
    {12} => {13}{fact:1:T}.
    {13} => {9}.
    {14}{mult:1:Y2} => {15}{mult:1:Y2}.
    {14} => {18}.
    {15}{mult:1:Y2} => {16}.
    {15} => {16}.
    {16} => {17}{Z}.
    {17} => {14}.
    {18} => {7}.
    {19}{Z} => {20}{Z}.
    {19} => {22}.
    {20}{Z} => {21}.
    {20} => {21}.
    {21} => {19}.
    {22}{fact:1:T} => {23}{fact:1:T}.
    {22} => {26}.
    {23}{fact:1:T} => {24}.
    {23} => {24}.
    {24} => {25}{Z}.
    {25} => {22}.
    {26}{fact:1:X2} => {27}{fact:1:X2}.
    {26} => {30}.
    {27}{fact:1:X2} => {28}.
    {27} => {28}.
    {28} => {29}{X}.
    {29} => {26}.
    {30}{X} => {31}.
    {30} => {31}.
    {31} => {1}.
    {32}.
    ? {0}{X}^5.

Finally, by using the Cratylus simplifier (script `simp_cr.py` with
the `-v -t "{Z}" Z` command-line switch, we get our first example back:

    H => mZ.
    am => af.
    m => J.
    af => aB.
    f => k.
    aB => u.
    B => u.
    u => cL.
    L => eG.
    G => f.
    ck => cr.
    k => d.
    cr => b.
    r => b.
    bZ => yZ.
    b => j.
    yZ => t.
    y => t.
    t => iK.
    K => lE.
    E => b.
    ij => is.
    j => I.
    is => x.
    s => x.
    x => DZ.
    D => j.
    I => k.
    dZ => wZ.
    d => h.
    wZ => o.
    w => o.
    o => d.
    hl => lp.
    h => g.
    lp => v.
    p => v.
    v => CZ.
    C => h.
    eg => en.
    g => q.
    en => z.
    n => z.
    z => aF.
    F => g.
    aq => A.
    q => A.
    A => m.
    J.
    ? a^5H.

Cratylus to C compiler
----------------------

The script `crc.py` compiles a Cratylus program in monomial form to an
equivalent C program. The GNU Multiple Precision Library is used to represent
polynomial exponents.

The factorial program above gets compiled into
[a C program](https://github.com/foones/cratylus/blob/master/examples/factorial_crc_example.c).

Cratylus^@: a more efficient variant of Cratylus
------------------------------------------------

As said before, Cratylus programs are very inefficient, since
we are restricted to representing and working with natural numbers
in unary.

To mitigate this problem, we propose the Cratylus^@
variant of Cratylus. The new programs must be written
in monomial form. Besides the usual kinds of rules,
the exponents can also be the special symbol `@`.

A rule containing an occurrence of `@` is equivalent
to an infinite number of rules, where `@` is to be
replaced by each of the non-zero natural numbers.

For instance the Cratylus^@ program:

    x^@ => y^@.

Rewrites `x^42` in one step to `y^42`.
That allows us to write an efficient addition routine:

    Add X^@ => Z^@.
    Add Y^@ => Z^@.
    Add.

Note that if there is more than one occurrence of `@` at the
left-hand side, Cratylus^@ will bind `@` to the least of the
exponents. For instance, in this case:

    X^@ Y^@ => Z^@.

Cratylus^@ will rewrite `X^10 Y^8` by the rule
`X^8 Y^8 => Z^8`, which results in `X^2 Z^8`.

With this addition to the language, it is possible to write
an actually efficient factorial program:

    b^@s=>s.sx^@=>b^@x^@K.s=>K.l^@K=>Q.K=>Q.Q=>lW.bW=>r.W=>r.br=>bAb.r=>R.Ab=>bq.
    a^@q=>q.b^@q=>a^@b^@k.q=>k.e^@k=>k.kl^@=>e^@l^@C.k=>C.p^@C=>w.C=>w.aw=>aV.w=>P.
    u^@V=>o.V=>o.ao=>aZ.o=>g.Z=>c^2dE.c^@E=>c^@f^@i^@Aa.f^@Aa=>i^@Y.i^@Y=>f^@i^@L.
    a^@L=>a^@v^@M.L=>M.f^@v^@M=>m.f^@m=>I.mv^@=>m.c^@m=>D.m=>D.i^@D=>c^@H.D=>H.
    d^@H=>f^@hv^@.H=>h.f^@h=>d^@h.hv^@=>d^@E.h=>E.i^@I=>I.a^@c^@I=>O.c^@O=>gu^@.
    d^@O=>ot^@.d^@g=>g.gt^@=>a^@g.g=>X.uX=>uN.X=>B.e^@N=>e^@p^@B.N=>B.B=>nt.
    e^@nt=>c^@d^@F.nt^@=>T.n=>T.c^@F=>e^@F.d^@F=>e^@n.T=>w.u^@P=>J.P=>J.e^@J=>A.
    J=>A.a^@A=>j.A=>j.jl^@=>j.jp^@=>l^@p^@y.j=>y.by=>G.y=>G.bG=>U.G=>U.U=>r.p^@R=>z.
    R=>z.x^@z=>S.z=>S.S.

    ? s x^1000.

The result being:

    l^402387260077093773543702433923003985719374864210714632543799910429938512398629020592044208486969404800479988610197196058631666872994808558901323829669944590997424504087073759918823627727188732519779505950995276120874975462497043601418278094646496291056393887437886487337119181045825783647849977012476632889835955735432513185323958463075557409114262417474349347553428646576611667797396668820291207379143853719588249808126867838374559731746136085379534524221586593201928090878297308431392844403281231558611036976801357304216168747609675871348312025478589320767169132448426236131412508780208000261683151027341827977704784635868170164365024153691398281264810213092761244896359928705114964975419909342221566832572080821333186116811553615836546984046708975602900950537616475847728421889679646244945160765353408198901385442487984959953319101723355556602139450399736280750137837615307127761926849034352625200015888535147331611702103968175921510907788019393178114194545257223865541461062892187960223838971476088506276862967146674697562911234082439208160153780889893964518263243671616762179168909779911903754031274622289988005195444414282012187361745992642956581746628302955570299024324153181617210465832036786906117260158783520751516284225540265170483304226143974286933061690897968482590125458327168226458066526769958652682272807075781391858178889652208164348344825993266043367660176999612831860788386150279465955131156552036093988180612138558600301435694527224206344631797460594682573103790084024432438465657245014402821885252470935190620929023136493273497565513958720559654228749774011413346962715422845862377387538230483865688976461927383814900140767310446640259899490222221765904339901886018566526485061799702356193897017860040811889729918311021171229845901641921068884387121855646124960798722908519296819372388642614839657382291123125024186649353143970137428531926649875337218940694281434118520158014123344828015051399694290153483077644569099073152433278288269864602789864321139083506217095002597389863554277196742822248757586765752344220207573630569498825087968928162753848863396909959826280956121450994871701244516461260379029309120889086942028510640182154399457156805941872748998094254742173582401063677404595741785160829230135358081840096996372524230560855903700624271243416909004153690105933983835777939410970027753472000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

For using the Cratylus^@ variant, use files with `.crm` extensions.
Optionally, the `-m` option can be passed to the `cratylus.py` script.
For even faster execution, use `tools/crc.py` to compile Cratylus^@
programs into C.

Extension for input/output facilities
-------------------------------------

The Cratylus^@ dialect has an extension for performing input/output.
In a Cratylus^@ program, beyond the usual lowercase, uppercase and
braced variables, two more variables are allowed:
`<` (which stands for "input") and `>` (which stands for "output").

For the output feature, whenever a goal contains a factor of the
form `>^n`, the ASCII character
with numeric code `n` is written to standard output and the goal
is automatically rewritten, factoring `>^n` out. (If `n` is not in
the range `0..255`, Cratylus^@ uses `n mod 256`).

For the input feature, when the left-hand side of a rule contains a
factor of the form `<^@` and all of the remaining factors match,
a single character is read from standard input, and its character
code is bound to the maximal power. On EOF, the value `256` is
read.

The simplest output program:

    a => >^64.
    ? a.

Prints an `@` (ASCII code 64) when run.

The simplest input program:

    a<^@ => b^@.
    ? a.

Reads a character from standard input and stores its ASCII code as a power
of `b`.

A very simple program that outputs `Hello world!` followed
by a newline:

    a => >^10n.
    b => >^111c.
    c => >^114d.
    d => >^108e.
    e => >^100h.
    f => >^32g.
    g => >^119b.
    h => >^33a.
    i => >^108j.
    j => >^111f.
    k => >^101l.
    l => >^108i.
    m => >^72k.
    n => 1.

    ? m.

The following example is a braindead version of `cat` in Cratylus^@:

    # Simple version of cat in Cratylus^@
    #
    # Usage:
    #
    #    echo Hello world | python cratylus.py -s cat.crm
    #
    # (redirect 2>/dev/null for seeing output only)

    I<^@  => X^@.
    X^256 => 1.
    X^@   => I>^@.

    ? I.

The following example reads a number in unary and stores it as a power of `X`:

    # Read a number in unary from stdin in the form sss...s0
    # Example:
    #
    #    echo ssssssss0 | python cratylus.py -s read_number.crm
    #    ==> X^8

    I<^@ => a^@.
    a^115 => IX.
    a^48  => 1.

    ? I.

The following is a generic string output routine. In this example,
strings are encoded as a big enough power of `X`. More precisely,
each character code is represented by a "codon" (group of three
digits) in base 10. The first character in the string corresponds
to the least significant codon. For instance the string `"A"` is
represented by `X^065` (i.e. `X^65`). The string `"AB"` is represented
by `X^066065`.

    # Divmod algorithm
    # input:  Divmod X^n Y^m
    # output: Q^(n div m) R^(n mod m)
    #
    DivmodDm_c^@=>Divmod.DivmodX^@=>Dm__hDm_c^@X^@.Divmod=>Dm__h.Dm__hQ^@=>Dm_z.
    Dm__h=>Dm_z.Dm_zR^@=>Dm_s.Dm_z=>Dm_s.Dm_cDm_s=>Dm_cDm_l.Dm_s=>Dm_r.
    Dm_d^@Dm_l=>Dm_l.Dm_lY^@=>Dm__fDm_d^@Y^@.Dm_l=>Dm__f.Dm__fDm_e^@=>Dm_eDm_f.
    Dm__f=>Dm_eDm_f.Dm_b^@Dm_f=>Dm_f.Dm_d^@Dm_f=>Dm__sDm_b^@Dm_d^@.Dm_f=>Dm__s.
    Dm__s=>Dm_aDm_j.Dm_aDm_b^@Dm_j=>Dm__gDm_g^@Dm_h^@.Dm_a^@Dm_j=>Dm_i.Dm_j=>Dm_i.
    Dm__gDm_h^@=>Dm__gDm_b^@.Dm__gDm_g^@=>Dm_b^@Dm_j.Dm_iDm_k^@=>Dm_i.
    Dm_c^@Dm_i=>Dm__dDm_c^@Dm_k^@.Dm_i=>Dm__d.Dm__dDm_b^@=>Dm__pDm_a^@Dm_b^@.
    Dm__d=>Dm_q.Dm__pDm_a^@Dm_k^@=>Dm__i.Dm__p=>Dm__i.Dm__iDm_a^@=>Dm_q.Dm__i=>Dm_q.
    Dm_kDm_q=>Dm__uDm_k.Dm_q=>Dm_o.Dm__u=>Dm_aDm_t.Dm_aDm_e^@Dm_t=>Dm_g^@Dm_h^@Dm_w.
    Dm_a^@Dm_t=>Dm__l.Dm_t=>Dm__l.Dm_h^@Dm_w=>Dm_e^@Dm_w.Dm_g^@Dm_w=>Dm_e^@Dm_t.
    Dm__l=>Dm_aDm_u.Dm_aDm_d^@Dm_u=>Dm_g^@Dm_h^@Dm_x.Dm_a^@Dm_u=>Dm__n.Dm_u=>Dm__n.
    Dm_h^@Dm_x=>Dm_d^@Dm_x.Dm_g^@Dm_x=>Dm_d^@Dm_u.Dm__n=>Dm_f.Dm_b^@Dm_o=>Dm_o.
    Dm_d^@Dm_o=>Dm__kDm_b^@Dm_d^@.Dm_o=>Dm__k.Dm__kDm_c^@=>Dm__rDm_a^@Dm_c^@.
    Dm__k=>Dm_p.Dm__rDm_a^@Dm_b^@=>Dm__c.Dm__r=>Dm__c.Dm__cDm_a^@=>Dm_p.Dm__c=>Dm_p.
    Dm_bDm_p=>Dm_bDm_m.Dm_p=>Dm__o.Dm__oDm_d^@=>Dm__tDm_a^@Dm_d^@.Dm__o=>Dm_n.
    Dm__tDm_a^@Dm_c^@=>Dm__e.Dm__t=>Dm__e.Dm__eDm_a^@=>Dm_n.Dm__e=>Dm_n.
    Dm_e^@Dm_n=>Dm__mDm_e^@Q^@.Dm_n=>Dm__m.Dm__m=>Dm_s.Dm_mR^@=>Dm_m.
    Dm_c^@Dm_m=>Dm_c^@Dm_rR^@.Dm_m=>Dm_r.Dm_d^@Dm_r=>Dm__a.Dm_r=>Dm__a.
    Dm__aDm_b^@=>Dm_v.Dm__a=>Dm_v.Dm_e^@Dm_v=>Dm__b.Dm_v=>Dm__b.Dm__bDm_c^@=>Dm__j.
    Dm__b=>Dm__j.Dm__jX^@=>Dm_y.Dm__j=>Dm_y.Dm_yY^@=>Dm__q.Dm_y=>Dm__q.Dm__q.

    # Output routine
    IX^@ => Divmod X^@ Y^1000 J. 
    I    => >^10.
    JR^@ => J>^@.
    JQ^@ => IX^@.
    J    => I.

    # Output an encoded string
    ? I X^33100108114111119032111108108101072. # Hello world!

Reversing the input is a bit trickier:

    # Reverse standard input.
    # Example:
    #     $ echo -n ab | python cratylus.py rev.crm -s 2>/dev/null
    #     ba

    # Divmod algorithm
    # input:  Divmod X^n Y^m
    # output: Q^(n div m) R^(n mod m)
    #
    DivmodDm_c^@=>Divmod.DivmodX^@=>Dm__hDm_c^@X^@.Divmod=>Dm__h.Dm__hQ^@=>Dm_z.
    Dm__h=>Dm_z.Dm_zR^@=>Dm_s.Dm_z=>Dm_s.Dm_cDm_s=>Dm_cDm_l.Dm_s=>Dm_r.
    Dm_d^@Dm_l=>Dm_l.Dm_lY^@=>Dm__fDm_d^@Y^@.Dm_l=>Dm__f.Dm__fDm_e^@=>Dm_eDm_f.
    Dm__f=>Dm_eDm_f.Dm_b^@Dm_f=>Dm_f.Dm_d^@Dm_f=>Dm__sDm_b^@Dm_d^@.Dm_f=>Dm__s.
    Dm__s=>Dm_aDm_j.Dm_aDm_b^@Dm_j=>Dm__gDm_g^@Dm_h^@.Dm_a^@Dm_j=>Dm_i.Dm_j=>Dm_i.
    Dm__gDm_h^@=>Dm__gDm_b^@.Dm__gDm_g^@=>Dm_b^@Dm_j.Dm_iDm_k^@=>Dm_i.
    Dm_c^@Dm_i=>Dm__dDm_c^@Dm_k^@.Dm_i=>Dm__d.Dm__dDm_b^@=>Dm__pDm_a^@Dm_b^@.
    Dm__d=>Dm_q.Dm__pDm_a^@Dm_k^@=>Dm__i.Dm__p=>Dm__i.Dm__iDm_a^@=>Dm_q.Dm__i=>Dm_q.
    Dm_kDm_q=>Dm__uDm_k.Dm_q=>Dm_o.Dm__u=>Dm_aDm_t.Dm_aDm_e^@Dm_t=>Dm_g^@Dm_h^@Dm_w.
    Dm_a^@Dm_t=>Dm__l.Dm_t=>Dm__l.Dm_h^@Dm_w=>Dm_e^@Dm_w.Dm_g^@Dm_w=>Dm_e^@Dm_t.
    Dm__l=>Dm_aDm_u.Dm_aDm_d^@Dm_u=>Dm_g^@Dm_h^@Dm_x.Dm_a^@Dm_u=>Dm__n.Dm_u=>Dm__n.
    Dm_h^@Dm_x=>Dm_d^@Dm_x.Dm_g^@Dm_x=>Dm_d^@Dm_u.Dm__n=>Dm_f.Dm_b^@Dm_o=>Dm_o.
    Dm_d^@Dm_o=>Dm__kDm_b^@Dm_d^@.Dm_o=>Dm__k.Dm__kDm_c^@=>Dm__rDm_a^@Dm_c^@.
    Dm__k=>Dm_p.Dm__rDm_a^@Dm_b^@=>Dm__c.Dm__r=>Dm__c.Dm__cDm_a^@=>Dm_p.Dm__c=>Dm_p.
    Dm_bDm_p=>Dm_bDm_m.Dm_p=>Dm__o.Dm__oDm_d^@=>Dm__tDm_a^@Dm_d^@.Dm__o=>Dm_n.
    Dm__tDm_a^@Dm_c^@=>Dm__e.Dm__t=>Dm__e.Dm__eDm_a^@=>Dm_n.Dm__e=>Dm_n.
    Dm_e^@Dm_n=>Dm__mDm_e^@Q^@.Dm_n=>Dm__m.Dm__m=>Dm_s.Dm_mR^@=>Dm_m.
    Dm_c^@Dm_m=>Dm_c^@Dm_rR^@.Dm_m=>Dm_r.Dm_d^@Dm_r=>Dm__a.Dm_r=>Dm__a.
    Dm__aDm_b^@=>Dm_v.Dm__a=>Dm_v.Dm_e^@Dm_v=>Dm__b.Dm_v=>Dm__b.Dm__bDm_c^@=>Dm__j.
    Dm__b=>Dm__j.Dm__jX^@=>Dm_y.Dm__j=>Dm_y.Dm_yY^@=>Dm__q.Dm_y=>Dm__q.Dm__q.

    # Multiplication algorithm
    # input:  Mult X^n Y^m
    # output: Z^(n * m)
    #
    Mul_a^@Mult=>Mult.MultX^@=>Mul_a^@Mul_jX^@.Mult=>Mul_j.Mul_d^@Mul_j=>Mul_j.
    Mul_jY^@=>Mul_d^@Mul_vY^@.Mul_j=>Mul_v.Mul_vZ^@=>Mul_o.Mul_v=>Mul_o.
    Mul_aMul_o=>Mul__hMul_a.Mul_o=>Mul__j.Mul__hMul_n^@=>Mul_k.Mul__h=>Mul_k.
    Mul_aMul_k=>Mul__lMul_a.Mul_k=>Mul_f.Mul__l=>Mul_b^2Mul_cMul_z.
    Mul_b^@Mul_z=>Mul__kMul_b^@Mul_e^@Mul_i^@.Mul__kMul_e^@=>Mul__mMul_i^@.
    Mul__mMul_i^@=>Mul__eMul_e^@Mul_i^@.Mul__eMul_a^@=>Mul__iMul_a^@Mul_p^@.
    Mul__e=>Mul__i.Mul__iMul_e^@Mul_p^@=>Mul_g.Mul_e^@Mul_g=>Mul_x.
    Mul_gMul_p^@=>Mul_g.Mul_b^@Mul_g=>Mul_t.Mul_g=>Mul_t.Mul_i^@Mul_t=>Mul_b^@Mul_y.
    Mul_t=>Mul_y.Mul_c^@Mul_y=>Mul_e^@Mul_hMul_p^@.Mul_y=>Mul_h.
    Mul_e^@Mul_h=>Mul_c^@Mul_h.Mul_hMul_p^@=>Mul_c^@Mul_z.Mul_h=>Mul_z.
    Mul_i^@Mul_x=>Mul_x.Mul_a^@Mul_b^@Mul_x=>Mul__d.Mul__dMul_b^@=>Mul_fMul_n^@.
    Mul__dMul_c^@=>Mul_kMul_m^@.Mul_c^@Mul_f=>Mul_f.Mul_fMul_m^@=>Mul_a^@Mul_f.
    Mul_f=>Mul__c.Mul__cMul_n=>Mul__bMul_n.Mul__c=>Mul_q.
    Mul__bMul_d^@=>Mul_d^@Mul_qZ^@.Mul__b=>Mul_q.Mul_q=>Mul_lMul_m.
    Mul_d^@Mul_lMul_m=>Mul_b^@Mul_c^@Mul_r.Mul_lMul_m^@=>Mul__f.Mul_l=>Mul__f.
    Mul_b^@Mul_r=>Mul_d^@Mul_r.Mul_c^@Mul_r=>Mul_d^@Mul_l.Mul__f=>Mul_o.
    Mul__jMul_n^@=>Mul__a.Mul__j=>Mul__a.Mul__aMul_d^@=>Mul_w.Mul__a=>Mul_w.
    Mul_a^@Mul_w=>Mul_s.Mul_w=>Mul_s.Mul_sX^@=>Mul_u.Mul_s=>Mul_u.Mul_uY^@=>Mul__g.
    Mul_u=>Mul__g.Mul__g.

    # Input until EOF, accumulate string in X^...
    Reverse  => I.
    I<^@     => MaA^@.
    MaA^256  => Rv.
    MaA^@    => MbB^@MultY^1000.
    Ma       => MbMultY^1000.
    MbZ^@    => MbX^@.
    MbB^@    => IX^@.
    Mb       => I.

    # Output X^...
    RvX^@ => DivmodSX^@Y^1000.
    SR^@ => S>^@.
    SQ^@ => RvX^@.

    ? Reverse.

By using this kind of string manipulation routines, one can write a
[a Cratylus^@ quine](https://github.com/foones/cratylus/blob/master/examples/strings/cratylus_quine.crm). To run it, compile it with `tools/crc.py` and wait a couple of hours :)
