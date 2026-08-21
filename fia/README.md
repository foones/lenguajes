
# fia

Implementation of an "interactive" theorem prover for Finitistic Intuitionistic Arithmetic (FIA).
Proof-checking is implemented by a simple bidirectional type checker.

## The logic

The logic is a variant of Peano Arithmetic in which there are two kinds of universal quantifiers:
- Symbolic universal quantifier: `forall n, P(n)`
- Finitistic universal quantifier: `forall #n, P(n)`

The introduction and elimination rules for the **symbolic** universal quantifier are the usual introduction and universal rules for the universal quantifier in first-order logic. Note that this does **not** allow to prove `forall n, P(n)` by induction.

The introduction rules for the **finitistic** universal quantifier include the usual introduction rule, as well as the induction principle `P(0) -> (forall #n, P(n) -> P(S(n))) -> forall #n, P(n)`. However, the elimination rule only allows to instantiate a universal formula in a **concrete** numeral `S(S(...S(0)))`, or perhaps in a variable `n` bound by another finitistic universal quantifier.

## Syntax of terms

Terms (`<term>`) are built inductively from the following grammar:
- Constant: `0`, `1`, `2`, ...
- Term variables: `n`, `m`, ...
- Successor: `S(<term>)`
- Addition: `<term> + <term>`
- Multiplication: `<term> * <term>`
- Applied function symbol: `f(<term>, ..., <term>)`

## Syntax of formulae

Formulas (`<form>`) are built inductively from the following grammar:
- Formula variables: `X`, `Y`, ...
- Disjunction: `<form> | <form>`
- Conjunction: `<form> & <form>`
- Implication: `<form> -> <form>`
- Symbolic forall: `forall n, <form>`
- Finitistic forall: `forall #n, <form>`
- Symbolic exists: `exists n, <form>`
- Finitistic exists: `exists #n, <form>`
- Applied propositional symbol: `P(<term>, ..., <term>, <form>, ..., <form>)`
- `false` abbreviates `0 = 1`
- `not(A)` abbreviates `A -> false`

## Syntax of programs

A program consists of a sequence of declarations.

Currently there are three forms of declarations:

- Declaration of a defined function symbol: `fun f(n1, ..., nK) := <term>`.
  Here n1, ..., nK are term variables.
  Definitions of function symbols cannot be recursive; they are just syntactic abbreviations ("macros").
- Declaration of a defined propositional symbol:
    `prop p(n1, ..., nK, X1, ..., Xm) := <form>`
  The propositional symbol may depend on term variables (which must start
  with lowercase) and on formula variables (which must start with uppercase).
  Again, definitions of propositional symbols cannot be recursive.
- Proof of a theorem:
```
    theorem H : <form>
    proof
      <proof>
    end
```

## Contradictions and target formulae

In FIA, conjunction, disjunction, and existential quantification (in both the symbolic
and the finitistic case) are understood as just abbreviations:
   - `(A | B)` abbreviates `not(A) -> not(B) -> false`
   - `(A & B)` abbreviates `not(A -> B -> false)`
   - `exists n, A` abbreviates `not(forall n, not(A))`
   - `exists #n, A` abbreviates `not(forall #n, not(A))`

The conclusion of the elimination rules for disjunction and existential quantification
a priori only allow to conclude `false`, although it can be shown that more general
versions of these elimination rules are derivable, when the conclusion is any *contradiction*,
and more in general any *target formula* (defined below).

A formula is called a *contradiction* iff it is
   of the form `0 = S(...)`,
or of the form `S(...) = 0`.

A formula is a *target formula* according to the following inductive definition:
- A contradiction is a target.
- If `H` is a target, then `A -> H` is a target.
- If `H` is a target, then `forall n. H` is a target.
- Conjunction, disjunction, and existential quantification are always targets.

## Syntax for proofs

Proofs (`<proof>`) are constructed inductively from the following grammar:

- Axiom rule (reference an hypothesis or theorem by name):
    `H`

- Forall introduction:
  ```
  let n, <proof>      (symbolic)
  let #n, <proof>     (finitistic)
  ```

- Introductions of foralls can be mixed, e.g. `let n #m p, <proof>`

- Implication introduction:
  - `suppose H, <proof>`
  - `suppose H : <form>, <proof>`

- Implication elimination is written like application:
    `<proof> <proof>`

- Forall elimination is written as:
    `<proof>[<term>]`
  - If `<proof>` proves a symbolic forall then `<term>` may be arbitrary.
  - If `<proof>` proves a finitistic forall then `<term>` may only be a numeral
    or a variable bound by a finitistic binder. 

- Proofs of equalities are written like chains:
  ```
    indeed <term0> = <term1> <justification1>
                   = <term2> <justification2>
                   ...
                   = <termN> <justificationN>
  ```

  Each justification may be of one of two forms:
  - If `<justification_i>` is empty, check definitional equality of `<term_i-1>` and `<term_i>`,
    using the standard Peano's axioms for addition and multiplication
    (defined by recursion on the left argument).
  - `<justification_i>` can also be of the form "`by <proof>`".
     In this case, `<proof>` must be a proof of an equality `A = B`.
     We consider the set `EQ` that results from closing the equality `A = B`
     with respect to the following operations:
     - If "`X = Y`" is in `EQ`, then "`Y = X`" is also in `EQ`.
     - If "`S(X) = S(Y)`" is in `EQ`, then "`X = Y`" is in `EQ`.
     Exactly one subterm of `<term_i-1>` must be replaced using exactly one
     of the equations in `EQ`.

  There are some alternative syntaxes for chains:
  ```
    <term1> = <term2>
            = <term3> by proof
            = <term4> (by proof)
            = <term5>
            =(by proof) <term6>
  ```
  The ones with parentheses may be convenient for one-liners, e.g. `n =(by p) n' =(by q) n''`.

- Induction on natural numbers:

      `induction <proof1> <proof2>`

   Here `<proof1>` is the proof of the base case; it must prove `P(0)`.
   And `<proof2>` is the proof of the induction step; it must prove
       `forall #n, P(n) -> P(S(n))`

   The induction command proves a *finitistic* forall.
   That is, its conclusion is `forall #n, P(n)`

- Contradictions allow proving any formula: `contradiction <proof>`
  proves any formula provided that `<proof>` is a proof of a contradiction
  (i.e. either `0 = S(...)` or `S(...) = 0`).

- Conjunction introduction:
  ```
    then <proof1>
    also <proof2>
    ...
    also <proofN>
  ```

  This proves a conjuction `<form1> & ... & <formN>`.

- Conjunction elimination:

  ```
    have H : <form> by <proof1>,
    <proof2>
  ```

  Projects one of the components of <proof1>,
  then continues to prove the goal with <proof2>.
  It can only be applied when the goal is a *target* formula.

- Disjunction introduction:

  If the goal is of the form `A1 | ... | An`,
  then:
  ```
  assume na_1 ... na_n, <proof>
  ```
  reduces to the proof obligation to show false
  with hypotheses `na_1 : not(A1), ..., na_n : not(An)`.
  Optionally, annotations can be included:
  ```
      assume na1 : not(A1) ... naN : not(An), <proof>
  ```

- Disjunction elimination:
  ```
    cases <proof>
    case H1 : <form_1>, <proof_1>
    case H2 : <form_2>, <proof_2>
    ...
    case Hn : <form_n>, <proof_n>
  ```
  The annotations ": <form>" are optional.

- Existential introduction: `take <term>, <proof>`
  proves either `exists n, A` (symbolic) or `exists #n, A`  (finitistic).
  In the finitistic case, the term must be a numeral or a variable
  bound by a finitistic binder.

  We can abbreviate a sequence of takes
  as `take <term_1> ... <term_k>, <proof>`
  to prove nested existentials (`exists (#)n1 ... (#)nk, <proof>`).

- Existential elimination:
  ```
   consider n st H by <proof1>, <proof2>     (symbolic)
   consider #n st H by <proof1>, <proof2>    (finitistic)
  ```
  They can be mixed:
  ```
    consider n #m p st H by <proof1>, <proof2>
  ```
  The name of the hypothesis can be decorated with the formula:
  ```
    consider n #m p st H : <form> by <proof1>, <proof2>
  ```

## Auxiliary proofs (not part of the core)

- We can restate the goal we are proving as follows:

   ```
   show <form>, <proof>
   ```

  Checks that `<form>` matches the current goal.
  This may help to make a proof more readable.

- We can prove an auxiliary result as follows:

   ```
    claim H : <form>
    proof
        <proof1>
    end
    <proof2>
   ```

  Here `<proof1>` must be a proof of the claimed formula
  and `<proof2>` must be a proof of the main goal,
  which may now use "`H`".

## Holes and wildcards

- Hypothesis names can be replaced by a wildcard "`_`" to 
  emphasize that they are not used.

- There are two kinds of holes. Both kinds of holes start with a "`?`"
  and are followed by an arbitrary identifier.

  - Proof holes.
    They stand for a proof.
    The bidirectional type checker can instantiate proof holes
    to aid interactive theorem proving.
    (Note that the hole cannot be in a "synthesis" position).

  - Term holes.
    The checker does not instantiate term holes,
    it just treats them as a symbol standing for a natural number.
    (Term holes are not very useful).

  - Two exceptions to the rules above are provided to ease
    interactive theorem proving of equalities:
    - If the user writes "indeed ?hole"
      the hole is taken to be an equality proof rather than a term.
    - If the user writes "indeed X = Y by ?hole"
      the hole is technically speaking in a synthesis position,
      because the justification may not prove X = Y
      (e.g. it may prove any equation whose closure includes X = Y, such as S(X) = S(Y)).
      However, in this case the checker displays X = Y
      as the proof obligation. 

  - If there are many proof holes in the file, the checker displays
    only the proof obligation for the first proof hole.

    If the user has many holes and they wish to work on a hole
    other than the first, they can rename the previous holes to "?_"
    to make them "invisible".

    For example, the following script has two holes and the checker
    shows the proof obligation for the first one:
       ```
       theorem add_comm : forall #n #m, n + m = m + n
       proof
         induction
           ?
           ?
       end
       ```
    while the following script has two holes but the first one is
    "invisible", and the checker shows the proof obligation for the
    second one:
       ```
       theorem add_comm : forall #n #m, n + m = m + n
       proof
         induction
           ?_
           ?
       end
       ```

