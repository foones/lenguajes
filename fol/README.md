
FOL es un chequeador de demostraciones en deducción natural para lógica de primer
orden inspirado en la sintaxis de Mizar y sus descendientes.
En particular, el Mathematical Vernacular de Freek Wiedijk. 

## Grammar of terms

```
  t ::= X
      | f(t1, ..., tn)
```

## Grammar of formulae

```
  A ::= p(t1, ..., tn)
      | A & B                | A ∧ B
      | A | B                | A ∨ B
      | A -> B               | A → B
      | ¬A
      | forall X1 ... Xn, A  | ∀ X1 ... Xn, A
      | exists X1 ... Xn, A  | ∃ X1 ... Xn, A
```

## Grammar of programs

A program is a sequence of declarations. 
Declarations are of the form:

```
  D ::= data Type(a1,...,an) = Constructor1(Type1,...,Typen) | ... | Constructor1(Type1,...,Typen)
      | prop predName(Type1, ..., Typen)
      | axiom name : A
      | theorem name : A proof P qed
```

## Grammar of proofs

A proof is a sequence of commands.

```
  Optional formula:
    ?A ::= _
         | ?<holeName>
         | A

  Optional name:
    ?Name ::= /* empty */
            | NAME :

  Optional type:
    ?Type ::= /* empty */
            | : Type

  Typed name:
    TypedName ::= NAME
                | (NAME : Type)

  Optional justifications:
    ?Justifications ::= /* empty */
                      | by NAME, ..., NAME
```

Commands are of the form:
```
    admit ?A

    suppose ?Name ?A
      ->-introduction

    thus ?A ?Justifications
      This command formally corresponds to instances of the axiom rule
      in natural deduction.
      If the current thesis is a tree of conjunctions
      among which ?A can be found,
      remove all the occurrences of ?A from the thesis.
      Then check whether ?A is sure a logical consequence of the given justifications.

      To check whether a formula A is a "sure" consequence of the hypotheses B1,...,Bn:
      1) Convert (¬A^B1^...^Bn) to disjunctive normal form.
      2) Check that each clause is refutable.
      3) To check whether a clause is refutable, there are two options:
         3.1) If the clause includes two literals P, Q
              such that (the negated normal form of) ¬P unifies with Q,
              the clause is refutable.
         3.2) Otherwise, select the first formula of the form (∀X.P) in the clause,
              instantiate X into a fresh variable, and continue checking whether 
              the clause is refutable.
              If there are no universal formulae in the clause, fail
              (the clause is considered to be irrefutable).
      This procedures terminates because hypotheses are used linearly.
      Note that this means that the relative order and the number of times
      that a justification is mentioned may affect the outcome!

      After doing this:
      - If ?A is NOT sure a logical consequence of the context, fail.
      - If ?A is sure a logical consequence of the context,
        and no other formula remains in the thesis, 
        this solves the current thesis.
      - If ?A is sure a logical consequence of the context,
        and some formulae remain in the thesis, the remaining
        formula becomes the current thesis.
      The names in the optional 'by' clause are names of global or local hypotheses
      from which the conclusion may be extracted using implication elimination
      and universal elimination.

    hence ?Name A ?Justifications
      'hence' is like 'thus',
       but it implicitly includes the previous assertion in the justifications.

    conclude ?Justifications
      'conclude' is a synonym of 'thus _'

    ---

    have ?Name A ?Justifications
      'have' checks whether A is sure a logical consequence of the current context,
      and give it a name ("?Name").
      The list of justifications, if provided, is used as in the 'thus' case,

    then ?Name A ?Justifications
      'then' is like 'have',
      but it implicitly includes the previous assertion in the justifications.

    let TypedName ... TypedName ?Type
      Introduction of the universal quantifier.

    take t
      Introduction of the existential quantifier.
      t must be a closed term.

    consider TypedName st ?Name A ?Justifications
      Elimination of the existential quantifier.

    induction ?Type
    case constructor(x1, ..., xn)
      ...
    case constructor(x1, ..., xn)
      ...
      .
      .
      .
    case constructor(x1, ..., xn)
      ...
    end
      Universal introduction by induction.

    claim ?Name A ... end

    cases ?A ?Justifications
    case ?Name A1
      ...
    case ?Name A2
      ...
      .
      .
      .
    case ?Name An
      ...
    end
      Disjunction elimination.
```

## TODO

  `contradiction`

  `set x := ...`

