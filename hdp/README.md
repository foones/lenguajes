
_Haskell de pepa_ (HDP) es una implementación de juguete de la lógica lineal intuicionista con
cuantificación universal de segundo orden. Todas las funciones definibles en HDP son totales
(todos los programas terminan). Pero el lenguaje es bastante expresivo.

## Sintaxis (informal)

Varias construcciones del lenguaje tienen dos versiones, una Unicode y una en ASCII puro.
Los identificadores se delimitan por espacios en blanco (espacios, tabs, CRs y LFs).

Los programas son secuencias de cláusulas. Hay tres tipos de cláusulas:

1. Declaración de un tipo:

La declaración de un tipo consta de la declaración del kind del tipo, seguida de su
definición. Por ejemplo, se puede declarar el tipo de las listas de elementos de tipo `A`:

```
List : Type → Type
List A = ∀ X : !(A → X → X) → !X → X
```

2. Declaración de un valor:

La declaración de un valor consta de la declaración de su tipo, seguida de su
definición. Por ejemplo, se puede declarar la función predecesor así:

```
pred : Nat → Nat
pred n = π₂ Nat Nat
            (n (Nat & Nat)
               (zero, zero)
               (! (λ p → (suc (π₁ Nat Nat p), π₁ Nat Nat p))))
```

3. Evaluación:

Cláusula de la forma `eval t`, donde `t` es un término arbitrario, que procede
a evaluar `t` y mostrar su resultado en pantalla.

### Sintaxis de los kinds

```
K ::= Type | K → K
```

### Sintaxis de los tipos

```
A ::= ∀ X : A        cuantificador universal de segundo orden
    | forall X : A     "
    | !A             modalidad exponencial of course / bang
    | A → B          implicación lineal
    | A -> B           "
    | A ⊗ B          conjunción multiplicativa (tensor)
    | A <*> B          "
    | A & B          conjunción aditiva (with)
    | A ⊕ B          disyunción aditiva (suma)
    | A <+> B          "
```

El cuantificador universal puede ligar varias variables simultáneamente;
por ejemplo `∀ X Y Z : A` es equivalente a `∀ X : ∀ Y : ∀ Z : A`.

El bang (`!`) está declarado como puntuación _soft_ a nivel léxico,
para que no haga falta circundarlo con espacios a la derecha.
(pero sí a la izquierda).

### Sintaxis de los términos

```
t ::= λ x → t                 abstracción (introducción de → y ∀)
    | \ x -> t                  "
    | t t                     aplicación (eliminación de →)
    | t A                     aplicación (eliminación de ∀)
    | t ⊗ s                   introducción de ⊗
    | t <*> s                   "
    | δ⊗ t (x y : s)          eliminación de ⊗
    | d<*> t (x y : s)          "
    | (t, s)                  introducción de &
    | δ&₁ t (x : s)           eliminación de & [1]
    | d&1 t (x : s)             "
    | δ&₂ t (x : s)           eliminación de & [2]
    | d&2 t (x : s)             "
    | inl t                   introducción de ⊕ [1]
    | inr t                   introducción de ⊕ [2]
    | δ⊕ t (x : s) (y : r)    eliminación de ⊕
    | d<+> t (x : s) (y : r)    "
    | !t                      introducción de !
    | δ! t (x : s)            eliminación de !
    | d! t (x : s)              "

```

Las abstracciones pueden ligar varias variables simultáneamente;
por ejemplo `λ X f x → t` es equivalente a `λ X → λ f → λ x → t`.

Los paréntesis no son estrictamente necesarios para la introducción de `&`.
La coma está declarada como puntuación a nivel léxico, para que no haga falta
circundarla con espacios.

