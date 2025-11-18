
data U = a
       | b
       | zero
       | succ(U)
       | f(U,U)

prop p
prop q
prop r
prop p1(U)
prop q1(U)
prop p2(U,U)
prop lt(U,U)

----

theorem andI : p → q → p ∧ q
  thus _
end

theorem andI_v1 : p → q → p ∧ q
  suppose 1: p
  suppose 2: q
  thus p ∧ q by 1,2
end

-- Alternative
theorem andI_v2 : p → q → p ∧ q
  suppose 1: _
  suppose 2: _
  thus _ by 1,2
end

theorem andE1 : p ∧ q → p
  thus _
end

theorem andE1_v1 : p ∧ q → p
  suppose 1: p ∧ q
  thus p by 1
end

theorem andE2 : p ∧ q → q
  suppose 1: p ∧ q
  thus q by 1
end

theorem and_comm : (p ∧ q) → (q ∧ p)
  thus _
end

theorem and_comm_v1 : (p ∧ q) → (q ∧ p)
  suppose 1: p ∧ q
  thus q ∧ p by 1
end

theorem and_comm_v2 : (p ∧ q) → (q ∧ p)
  suppose 1: p ∧ q
  thus q by 1
  thus p by 1
end

theorem and_assoc_LR : ((p ∧ q) ∧ r) → (p ∧ (q ∧ r))
  thus _
end

theorem naming_thus : p → (p ∧ (q → p))
  suppose 1: p
  thus 2: p by 1
  thus q → p by 2
end

----

theorem or_comm : (p ∨ q) → (q ∨ p)
  thus _
end

theorem or_assoc_LR : ((p ∨ q) ∨ r) → (p ∨ (q ∨ r))
  thus _
end

----

theorem impE : (p → q) → p → q
  suppose 1: p → q
  suppose 2: p
  thus q by 1,2
end

theorem impE2 : (p → q → r) → p → q → r
  suppose A: p → q → r
  suppose B: p
  suppose C: q
  thus r by A,B,C
end

theorem imp_I : p → p
  suppose A: p
  thus p by A
end

theorem imp_K : p → q → p
  suppose A: p
  suppose q
  thus p by A
end

theorem imp_S_v1 : (p → q → r) → (p → q) → p → r
  thus _
end

theorem imp_S_v2 : (p → q → r) → (p → q) → p → r
  suppose A: p → q → r
  suppose B: p → q
  suppose C: p
  have    D: q → r      by A,C
  have    E: q          by B,C
  thus       r          by D,E
end

theorem imp_S_v3 : (p → q → r) → (p → q) → p → r
  suppose A: p → q → r
  suppose B: p → q
  suppose C: p
  have D: q by B,C
  thus    r by A,D,C
end

theorem imp_S_v4 : (p → q → r) → (p → q) → p → r
  suppose 1: p → q → r
  suppose 2: p → q
  suppose 3: p
  thus r by 1,2,3
end

----

theorem lem : p ∨ ¬p
  thus _
end

theorem ex_falso_01 : p → ¬p → q
  thus _
end

theorem dneg : ¬¬p → p
  thus _
end

theorem imp_trans : (p → q) → (q → r) → p → r
  thus _
end

theorem forall_inst_01 : (∀ X Y : U, p1(a)) → p1(a)
  thus _
end

theorem forall_inst_02 : (∀ X Y : U, p1(X)) → p1(a)
  thus _
end

theorem forall_inst_03 : (∀ X, p2(X, X)) → p2(succ(zero), succ(zero))
  thus _
end

theorem forall_inst_03_v2 : (∀ X, p2(X, X)) → p2(succ(zero), succ(zero))
  suppose 1: ∀ X, p2(X, X)
  thus p2(succ(zero), succ(zero)) by 1
end

theorem forall_inst_04 : (∀ X Y, p2(f(X,Y),f(Y,X))) → p2(f(a,b),f(b,a))
  suppose 1:_ thus _ by 1
end

theorem forall_inst_04_v2 : (∀ X Y, p2(f(X,Y),f(Y,X))) → p2(f(a,b),f(b,a))
  thus _
end

axiom lt-succ  : ∀ X, lt(X,succ(X))
axiom lt-trans : ∀ X Y Z, (lt(X,Y) → lt(Y,Z) → lt(X,Z))

theorem lt_0_2 : lt(zero, succ(succ(zero)))
  have A: lt(zero, succ(zero)) by lt-succ
  have B: lt(succ(zero), succ(succ(zero))) by lt-succ
  have C: lt(zero, succ(zero))
        → lt(succ(zero), succ(succ(zero)))
        → lt(zero, succ(succ(zero)))
    by lt-trans
  thus lt(zero, succ(succ(zero))) by A,B,C
end

theorem lt-0_1 : lt(zero, succ(zero))
  thus _ by lt-succ
end

theorem lt-0_2 : lt(zero, succ(succ(zero)))
  thus _ by lt-trans, lt-succ
end

theorem lt-0_3 : lt(zero, succ(succ(succ(zero))))
  thus _ by lt-trans, lt-trans, lt-succ
end

-----

theorem test_then_S : (p → q → r) → (p → q) → p → r
  suppose A: (p → q → r)
  suppose B: (p → q)
  suppose C: p
  then    D: q     by B
  have       q → r by A,C
  hence      r     by D
end

theorem forall_1 : (∀ X, p1(X) ∧ q1(X)) → ∀ X, p1(X)
  suppose ∀ X, (p1(X) ∧ q1(X))
  let x
  then have p1(x) ∧ q1(x)
  hence p1(x)
end

----

theorem or_elim_01 : ((p → r) ∧ (¬r → ¬q)) → (p ∨ q ∨ r) → r
  suppose H: (p → r) ∧ (¬r → ¬q)
  suppose 1: p ∨ q ∨ r
  cases by 1
  case p
    hence r by H
  case q
    hence r by H
  case r
    hence r
  end
end

theorem ex : p1(a) → ∃ X, p1(X)
  suppose p1(a)
  take a
  hence p1(a)
end

--- then... syntax

theorem then_syntax_1 : p → (p ∧ (q → p))
  suppose p
  hence p
  then conclude q → p
end

theorem then_syntax_2 : (p ∧ ¬p) → q
  suppose p ∧ ¬p
  then contradiction
end

theorem then_syntax_3 : (p ∨ q) → (q ∨ p)
  suppose p ∨ q
  then cases
  case p then conclude
  case q then conclude
  end
end

theorem then_syntax_4 : (∃ x, p1(succ(x))) → ∃ x, p1(x)
  suppose ∃ x, p1(succ(x))
  then consider x st p1(succ(x))
  take succ(x) then conclude
end

