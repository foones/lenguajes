
data Nat = 0 | s(Nat) | +(Nat,Nat)

prop eq(Nat,Nat)

axiom add0    : eq(+(0,x),x)
axiom addS    : eq(+(s(x),y),s(+(x,y)))
axiom eqRefl  : eq(x,x)
axiom eqSym   : eq(x,y) → eq(y,x)
axiom eqTrans : eq(x,y) → eq(y,z) → eq(x,z)
axiom eqCong+ : (eq(x,y) → eq(+(x,z),+(y,z)))
              ∧ (eq(x,y) → eq(+(z,x),+(z,y)))
axiom eqCongS : eq(x,y) → eq(s(x),s(y))

theorem add-trans-step :
          eq(+(x,+(y,z)),+(+(x,y),z))
        → eq(+(s(x),+(y,z)),+(+(s(x),y),z))
  let x y z
  suppose IH: eq(+(x,+(y,z)),+(+(x,y),z))
  have A: eq(+(s(x),+(y,z)), s(+(+(x,y),z)))
    by addS,eqTrans,eqCongS,IH
  have B: eq(s(+(+(x,y),z)),+(s(+(x,y)),z))
    by addS,eqSym
  have C: eq(+(s(+(x,y)),z),+(+(s(x),y),z))
    by addS,eqSym,eqCong+
  thus eq(+(s(x),+(y,z)),+(+(s(x),y),z))
    by A,B,C,eqTrans,eqTrans
end

prop p
prop q

theorem contraposition : (p → q) → (¬q → ¬p)
  suppose 1: p → q
  suppose 2: ¬q
  cases
  case p
    then q ∧ ¬q by 1,2
    hence ¬p
  case ¬p
    hence ¬p
  end
end

