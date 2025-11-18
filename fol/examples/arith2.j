
data Nat = 0
         | S(Nat)

prop eq(Nat, Nat)
axiom refl   : eq(x, x)
axiom sym    : eq(x, y) → eq(y, x)
axiom trans  : eq(x, y) → eq(y, z) → eq(x, z)
axiom congS  : eq(x1, x2) → eq(S(x1), S(x2))
axiom 0_ne_S : ¬eq(0, S(x))
axiom injS   : eq(S(x), S(y)) → eq(x, y)

prop add(Nat, Nat, Nat)
axiom add0l  : eq(x, 0) → (add(x, y, z) <-> eq(y, z))
axiom addSl  : eq(x, S(x')) → (add(x, y, z) <-> ∃ z', eq(z, S(z')) ∧ add(x', y, z'))

theorem add_cong1 : eq(x1, x2) → add(x1, y, z) → add(x2, y, z)
  induction
  case 0
    let x2 y z
    suppose eq(0, x2)    then 1: eq(x2, 0) by sym
    suppose add(0, y, z) then 2: eq(y, z) by refl,add0l
    conclude by 1,2,add0l 
  case S(x1')
    suppose IH: ∀ x2 y z, eq(x1', x2) → add(x1', y, z) → add(x2, y, z)
    let x2 y z
    suppose eq(S(x1'), x2)    then 1: eq(x2, S(x1')) by sym
    suppose add(S(x1'), y, z) then 2: ∃ z', (eq(z, S(z')) ∧ add(x1', y, z')) by refl,addSl
    conclude by 1,2,addSl
  end
end

theorem add_cong3 : ∀ x y z1 z2, eq(z1, z2) → add(x, y, z1) → add(x, y, z2)
  induction
  case 0
    let y z1 z2 : Nat
    suppose eq_z1_z2: eq(z1, z2)
    suppose add(0, y, z1)
    then eq(y, z1) by refl,add0l
    then eq(y, z2) by eq_z1_z2,trans
    hence add(0, y, z2) by refl,add0l
  case S(x)
    suppose IH: ∀ y z1 z2, eq(z1, z2) → add(x, y, z1) → add(x, y, z2)
    let y z1 z2 : Nat
    suppose eq_z1_z2: eq(z1, z2)
    suppose add(S(x), y, z1)
    then consider z1' st 2: eq(z1, S(z1')) ∧ add(x, y, z1') by refl,addSl
    claim 3: eq(z2, S(z1')) ∧ add(x, y, z1')
      have eq(z2, z1) by eq_z1_z2,sym
      hence eq(z2, S(z1')) by 2,trans
      conclude by 2
    end
    conclude by 3,refl,addSl
  end
end

theorem add0r : ∀ x y z, eq(y, 0) → (add(x, y, z) <-> eq(x, z))
  induction
  case 0
    let y z : Nat
    suppose eq_y_0: eq(y, 0)
    show add(0, y, z) → eq(0, z)
      suppose add(0, y, z)
      then 1.1: eq(y, z) by refl,add0l
      hence eq(0, z) by eq_y_0,sym,1.1,trans
    show eq(0, z) → add(0, y, z)
      suppose eq(0, z)
      then eq(y, z) by eq_y_0,trans
      hence add(0, y, z) by refl, add0l
  case S(x)
    suppose IH: ∀ y z, eq(y, 0) → (add(x, y, z) <-> eq(x, z))
    let y z : Nat
    suppose eq_y_0: eq(y, 0)
    show add(S(x), y, z) → eq(S(x), z)
      suppose add(S(x), y, z)
      then consider z' st 1.2: eq(z, S(z')) ∧ add(x, y, z') by refl,addSl
      then eq(x, z') by eq_y_0,IH
      then eq(S(x), S(z')) by congS
      hence eq(S(x), z) by 1.2,sym,trans
    show eq(S(x), z) → add(S(x), y, z)
      suppose eq(S(x), z) then 2.1: eq(z,S(x)) by sym
      have add(x, y, x) by IH,refl,eq_y_0
      hence add(S(x), y, z) by refl,addSl,2.1
  end
end

theorem addSr : ∀ x y y' z, eq(y, S(y')) → (add(x, y, z) <-> ∃ z', eq(z, S(z')) ∧ add(x, y', z'))
  induction
  case 0
    let y y' z : Nat
    suppose eq_y_Sy': eq(y, S(y'))
    show add(0, y, z) → (∃ z', eq(z, S(z')) ∧ add(0, y', z'))
      suppose add(0, y, z) then 1.1: eq(y, z) by refl,add0l
      take y'
      have 1.2: eq(z, S(y')) by 1.1,eq_y_Sy',sym,trans
      have 1.3: add(0, y', y') by add0l,refl
      conclude by 1.2,1.3
    show (∃ z', eq(z, S(z')) ∧ add(0, y', z')) → add(0, y, z)
      suppose ∃ z', eq(z, S(z')) ∧ add(0, y', z')
      then consider z' st 2.2: eq(z, S(z')) ∧ add(0, y', z')
      have eq(y', z') by refl,add0l,2.2
      then eq(S(y'), S(z')) by congS
      then eq(y, S(z')) by eq_y_Sy',trans
      then eq(y, z) by 2.2,trans,sym
      hence add(0, y, z) by refl,add0l
  case S(x)
    suppose IH: ∀ y y' z, eq(y, S(y')) → (add(x, y, z) <-> ∃ z', eq(z, S(z')) ∧ add(x, y', z'))
    let y y' z : Nat
    suppose eq_y_Sy': eq(y, S(y'))
    show add(S(x), y, z) → ∃ z', eq(z, S(z')) ∧ add(S(x), y', z')
      suppose add(S(x), y, z)
      then consider z' st 1.2: eq(z, S(z')) ∧ add(x, y, z') by refl,addSl
      then consider z'' st 1.3: eq(z', S(z'')) ∧ add(x, y', z'') by IH,eq_y_Sy'
      take z'
      thus eq(z, S(z')) by 1.2
      have add(S(x), y', S(z'')) by 1.3,refl,addSl
      hence add(S(x), y', z') by 1.3,sym,add_cong3
    show (∃ z', eq(z, S(z')) ∧ add(S(x), y', z')) → add(S(x), y, z)
      suppose 2.1: _
      then consider z' st 2.2: eq(z, S(z')) ∧ add(S(x), y', z')
      then consider z'' st 2.4: eq(z', S(z'')) ∧ add(x, y', z'') by refl,addSl
      claim 2.5: eq(z, S(z')) ∧ add(x, y, z')
        thus eq(z, S(z')) by 2.2
        thus add(x, y, z') by IH,eq_y_Sy',2.4
      end
      conclude by 2.5,refl,addSl
  end
end

theorem add_comm : ∀ x y z, add(x, y, z) → add(y, x, z)
  induction 
  case 0
    let y z
    suppose add(0, y, z)
    then eq(y, z) by refl,add0l
    hence add(y, 0, z) by refl,add0r
  case S(x)
    suppose IH: ∀ y z, add(x, y, z) → add(y, x, z)
    let y z
    suppose add(S(x), y, z)
    then consider z' st eq(z, S(z')) ∧ add(x, y, z') by refl,addSl
    then eq(z, S(z')) ∧ add(y, x, z') by IH
    hence add(y, S(x), z) by refl,addSr
  end 
end

