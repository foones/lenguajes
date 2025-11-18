
data Nat = 0 | s(Nat) | +(Nat,Nat) | *(Nat,Nat)

prop eq(Nat,Nat)

axiom ax_0_s : ¬eq(0,s(n))
axiom ax_s_s : eq(s(n),s(m)) → eq(n,m)
axiom ax_+_0 : eq(+(0,n),n)
axiom ax_+_s : eq(+(s(n),m),s(+(n,m)))
axiom ax_*_0 : eq(*(0,n),0)
axiom ax_*_s : eq(*(s(n),m),+(m,*(n,m)))

axiom eq_cong_s : eq(n,m) → eq(s(n),s(m))
axiom eq_sym    : eq(n,m) → eq(m,n)
axiom eq_trans  : eq(n,m) → eq(m,p) → eq(n,p)

axiom ind_+_0_r : eq(+(0,0),0)
                → ∀ n, (eq(+(n,0),n) → eq(+(s(n),0),s(n)))
                → ∀ n, eq(+(n,0),n)

theorem +_0_r : ∀ n, eq(+(n,0),n)
  claim base: eq(+(0,0),0)
    conclude by ax_+_0
  end
  claim step: ∀ n, (eq(+(n,0),n) → eq(+(s(n),0),s(n)))
    let n
    suppose ih: eq(+(n,0),n)
    have 1: eq(+(s(n),0),s(+(n,0))) by ax_+_s
    have 2: eq(s(+(n,0)),s(n)) by eq_cong_s,ih
    conclude by 1,2,eq_trans
  end
  conclude by ind_+_0_r,base,step 
end

axiom ind_+_s_r : eq(+(0,s(m)),s(+(0,m)))
                → ∀ n m, (eq(+(n,s(m)),s(+(n,m))) → eq(+(s(n),s(m)),s(+(s(n),m))))
                → ∀ n m, eq(+(n,s(m)),s(+(n,m)))
theorem +_s_r : ∀ n m, eq(+(n,s(m)),s(+(n,m)))
  claim base: ∀ m, eq(+(0,s(m)),s(+(0,m)))
    let m
    have 1: eq(+(0,s(m)),s(m)) by ax_+_0
    have 2: eq(s(m),s(+(0,m))) by ax_+_0,eq_sym,eq_cong_s
    conclude by 1,2,eq_trans
  end
  claim step: ∀ n m, (eq(+(n,s(m)),s(+(n,m))) → eq(+(s(n),s(m)),s(+(s(n),m))))
    let n m
    suppose ih: _
    have 1: eq(+(s(n),s(m)),s(+(n,s(m)))) by ax_+_s
    have 2: eq(s(+(n,s(m))),s(s(+(n,m)))) by ih,eq_cong_s
    have 3: eq(s(s(+(n,m))),s(+(s(n),m))) by ax_+_s,eq_cong_s,eq_sym
    conclude by 1,2,3,eq_trans,eq_trans
  end
  conclude by ind_+_s_r,base,step 
end

