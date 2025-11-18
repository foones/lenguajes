
data G = 1
       | i(G)
       | *(G,G)

prop eq(G,G)

axiom neutl       : eq(*(1,a),a)
axiom neutr       : eq(*(a,1),a)
axiom invl        : eq(*(i(a),a),1)
axiom invr        : eq(*(a,i(a)),1)
axiom assoc       : eq(*(*(a,b),c),*(a,*(b,c)))
--
axiom eq_refl     : eq(a,a)
axiom eq_sym      : eq(a,b) → eq(b,a)
axiom eq_trans    : eq(a,b) → eq(b,c) → eq(a,c)
axiom eq_cong_inv : eq(a,b) → eq(i(a),i(b))
axiom eq_cong_opl : eq(a,b) → eq(*(a,c),*(b,c))
axiom eq_cong_opr : eq(a,b) → eq(*(c,a),*(c,b))
--

theorem invmull : ∀ a b, eq(*(i(a),*(a,b)),b)
  let a b
  have 1: eq(*(i(a),*(a,b)),*(*(i(a),a),b)) by assoc,eq_sym
  have eq(*(*(i(a),a),b),b) by invl,neutl,eq_trans,eq_cong_opl
  hence _ by 1,eq_trans
end

theorem invmulr : ∀ a b, eq(*(*(a,b),i(b)),a)
  let a b
  have 1: eq(*(*(a,b),i(b)),*(a,*(b,i(b)))) by assoc
  have eq(*(a,*(b,i(b))),a) by invr,neutr,eq_trans,eq_cong_opr
  hence _ by 1,eq_trans
end

theorem neutl_uniq : ∀ e, ((∀ a, eq(*(e,a),a)) → eq(e,1))
  let e
  suppose e_neutl:_
  have 1: eq(e,*(e,1)) by eq_sym,neutr
  have 2: eq(*(e,1),1) by e_neutl
  conclude by 1,2,eq_trans
end

theorem neutr_uniq : ∀ e, ((∀ a, eq(*(a,e),a)) → eq(e,1))
  let e
  suppose e_neutr:_
  have 1: eq(e,*(1,e)) by eq_sym,neutl
  have 2: eq(*(1,e),1) by e_neutr
  conclude by 1,2,eq_trans
end

theorem invl_uniq : ∀ a b, (eq(*(b,a),1) → eq(b,i(a)))
  let a b
  suppose b_invl:_
  have eq(*(*(b,a),i(a)),*(1,i(a))) by b_invl,eq_cong_opl
  then eq(b,*(1,i(a))) by invmulr,eq_sym,eq_trans
  hence eq(b,i(a)) by neutl,eq_trans
end

theorem invr_uniq : ∀ a b, (eq(*(a,b),1) → eq(b,i(a)))
  let a b
  suppose b_invr:_
  have eq(*(i(a),*(a,b)),*(i(a),1)) by b_invr,eq_cong_opr
  then eq(b,*(i(a),1)) by invmull,eq_sym,eq_trans
  hence eq(b,i(a)) by neutr,eq_trans
end

theorem cancell : ∀ a b c, (eq(*(a,c),*(b,c)) → eq(a,b))
  let a b c
  suppose _
  then eq(*(*(a,c),i(c)),*(*(b,c),i(c))) by eq_cong_opl
  then eq(a,*(*(b,c),i(c))) by invmulr,eq_sym,eq_trans
  hence eq(a,b) by invmull,eq_sym,eq_trans
end

theorem cancelr : ∀ a b c, (eq(*(a,b),*(a,c)) → eq(b,c))
  let a b c
  suppose _
  then eq(*(i(a),*(a,b)),*(i(a),*(a,c))) by eq_cong_opr
  then eq(b,*(i(a),*(a,c))) by invmull,eq_sym,eq_trans
  hence eq(b,c) by invmulr,eq_sym,eq_trans
end

theorem inv_mul : ∀ a b, eq(*(i(b),i(a)),i(*(a,b)))
  let a b
  claim 1: eq(*(*(a,b),*(i(b),i(a))),1)
    have 1: eq(*(*(a,b),*(i(b),i(a))),*(a,*(b,*(i(b),i(a))))) by assoc
    have eq(*(a,*(b,*(i(b),i(a)))),*(a,*(*(b,i(b)),i(a)))) by assoc,eq_sym,eq_cong_opr
    then 2: eq(*(*(a,b),*(i(b),i(a))),*(a,*(*(b,i(b)),i(a)))) by 1,eq_trans
    have eq(*(a,*(*(b,i(b)),i(a))),*(a,*(1,i(a)))) by invr,eq_cong_opr,eq_cong_opl
    then 3: eq(*(*(a,b),*(i(b),i(a))),*(a,*(1,i(a)))) by 2,eq_trans
    have eq(*(a,*(1,i(a))),*(a,i(a))) by neutl,eq_cong_opr
    then 4: eq(*(*(a,b),*(i(b),i(a))),*(a,i(a))) by 3,eq_trans
    have eq(*(a,i(a)),1) by invr
    hence eq(*(*(a,b),*(i(b),i(a))),1) by 4,eq_trans
  end
  conclude by 1,invr_uniq
end

