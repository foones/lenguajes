
data Nat = 0 | s(Nat) | +(Nat,Nat) | *(Nat,Nat)

prop eq(Nat,Nat)

axiom zns     : ¬∃N, eq(0,s(N))
axiom injS    : eq(s(N),s(M)) → eq(N,M)
axiom addZ    : eq(+(0,N),N)
axiom addS    : eq(+(s(M),N),s(+(M,N)))
axiom mulZ    : eq(*(0,N),0)
axiom mulS    : eq(*(s(M),N),+(N,*(M,N)))
axiom eqRefl  : eq(N,N)
axiom eqSym   : eq(N,M) → eq(M,N)
axiom eqTrans : eq(N,M) → eq(M,P) → eq(N,P)
axiom eqCongS : eq(N,M) → eq(s(N),s(M))
axiom eqCong+ : (eq(N,M) → eq(+(N,P),+(M,P)))
              ∧ (eq(N,M) → eq(+(P,N),+(P,M)))
axiom eqCong* : (eq(N,M) → eq(*(N,P),*(M,P)))
              ∧ (eq(N,M) → eq(*(P,N),*(P,M)))

axiom induction<addAssoc> :
      ∀ N M, eq(+(+(0,N),M),+(0,+(N,M)))
      → ∀ P N M,
          (eq(+(+(P,N),M),+(P,+(N,M)))
          → eq(+(+(s(P),N),M),+(s(P),+(N,M))))
      → ∀ P N M, eq(+(+(P,N),M),+(P,+(N,M)))

axiom induction<addZR> :
      eq(+(0,0),0)
      → ∀ N, (eq(+(N,0),N) → eq(+(s(N),0),s(N)))
      → ∀ N, eq(+(N,0),N)

axiom induction<addSR> :
      ∀ N, eq(+(0,s(N)),s(+(0,N)))
      → ∀ N M,
        (eq(+(N,s(M)),s(+(N,M)))
         → eq(+(s(N),s(M)),s(+(s(N),M))))
      → ∀ N M, eq(+(N,s(M)),s(+(N,M)))

axiom induction<addComm> :
      ∀ N, eq(+(0,N),+(N,0))
      → ∀ N M,
        (eq(+(N,M),+(M,N))
         → eq(+(s(N),M),+(M,s(N))))
      → ∀ N M, eq(+(N,M),+(M,N))

theorem addAssoc : eq(+(+(X,Y),Z),+(X,+(Y,Z)))
  claim Base: ∀ Y Z, eq(+(+(0,Y),Z),+(0,+(Y,Z)))
    let y z : Nat
    conclude by addZ,eqTrans,eqSym,eqCong+
  end
  claim Step: ∀ X Y Z,
              (eq(+(+(X,Y),Z),+(X,+(Y,Z)))
               → eq(+(+(s(X),Y),Z),+(s(X),+(Y,Z))))
    let x y z
    suppose IH: eq(+(+(x,y),z),+(x,+(y,z)))
    have  eq(+(+(s(x),y),z),s(+(+(x,y),z))) by eqCong+,addS,eqTrans
    then  eq(+(+(s(x),y),z),s(+(x,+(y,z)))) by eqTrans,IH,eqCongS
    hence eq(+(+(s(x),y),z),+(s(x),+(y,z))) by eqTrans,eqSym,addS
  end
  conclude by induction<addAssoc>,Base,Step 
end

theorem addZR : eq(+(X,0),X)
  have Base: eq(+(0,0),0) by addZ
  claim Step: ∀ X,
              (eq(+(X,0),X)
              → eq(+(s(X),0),s(X)))
    let x
    suppose IH: eq(+(x,0),x)
    have eq(+(s(x),0),s(+(x,0))) by addS
    hence eq(+(s(x),0),s(x))     by eqTrans,eqCongS,IH
  end
  conclude by induction<addZR>,Base,Step
end

theorem addSR : eq(+(X,s(Y)),s(+(X,Y)))
  claim Base: ∀ X, eq(+(0,s(X)),s(+(0,X)))
    let x
    thus eq(+(0,s(x)),s(+(0,x))) by addZ,eqTrans,eqCongS,addZ
  end
  claim Step: ∀ X Y,
              (eq(+(X,s(Y)),s(+(X,Y)))
               → eq(+(s(X),s(Y)),s(+(s(X),Y))))
    let x y
    suppose IH: eq(+(x,s(y)),s(+(x,y)))
    have eq(+(s(x),s(y)),s(+(x,s(y))))    by addS
    then 1: eq(+(s(x),s(y)),s(s(+(x,y)))) by eqTrans,eqCongS,IH
    have eq(s(s(+(x,y))),s(+(s(x),y)))    by eqSym,eqCongS,addS
    hence eq(+(s(x),s(y)),s(+(s(x),y)))   by 1,eqTrans
  end
  conclude by induction<addSR>,Base,Step
end

theorem addComm : eq(+(X,Y),+(Y,X))
  claim Base: ∀ X, eq(+(0,X),+(X,0))
    let x : Nat
    conclude by addZ,addZR,eqTrans
  end
  claim Step: ∀ X Y,
              (eq(+(X,Y),+(Y,X))
              → eq(+(s(X),Y),+(Y,s(X))))
    let x y
    suppose IH: eq(+(x,y),+(y,x))
    have  eq(+(s(x),y),s(+(x,y))) by addS
    then  eq(+(s(x),y),s(+(y,x))) by eqTrans,eqCongS,IH
    hence eq(+(s(x),y),+(y,s(x))) by eqTrans,eqSym,addSR
  end
  conclude by induction<addComm>,Base,Step
end

