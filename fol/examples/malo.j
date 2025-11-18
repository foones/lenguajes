
data Nat = zero | succ(Nat)

prop lt(Nat,Nat)

axiom lt-succ  : ∀ X, lt(X,succ(X))
axiom lt-trans : ∀ X Y Z, (lt(X,Y) → lt(Y,Z) → lt(X,Z))

theorem lt-0_1 : lt(zero, succ(zero))
  thus _ by lt-succ
end

theorem lt-0_2 : lt(zero, succ(succ(zero)))
  thus _ by lt-succ,lt-trans
end

theorem lt-0_3 : lt(zero, succ(succ(succ(zero))))
  thus _ by lt-trans,lt-trans,lt-succ
end

theorem lt-0_4 : lt(zero, succ(succ(succ(succ(zero)))))
  thus _ by lt-trans,lt-trans,lt-trans,lt-succ
end

theorem lt-0_5_v1 : lt(zero, succ(succ(succ(succ(succ(zero))))))
  thus _ by lt-trans,lt-trans,lt-trans,lt-trans,lt-succ
end

theorem lt-0_5 : lt(zero, succ(succ(succ(succ(succ(zero))))))
  thus _ by lt-0_4,lt-trans,lt-succ
end

theorem lt-0_6 : lt(zero, succ(succ(succ(succ(succ(succ(zero)))))))
  thus _ by lt-0_5,lt-trans,lt-succ
end

