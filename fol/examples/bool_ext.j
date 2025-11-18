
data Bool = True | False

prop eq(Bool, Bool)

axiom eq_refl : ∀ b, eq(b, b)

theorem Bool_ext : ∀ b : Bool, eq(b, True) ∨ eq(b, False)
  induction Bool
  case False
    conclude by eq_refl
  case True
    conclude by eq_refl
  end
end

