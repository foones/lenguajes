
prop a
prop b
prop c

theorem xxx : c → a ∨ b → (a ∨ b) ∧ (b ∨ a) ∧ c
  suppose 0: c
  suppose 1: a ∨ b
  claim 2 : (a ∨ b) ∧ (b ∨ a)
    cases by 1
    case a
      hence a ∨ b
      hence b ∨ a
    case b
      hence a ∨ b
      hence b ∨ a
    end
  end
  hence c by 0
  conclude by 2
end
