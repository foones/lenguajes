
data Enfermedad = Resfrio
                | Hipocondria

prop CreoQuePadezco(Enfermedad)
prop Padezco(Enfermedad)

axiom def_Hipocondria : (∃ e, (CreoQuePadezco(e) ∧ ¬Padezco(e))) → Padezco(Hipocondria)

theorem chph : CreoQuePadezco(Hipocondria) → Padezco(Hipocondria)
  suppose CH : CreoQuePadezco(Hipocondria)
  cases
  case PH: Padezco(Hipocondria)
    conclude by PH
  case NPH: ¬Padezco(Hipocondria)
    then (CreoQuePadezco(Hipocondria) ∧ ¬Padezco(Hipocondria)) by CH
    hence Padezco(Hipocondria) by def_Hipocondria,CH
  end
end

