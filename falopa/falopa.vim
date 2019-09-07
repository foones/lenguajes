
au BufEnter *.fa
        \ set filetype=haskell|
        \ set et|
        \ set ts=4|
        \ imap \bC ℂ|
        \ imap \bH ℍ|
        \ imap \bN ℕ|
        \ imap \bP ℙ|
        \ imap \bQ ℚ|
        \ imap \bR ℝ|
        \ imap \bZ ℤ|
        \ imap \:: ∷|
        \ imap \x ×|
        \ imap \o ∘|
        \ imap \lambda λ|
        \ imap \\ λ|
        \ imap \bot ⟂|
        \ imap \top ⊤|
        \ imap \equiv ≡|
        \ imap \GG Γ|
        \ imap \\|- ⊢|
        \ imap \to →|
        \ imap \in ∈|
        \ nmap <f5> :w<Return>:!python main.py %<Return>|
        \ imap <f5> <Esc><F5>

