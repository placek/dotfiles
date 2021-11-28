if exists('b:current_syntax')
  finish
endif

syntax match  aplComment     /⍝.*/
syntax match  aplNumber      /\v\c¯?(\d*\.?\d+(e[+¯]?\d+)?|¯|∞)(j¯?(\d*\.?\d+(e[+¯]?\d+)?|¯|∞))?/
syntax match  aplQuote       /''/ contained
syntax region aplString      start=/"/ end=/"/
syntax region aplString      matchgroup=aplString start=/'/rs=s+1 skip=/''/ end=/'/re=e-1 contains=aplQuote oneline
syntax match  aplVerb        /[+\-×÷⌈⌊∣|⍳⍸?*⍟○!⌹<≤=>≥≠≡≢∊⍷∪∩~∨∧⍱⍲⍴,⍪⌽⊖⍉↑↓⊂⊃⊆⊇⌷⍋⍒⊤⊥⍕⍎⊣⊢⍁⍂≈⍯↗¤→]/
syntax match  aplAdverb      /[\\\/⌿⍀¨⍨⌶&∥⌸]/
syntax match  aplSeparator   /⋄/
syntax match  aplConjunction /[.@∘⍠⍣⍤⍥⌺]/
syntax match  aplSpecial     /[⍬⌾#⎕⍞]/
syntax match  aplID          /[A-Z_a-zÀ-ÖØ-Ýß-öø-üþ∆⍙Ⓐ-Ⓩ][A-Z_a-zÀ-ÖØ-Ýß-öø-üþ∆⍙Ⓐ-Ⓩ0-9]*/

highlight link aplComment     Comment
highlight link aplSeparator   Comment
highlight link aplNumber      Number
highlight link aplString      String
highlight link aplVerb        Keyword
highlight link aplAdverb      PreProc
highlight link aplConjunction Operator
highlight link aplSpecial     Special
highlight link aplId          Identifier

let b:current_syntax='apl'
