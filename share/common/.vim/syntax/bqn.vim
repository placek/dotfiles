scripte utf-8

if exists('b:current_syntax')
  finish
endif

syn match bqnerr "[^ \t\r\n]"
syn match bqnblk "[{}]"
syn match bqnlst "[⟨⟩\[\]‿]"
syn match bqnpar "[()]"
syn match bqnhed "[:;?]"
syn match bqnsep "[⋄,]"
syn match bqnarw "[←⇐↩→]"
syn match bqnchr "'.'"
syn match bqn1md "[˙˜˘¨⌜⁼´˝`]"
syn match bqn2md "[∘○⊸⟜⌾⊘◶⎉⚇⍟⎊]"
syn match bqnfun "[𝔽𝔾𝕎𝕏𝕊+\-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!⍕⍎]"
syn match bqnsub "[𝕗𝕘𝕨𝕩𝕤]"
syn match bqnnot "·"
syn match bqnnul "@"
syn match bqnnum "\v\c%(¯_*)?%((\d[\d_]*(\.\d[\d_]*)?|π_*)%(e_*(¯_*)?\d[\d_]*)?|∞_*)(i_*%(¯_*)?%((\d[\d_]*(\.\d[\d_]*)?|π_*)%(e_*(¯_*)?\d[\d_]*)?|∞_*))?"
syn match bqnsid "\(•\|•\?[a-z][A-Z_a-z0-9π∞¯]*\|𝕣\)"
syn match bqnfid "•\?[A-Z][A-Z_a-z0-9π∞¯]*"
syn match bqn1id "\(•\?_[A-Za-z][A-Z_a-z0-9π∞¯]*\|_𝕣\)"
syn match bqn2id "\(•\?_[A-Za-z][A-Z_a-z0-9π∞¯]*_\|_𝕣_\)"
syn match bqndot "\."
syn match bqncom "#.*$"
syn match bqnquo /""/ contained
syn region bqnstr matchgroup=bqnstr start=/"/ end=/"/ contains=bqnquo
syn sync fromstart

hi link bqnerr error
hi link bqncom comment
hi link bqnblk special
hi link bqnhed delimiter
hi link bqnpar delimiter
hi link bqnlst preproc
hi link bqnsep preproc
hi link bqnarw normal
hi link bqnchr string
hi link bqnnul string
hi link bqnstr string
hi link bqnquo specialchar
hi link bqnnum number
hi link bqnnot constant
hi link bqndot normal
hi link bqnsub normal
hi link bqnsid normal
hi link bqnfun type
hi link bqnfid type
hi link bqn1md macro
hi link bqn1id macro
hi link bqn2md operator
hi link bqn2id operator

let b:current_syntax='bqn'

let b:keymap_name=expand('<sfile>:t:r')

" Configurable prefix key; backslash by default
let p=exists('g:bqn_prefix_key')?g:bqn_prefix_key:'\'

let a ='`1234567890-= ~!@#$%^&*()_+'
let a.='qwertyuiop[]  QWERTYUIOP{} '
let a.='asdfghjkl;''\ ASDFGHJKL:"| '
let a.='zxcvbnm,./    ZXCVBNM<>?   '

let b ='˜˘¨⁼⌜´˝7∞¯•÷× ¬⎉⚇⍟◶⊘⎊⍎⍕⟨⟩√⋆'
let b.='⌽𝕨∊↑∧y⊔⊏⊐π←→  ↙𝕎⍷𝕣⍋YU⊑⊒⍳⊣⊢ '
let b.='⍉𝕤↕𝕗𝕘⊸∘○⟜⋄↩\  ↖𝕊D𝔽𝔾«J⌾»·˙| '
let b.='⥊𝕩↓∨⌊n≡∾≍≠    ⋈𝕏C⍒⌈N≢≤≥⇐   '

let[a,b]=map([a,b],{i,x->split(x,'\zs *')})
let a+=['<space>']|let b+=['‿']
for l in ['l','c']
 for i in range(len(a))
  exe escape(l.'no<buffer>'.p.a[i].' '.b[i],'|')
 endfor
endfor
unl p a b l i

setlocal keymap=bqn

setlocal commentstring=#%s
setlocal matchpairs=(:),{:},[:],⟨:⟩
setlocal iskeyword=@,48-57,_,^×,^÷
setlocal ignorecase

setlocal shiftwidth=2 tabstop=2 softtabstop=2
