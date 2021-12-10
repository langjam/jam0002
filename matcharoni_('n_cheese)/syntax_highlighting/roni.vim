" Vim syntax file
" Language: Matcharoni

" Usage Instructions
" Put this file in .vim/syntax/roni.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.roni set filetype=roni

if exists("b:current_syntax")
  finish
endif

syntax keyword roniTodos TODO XXX FIXME NOTE

" Language keywords
syntax keyword roniKeywords expr pat bind
syntax keyword roniLoopKeywords while if else break continue loop for

" Comments
syntax region roniCommentLine start="//" end="$"   contains=roniTodos
" syntax region roniDirective start="%" end=" "

" TODO: this doesn't work exactly right
syntax match roniSpecial		"[#~*%@]\ze[a-zA-Z0-9_]*\|<@"
syntax match roniIdent		"\<[a-z_][a-z0-9_]*\>"
syntax match roniType		"\<[A-Z_][a-z0-9_]*\>"

" Numbers
syntax match roniDecInt display "\<[0-9][0-9_]*"
"syntax match roniHexInt display "\<0[xX][0-9a-fA-F][0-9_a-fA-F]*"
"syntax match roniFloat  display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)"

" Functions
syntax match roniFunction display "\<[a-z_][a-z0-9_]*\>("he=e-1

" TODO: we don't have string literalls... yet? or at all?
" Strings
" syntax region roniString start=/\v"/ skip=/\v\\./ end=/\v"/
" syntax region roniString start=/\v'/ skip=/\v\\./ end=/\v'/

" Set highlights
highlight default link roniType Type
highlight default link roniTodos Todo
highlight default link roniKeywords Keyword
" comments are highlighted as strings in this lang
" TODO: only some strings treated as values highlighted this way
" highlight default link roniCommentLine Comment
highlight default link roniDirective PreProc
highlight default link roniLoopKeywords Repeat
highlight default link roniDecInt Number
"highlight default link roniHexInt Number
"highlight default link roniFloat Float
highlight default link roniCommentLine Comment
highlight default link roniIdent Identifier
highlight default link roniSpecial Special
highlight default link roniFunction Function

let b:current_syntax = "roni"
