" Vim syntax file for likelang
" Language: Like
" Maintainer: Samyak S Sarnayak
" Latest Revision: 07 December 2021

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword likeKeywords fn nextgroup=likeFunName print let nextgroup=likeVar collect nextgroup=likePrefixPattern,likePostfixPattern

syntax match likeOperator "\v\*"
syntax match likeOperator "\v/"
syntax match likeOperator "\v\+"
syntax match likeOperator "\v-"

" Matches
" syn match syntaxElementMatch 'regexp' contains=syntaxElement1 nextgroup=syntaxElement2 skipwhite
" syn match likeAssignment "="
" syn match likePatternInside "\*\=.*\*\="
syn match likePrefixPattern "\/\*.*\/"
syn match likePostfixPattern "\/.*\*\/"
" syn region likePattern start='/' end='/' skip='\n' contains=likePatternInside oneline

syn match likeDelim '\.'
" syn match likeSpecial '+\|-\|*\|/'
syn match likeFunName "\w\+"
syn match likeVar "\w\+"
" Regular int like number with - + or nothing in front
syn match likeNumber '\d\+' contained display
syn match likeNumber '[-+]\d\+' contained display

" Floating point number with decimal no E or e (+,-)
syn match likeNumber '\d\+\.\d*' contained display
syn match likeNumber '[-+]\d\+\.\d*' contained display

" Floating point like number with E and no decimal point (+,-)
syn match likeNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+' contained display
syn match likeNumber '\d[[:digit:]]*[eE][\-+]\=\d\+' contained display

" Floating point like number with E and decimal point (+,-)
syn match likeNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display
syn match likeNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display

syn match likeComment "//.*$"

" Regions
syn region likeString start='"' end='"' oneline
syn region likeArgs matchgroup=likeParen start='(' end=')' contains=likeVar,likeString,likeNumber transparent oneline
syn region likeBlock matchgroup=likeParen start="{" end="}" fold contains=ALL transparent

let b:current_syntax = "like"

hi def link likeComment Comment
hi def link likeString Constant
hi def link likeNumber Constant
hi def link likeVar Identifier
hi def link likeFunName Function
" hi def link likeAssignment Statement
hi def link likeDelim Delimiter
" hi def link likeSpecial Special
hi def link likePrefixPattern Constant
hi def link likePostfixPattern Constant
hi def link likeKeywords Keyword
hi def link likeOperator Operator
hi def link likeParen Special
