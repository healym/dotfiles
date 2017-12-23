" mu's vim colorscheme"
"  Maintainer: Matthew Healy <mhealy@mst.edu>
" Last Change: 17 April 2017
"     version: 1.0
" This color scheme uses a dark background.

set background=dark

let colors_name = "mu"

" Color chart:
" 00  :  000000
" 01  :  303030
" 02  :  505050
" 03  :  808080
" 04  :  d0d0d0
" 05  :  e0e0e0
" 06  :  f5f5f5
" 07  :  ffffff
" 08  :  fb0120
" 09  :  fc6d24
" 0A  :  fda331
" 0B  :  34a853
" 0C  :  76c7b7
" 0D  :  6fb3d2
" 0E  :  d381c2
" 0F  :  be643c


hi Normal       guifg=#f0f0f8 guibg=NONE

" Search
hi IncSearch    gui=UNDERLINE guifg=#303030 guibg=#505050
hi Search       gui=NONE guifg=#808080 guibg=#505050

" Messages
hi ErrorMsg     gui=BOLD guifg=#fb0120 guibg=NONE
hi WarningMsg   gui=BOLD guifg=#fb0120 guibg=NONE
hi ModeMsg      gui=BOLD guifg=#34a853 guibg=NONE
hi MoreMsg      gui=BOLD guifg=#34a853 guibg=#008070
hi Question     gui=BOLD guifg=#e8e800 guibg=NONE

" Split area
hi StatusLine   gui=NONE guifg=#000000 guibg=#c8c8d8
hi StatusLineNC gui=NONE guifg=#707080 guibg=#c8c8d8
hi VertSplit    gui=NONE guifg=#606080 guibg=#c8c8d8
hi WildMenu     gui=NONE guifg=#fb0120 guibg=#fda331

" Diff
hi DiffText     gui=NONE guifg=#6fb3d2 guibg=NONE
hi DiffLine     gui=NONE guifg=#6fb3d2 guibg=NONE
hi DiffChange   gui=NONE guifg=#e03870 guibg=NONE
hi DiffDelete   gui=NONE guifg=#a0d0ff guibg=NONE
hi DiffAdd      gui=NONE guifg=#34a853 guibg=NONE
hi DiffAdded    gui=NONE guifg=#34a853 guibg=NONE
hi DiffNewFile  gui=NONE guifg=#34a853 guibg=NONE

" Cursor
hi Cursor       gui=NONE guifg=#000000  guibg=#e0e0e0
hi lCursor      gui=NONE guifg=#ffffff guibg=#303030
hi CursorIM     gui=NONE guifg=#ffffff guibg=#303030

" Fold
hi Folded       gui=NONE guifg=#40f0f0 guibg=#303030
hi FoldColumn   gui=NONE guifg=#76c7b7 guibg=#303030

" Other
hi Conceal      gui=NONE guifg=#6fb3d2 guibg=NONE
hi Directory    gui=NONE guifg=#6fb3d2 guibg=NONE
hi LineNr       gui=NONE guifg=#808080 guibg=NONE
hi NonText      gui=BOLD guifg=#808080 guibg=NONE
hi SpecialKey   gui=BOLD guifg=#8080ff guibg=NONE
hi Title        gui=BOLD guifg=#6fb3d2 guibg=NONE
hi Visual       gui=NONE guifg=#e0e0f0 guibg=#707080
hi MatchParen   gui=NONE guifg=#000000 guibg=#808080
hi Question     gui=NONE guifg=#6fb3d2 guibg=NONE

" Table
hi TabLine      gui=NONE guifg=#40f0f0 guibg=#303030
hi TabLineFill  gui=NONE guifg=#40f0f0 guibg=#303030
hi TabLineSel   gui=NONE guifg=#a1c650 guibg=#303030

" Syntax group
hi Boolean      gui=NONE guifg=#fc6d24 guibg=NONE
hi Character    gui=NONE guifg=#fb0120 guibg=NONE
hi Comment      gui=NONE guifg=#808080 guibg=NONE
hi Constant     gui=NONE guifg=#fc6d24 guibg=NONE
hi Conditional  gui=NONE guifg=#d381c3 guibg=NONE
hi Define       gui=NONE guifg=#d381c3 guibg=NONE
hi Delimiter    gui=NONE guifg=#b3643c guibg=NONE
hi Error        gui=BOLD guifg=#ffffff guibg=#fb0120
hi Float        gui=NONE guifg=#fc6d24 guibg=NONE
hi Function     gui=NONE guifg=#6fb3d2 guibg=NONE
hi Identifier   gui=NONE guifg=#fb0120 guibg=NONE
hi Ignore       gui=NONE guifg=#808080 guibg=NONE
hi Include      gui=NONE guifg=#6fb3d2 guibg=NONE
hi Keyword      gui=NONE guifg=#d381c3 guibg=NONE
hi Label        gui=NONE guifg=#fda331 guibg=NONE
hi Number       gui=NONE guifg=#fc6d24 guibg=NONE
hi Operator     gui=NONE guifg=#e0e0e0 guibg=NONE
hi PreProc      gui=NONE guifg=#fda331 guibg=NONE
hi Repeat       gui=NONE guifg=#fda331 guibg=NONE
hi Special      gui=NONE guifg=#76c7b7 guibg=NONE
hi SpecialChar  gui=NONE guifg=#be643c guibg=NONE
hi Statement    gui=NONE guifg=#fb0120 guibg=NONE
hi StorageClass gui=NONE guifg=#fda331 guibg=NONE
hi String       gui=NONE guifg=#34a853 guibg=NONE
hi Structure    gui=NONE guifg=#d381c3 guibg=NONE
hi Tag          gui=NONE guifg=#fda331 guibg=NONE
hi Todo         gui=BOLD,UNDERLINE guifg=#fda331 guibg=NONE
hi Type         gui=NONE guifg=#fda331 guibg=NONE
hi Typedef      gui=none guifg=#fda331 guibg=NONE
hi Underlined   gui=UNDERLINE guifg=#fb0120 guibg=NONE

" HTML
hi htmlLink                 gui=UNDERLINE
hi htmlBold                 gui=BOLD
hi htmlBoldItalic           gui=BOLD,ITALIC
hi htmlBoldUnderline        gui=BOLD,UNDERLINE
hi htmlBoldUnderlineItalic  gui=BOLD,UNDERLINE,ITALIC
hi htmlItalic               gui=ITALIC
hi htmlUnderline            gui=UNDERLINE
hi htmlUnderlineItalic      gui=UNDERLINE,ITALIC
