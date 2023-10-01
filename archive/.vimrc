set number
syntax on
filetype plugin on
filetype indent on
set smartindent
nnoremap <C-@> :call system("wl-copy", @")<CR>
" auto indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" powerline
set laststatus=2

" clipboard
set clipboard=unnamedplus

" autoclose brackets
inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap <expr> ) strpart(getline('.'), col('.')-1, 1) == ")" ? "\<Right>" : ")"
inoremap [ []<left>
inoremap <expr> ] strpart(getline('.'), col('.')-1, 1) == "]" ? "\<Right>" : "]"
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O
