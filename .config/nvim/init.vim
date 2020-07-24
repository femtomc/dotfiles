" Sensible defaults.
syntax enable
filetype plugin on
set ts=4 sw=4
set expandtab
set number relativenumber
set showcmd
set cursorline
set wildmenu
set showmatch
set foldenable
set foldmethod=indent
set tags=tags;/


" Plugs.
call plug#begin('~/.vim/plugged')

" Themes.
Plug 'morhetz/gruvbox'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'arcticicestudio/nord-vim'

" Git.
Plug 'tpope/vim-fugitive'
Plug 'will133/vim-dirdiff'

" Langs.
Plug 'lervag/vimtex'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'ollykel/v-vim'
Plug 'idris-hackers/idris-vim'

Plug 'racer-rust/vim-racer'

set hidden

Plug 'neovimhaskell/haskell-vim'
autocmd BufWrite *.hs :Autoformat
autocmd FileType haskell let b:autoformat_autoindent=0

Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim'

Plug 'unisonweb/unison', { 'rtp': 'editor-support/vim' }

" Tooling.
Plug 'Chiel92/vim-autoformat'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
call plug#end()

" Set theme.
" The "^[" is a single character. You enter it by pressing Ctrl+v and then ESC.
set t_8f=^[[38;2;%lu;%lu;%lum
set t_8b=^[[48;2;%lu;%lu;%lum
colorscheme gruvbox
set termguicolors
set background=dark
hi Normal guibg=NONE ctermbg=NONE

let g:mkdp_auto_start = 0
let g:mkdp_auto_close = 1
let g:mkdp_refresh_slow = 0
let g:mkdp_command_for_global = 0
let g:mkdp_open_to_the_world = 0
let g:mkdp_open_ip = ''
let g:mkdp_browser = ''
let g:mkdp_echo_preview_url = 0
let g:mkdp_browserfunc = ''
let g:mkdp_preview_options = {
    \ 'mkit': {},
    \ 'katex': {},
    \ 'uml': {},
    \ 'maid': {},
    \ 'disable_sync_scroll': 0,
    \ 'sync_scroll_type': 'middle',
    \ 'hide_yaml_meta': 1,
    \ 'sequence_diagrams': {},
    \ 'flowchart_diagrams': {}
    \ }
let g:mkdp_markdown_css = ''
let g:mkdp_highlight_css = ''
let g:mkdp_port = ''
let g:mkdp_page_title = '「${name}」'

" Merlin.
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
set rtp+=~/ocp-indent-vim

" Racer.
let g:racer_cmd = "/home/mccoy/.cargo/bin/racer"
let g:racer_experimental_completer = 1
augroup Racer
    autocmd!
    autocmd FileType rust nmap <buffer> gd         <Plug>(rust-def)
    autocmd FileType rust nmap <buffer> gs         <Plug>(rust-def-split)
    autocmd FileType rust nmap <buffer> gx         <Plug>(rust-def-vertical)
    autocmd FileType rust nmap <buffer> gt         <Plug>(rust-def-tab)
    autocmd FileType rust nmap <buffer> <leader>gd <Plug>(rust-doc)
augroup END

" Italics.
highlight Comment cterm=italic gui=italic
