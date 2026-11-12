" setting up cursor, work in Konsole and Windows Terminal
let &t_SI.="\e[5 q"
let &t_EI.="\e[1 q"
let &t_te.="\e[0 q"

" size of a hard tabstop, default 8
set tabstop=4

" size of an indent, default 8
set shiftwidth=4

" min lines before/after cursor, default 0 or 1 with vim-sensible
set scrolloff=8

" a combination of spaces and tabs are used to simulate tab stops at a width
" other than the (hard)tabstop, default 0
set softtabstop=4

" replace tabs with spaces
set expandtab

" smartcase search
set ignorecase
set smartcase

" highlight search; vim-sensible - enable incsearch and map <C-L> to :nohl
set hlsearch

" no beep
set visualbell

" mouse in term
set mouse=a

" allow to switch buffers with unsaved changes
set hidden

" command completion: common prefix first, then menu
set wildmode=longest:full,full

" change leader to space
let mapleader = " "

" set runtimepath on windows
if has('win32') || has('win64')
    set runtimepath-=~/vimfiles
    set runtimepath^=~/.vim
    set runtimepath-=~/vimfiles/after
    set runtimepath+=~/.vim/after
endif

" scrolling other window
"this function maps Alt-down and Alt-Up to move other window
"" put in your ~/.vimrc
fun! ScrollOtherWindow(dir)
    if a:dir == "down"
        let move = "\<C-E>"
    elseif a:dir == "up"
        let move = "\<C-Y>"
    elseif a:dir == "left"
        let move = "zh"
    elseif a:dir == "right"
        let move = "zl"
    elseif a:dir == "leftHalf"
        let move = "zH"
    elseif a:dir == "rightHalf"
        let move = "zL"
    endif
    exec "normal \<C-W>p" . move . "\<C-W>p"
endfun
nmap <silent> <M-Down> :call ScrollOtherWindow("down")<CR>
nmap <silent> <M-Up> :call ScrollOtherWindow("up")<CR>
nmap <silent> <M-Left> :call ScrollOtherWindow("left")<CR>
nmap <silent> <M-Right> :call ScrollOtherWindow("right")<CR>
nmap <silent> <C-M-Left> :call ScrollOtherWindow("leftHalf")<CR>
nmap <silent> <C-M-Right> :call ScrollOtherWindow("rightHalf")<CR>

" bootstrap vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" vim-plug
call plug#begin()

Plug 'tinted-theming/base16-vim'
Plug 'preservim/nerdtree'
Plug 'dense-analysis/ale'
Plug 'tpope/vim-abolish'
Plug 'vim-airline/vim-airline'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 's3rvac/AutoFenc'
Plug 'inkarkat/vim-ReplaceWithRegister'
Plug 'vim-scripts/argtextobj.vim'

" local .vimrc file
if filereadable(glob("~/.vimrc.local")) 
    source ~/.vimrc.local
endif

call plug#end()
" need to run :PlugInstall

" gc for comments
packadd! comment

" 24-bit colors in term
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" theme; works in term with termguicolors
silent! colorscheme base16-default-dark

" font
if has('gui_running')
    set guioptions-=T  " no toolbar
    set lines=45 columns=108 linespace=0
    if has('gui_win32')
        set guifont=Consolas:h12:cRUSSIAN
    else
        set guifont=DejaVu\ Sans\ Mono\ 12
    endif
endif

" easymotion
let g:EasyMotion_smartcase = 1
nmap <C-;> <Plug>(easymotion-s)
nmap <leader>s <Plug>(easymotion-s)

" airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#ale#enabled = 1

" ale
" check on save instead of each keypress, default always
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0

" nerdtree mapping
map <C-n> :NERDTreeToggle<CR>
