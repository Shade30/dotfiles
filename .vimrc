" setting up cursor in mintty
let &t_ti.="\e[1 q"
let &t_SI.="\e[5 q"
let &t_EI.="\e[1 q"
let &t_te.="\e[0 q"

let &t_ti.="\e[?7727h"
let &t_te.="\e[?7727l"
noremap <Esc>O[ <Esc>
noremap! <Esc>O[ <C-c>

" size of a hard tabstop
set tabstop=4

" size of an indent
set shiftwidth=4

" a combination of spaces and tabs are used to simulate tab stops at a width
" other than the (hard)tabstop
set softtabstop=4

" replace tabs with spaces
set expandtab

" smartcase search
set ignorecase
set smartcase

" no beep
set visualbell

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

Plug 'chriskempson/base16-vim'
Plug 'scrooloose/nerdtree'
Plug 'vim-syntastic/syntastic'
Plug 'tpope/vim-abolish'
Plug 'vim-airline/vim-airline'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-sensible'
Plug 's3rvac/AutoFenc'
Plug 'inkarkat/vim-ReplaceWithRegister'
Plug 'vim-scripts/argtextobj.vim'

call plug#end()
" need to run :PlugInstall

" font
if has('gui_running')
	set guioptions-=T  " no toolbar
	silent! colorscheme base16-default-dark
	set lines=60 columns=108 linespace=0
	if has('gui_win32')
		set guifont=Consolas:h12:cRUSSIAN
	else
		set guifont=DejaVu\ Sans\ Mono\ 12
	endif
endif

" easymotion
let g:EasyMotion_smartcase = 1
nmap <C-;> <Plug>(easymotion-s)

" airline
let g:airline_powerline_fonts = 1

" nerdtree mapping
map <C-n> :NERDTreeToggle<CR>

" local .vimrc file
if filereadable(glob("~/.vimrc.local")) 
	source ~/.vimrc.local
endif
