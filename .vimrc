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

" pathogen
execute pathogen#infect()

" dbext
" mssql
let g:dbext_default_profile_mssql_fcs_dump = 'type=SQLSRV:user=sa:passwd=saPass1:host=localhost:dbname=taxi_fcs_dump'
" postgres
let g:dbext_default_profile_postgres_fcs_dump = 'type=PGSQL:user=postgres:passwd=postgres:host=localhost:dbname=taxi_fcs_dump'
