My dotfiles
============
Clone and run this to configure your environment as follows:
```sh
# TODO merge with dotfiles-mooc
mv .bash_profile .bash_profile~
ln -sb dotfiles/.bash_profile .
mv .bashrc .bashrc~
ln -sb dotfiles/.bashrc .
mv .emacs.d .emacs.d~
ln -sb dotfiles/.emacs.d .
# TODO use init script to install emacs packages
mv .vim .vim~
ln -sb dotfiles/.vim .
# TODO use git submodules for vim plugins
mv .vimrc .vimrc~
ln -sb dotfiles/.vimrc .
mv .minttyrc .minttyrc~
ln -sb dotfiles/.minttyrc .
mv .tmux.conf .tmux.conf~
ln -sb dotfiles/.tmux.conf .
mv .tridactylrc .tridactylrc~
ln -sb dotfiles/.tridactylrc .
```
