My dotfiles
============
Clone and run this to configure your environment as follows:
```sh
mv .bash_profile .bash_profile~
ln -sb dotfiles/.bash_profile .
mv .bashrc .bashrc~
ln -sb dotfiles/.bashrc .
mv .inputrc .inputrc~
ln -sb dotfiles/.inputrc .
mv .emacs.d .emacs.d~
ln -sb dotfiles/.emacs.d .
mv .vim .vim~
ln -sb dotfiles/.vim .
mv .vimrc .vimrc~
ln -sb dotfiles/.vimrc .
mv .tmux.conf .tmux.conf~
ln -sb dotfiles/.tmux.conf .
mv .tridactylrc .tridactylrc~
ln -sb dotfiles/.tridactylrc .
mv .ideavimrc .ideavimrc~
ln -sb dotfiles/.ideavimrc .
```
